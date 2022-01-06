#' @title Retrieve LD matrix with LDLink
#' @description This function is a wrapper around [LDlinkR::LDmatrix()] that allows queries for greater than 1000 SNPs, and using an optional local cache file for faster look-ups. The LDlink API only allows queries of up to 1000s SNPs at a time. This function makes repeated calls to the API to build the appropriate LD matrix.
#'
#' @param snps 	List of variants, using an rsID or chromosome coordinate (e.g. "chr7:24966446")
#' @param pop A 1000 Genomes Project population, default = "EUR"
#' @param r2d Either "r2" for LD R2 or "d" for LD D', default = "r2"
#' @param cache Name of an optional cache file to read/store results from/to
#' @param max_query_size Max number of SNPs to pass to LDlinkR at a time
#' @param progress Display a progress indicator
#' @param compress Whether output rds should be compressed
#' @param tokens List of LDLink tokens. If multiple tokens are provided, look-ups will be performed in parallel
#' @param cache_tokens Whether to save tokens in the exported LD cache file.
#'
#' @import Matrix
#' @export
ld_matrix_LDLink <- function(snps,
                      pop = "EUR",
                      r2d = "r2",
                      tokens = NULL,
                      cache = NULL,
                      cache_tokens = TRUE,
                      max_query_size = 1000,
                      progress = TRUE,
                      compress = FALSE) {

  ## ---- Validate options ----
  avail_pop = c("YRI", "LWK", "GWD", "MSL", "ESN", "ASW",
                "ACB", "MXL", "PUR", "CLM", "PEL", "CHB", "JPT",
                "CHS", "CDX", "KHV", "CEU", "TSI", "FIN", "GBR",
                "IBS", "GIH", "PJL", "BEB", "STU", "ITU", "ALL",
                "AFR", "AMR", "EAS", "EUR", "SAS")
  avail_ld = c("r2", "d")
  pop <- match.arg(pop, avail_pop)
  r2d <- match.arg(r2d, avail_ld)
  snps <- unique(unlist(snps))
  tokens <- unique(unlist(tokens))
  num_tokens <- length(tokens)

  rsid_pattern <- "^rs\\d{1,}"
  chr_coord_pattern <- "(^chr)(\\d{1,2}|X|x|Y|y):(\\d{1,9})$"
  for (i in 1:length(snps)) {
    if (!((grepl(rsid_pattern, snps[i], ignore.case = TRUE)) |
          (grepl(chr_coord_pattern, snps[i], ignore.case = TRUE)))) {
      stop(paste("Invalid query format for variant: ",
                 snps[i], ".", sep = ""))
    }
  }

  ## ---- Load cache ----
  use_cache <- !is.null(cache)
  if (use_cache) {
    # read cache
    if (file.exists(cache)) {
      cache_data <- readRDS(cache)
      if (!setequal(names(cache_data), c("token", avail_pop))) {
        warning("Unrecognized format for cache at ", cache, "; will not use cache", call. = F, immediate. = T)
        use_cache <- FALSE
        cache_data <- NULL
      }

    # make empty cache if file doesn't exist
    } else {
      dir.create(dirname(cache), showWarnings = F, recursive = T)
      cache_data <- as.list(rep(NA, length(avail_pop) + 1))
      names(cache_data) <- c("token", avail_pop)
    }
  }
  cache_data_matrix <- if (use_cache && !identical(NA, cache_data[[pop]])) as.matrix(cache_data[[pop]])

  # load/store token
  if (use_cache && cache_tokens) {
    if (is.null(tokens)) {
      if (is.na(cache_data$tokens)) stop("Token not provided and not found in cache")
      else token <- cache_data$tokens
    }
  } else if (!is.null(tokens)) {
    if (!is.na(cache_data$tokens) && !identical(cache_data$tokens, tokens)) {
      warning("Overwriting cached tokens '", toString(cache_data$tokens), "' with '", toString(tokens), "'")
    }
    cache_data$tokens <- tokens
  } else {
    stop("Token must be provided if not using cache")
  }

  ## ---- Check for snps in cache ----
  cached_snps_all <- NULL
  if (use_cache) cached_snps_all <- dimnames(cache_data_matrix)[[1]]
  cached_snps_lookup <- intersect(snps, cached_snps_all)
  uncached_snps_lookup <- setdiff(snps, cached_snps_all)
  n_cached <- length(cached_snps_lookup) %||% 0
  n_uncached <- length(uncached_snps_lookup) %||% 0

  # extract subset matrix of cached snps
  cached_data_lookup <- if (n_cached > 0) cache_data_matrix[cached_snps_lookup, cached_snps_lookup, drop = F]

  ## ---- group cached matrix into contiguous blocks ----
  cached_data_lookup <- if (n_cached > 0) sort_blocks_diagonally(as.matrix(cached_data_lookup))

  ## ---- expand matrix to include search snps ----
  populate_data <- matrix(NA_real_, nrow = n_uncached, ncol = n_uncached, dimnames = list(uncached_snps_lookup, uncached_snps_lookup))
  populate_data <- combine_block_diagonal(cached_data_lookup, populate_data)
  populate_data_names <- dimnames(populate_data)[[1]]

  ## ---- Iteratively look for the largest unfilled blocks, and retrieve LD data for them until done ----
  while (T) {
    # find the largest NA block that has <= 1000 unique SNPs
    largest_block_list <- largest_na_block(as.matrix(populate_data), max_perim = max_query_size, num_results = num_tokens)
    if (num_tokens == 1 && length(largest_block_list) == 2) largest_block_list <- list(largest_block_list)
    if (all(sapply(largest_block_list, is.null))) break

    # print progress diagram
    if (progress) print_progress(populate_data, largest_block_list)

    # retrieve snps in parallel
    snp_lists <- lapply(largest_block_list, function(blk) {
      unique(populate_data_names[c(blk$cols, blk$rows)])
    })
    retrieved_LD_list <- retrieve_LDLink_parallel(
      snps_list = snp_lists,
      token_list = tokens,
      pop = pop,
      r2d = r2d
    )

    # loop over results for each set of returned data
    for (b in seq_along(retrieved_LD_list)) {
      retrieved_LD_data <- retrieved_LD_list[[b]]
      search_snps <- snp_lists[[b]]
      largest_block <- largest_block_list[[b]]

      retrieved_snps <- dimnames(retrieved_LD_data)[[1]]

      # change NA to -Inf so algorithm doesn't keep repeating over NA values
      retrieved_LD_data[is.na(retrieved_LD_data)] <- -Inf

      # see if any snps were left out, and remove them
      missing_snps <- setdiff(search_snps, retrieved_snps)
      if (length(missing_snps > 0)) {
        warning("The following SNPs were not returned from the LDLink lookup:\n  '", paste0(missing_snps, collapse = "', '"), "'", immediate. = F, call. = F)
        missing_idx <- match(missing_snps, populate_data_names)
        populate_data[missing_idx,] <- populate_data[,missing_idx] <- -Inf
        largest_block$rows <- setdiff(largest_block$rows, missing_idx)
        largest_block$cols <- setdiff(largest_block$cols, missing_idx)
      }

      retrieved_LD_data_cols <- match(populate_data_names[largest_block$cols], retrieved_snps)
      retrieved_LD_data_rows <- match(populate_data_names[largest_block$rows], retrieved_snps)

      populate_data[largest_block$rows, largest_block$cols] <- retrieved_LD_data[retrieved_LD_data_rows, retrieved_LD_data_cols]
      populate_data[largest_block$rows, largest_block$rows] <- retrieved_LD_data[retrieved_LD_data_rows, retrieved_LD_data_rows]
      populate_data[largest_block$cols, largest_block$cols] <- retrieved_LD_data[retrieved_LD_data_cols, retrieved_LD_data_cols]

      populate_data[largest_block$cols, largest_block$rows] <- t(populate_data[largest_block$rows, largest_block$cols])
    }

    populate_data <- sort_blocks_diagonally(populate_data)
    populate_data_names <- dimnames(populate_data)[[1]]

    ## ---- save to cache ----
    if (use_cache) {
      has_data <- which(apply(populate_data, 1, function(x) any(!is.na(x))))
      new_snps <- setdiff(populate_data_names[has_data], cached_snps_lookup) %||% populate_data_names[has_data]
      cache_data_matrix <- combine_block_diagonal(cache_data_matrix, populate_data[new_snps, new_snps, drop = F])
      new_cache_data_idx <- match(populate_data_names[has_data], dimnames(cache_data_matrix)[[1]])
      cache_data_matrix[new_cache_data_idx, new_cache_data_idx] <- populate_data[has_data, has_data]
      cache_data[[pop]] <- as(forceSymmetric(cache_data_matrix, uplo = "U"), "dspMatrix")
      saveRDS(cache_data, cache, compress = compress)
    }
  }

  populate_data[is.infinite(populate_data)] <- NA
  return(as.matrix(populate_data))
}

print_progress <- function(M, pending_blocks = NULL) {

  n <- nrow(M)
  DISPLAY_SZ <- min(15, n)

  # defines breaks
  entries_per_block <- floor(n/DISPLAY_SZ)
  remainder_entries <- n - entries_per_block * DISPLAY_SZ
  remainder_offset <- floor((DISPLAY_SZ - remainder_entries)/2)
  remainder_add <- c(rep(0, remainder_offset), rep(1, remainder_entries), rep(0, DISPLAY_SZ - remainder_offset - remainder_entries))

  breaks <- rep(entries_per_block, DISPLAY_SZ)
  breaks <- breaks + remainder_add
  breaks_start <- Reduce(`+`, breaks, accumulate = T) - breaks + 1
  breaks_end <- Reduce(`+`, breaks, accumulate = T)
  breaks_start <- matrix(rep(breaks_start, DISPLAY_SZ), DISPLAY_SZ, DISPLAY_SZ)
  breaks_end <- matrix(rep(breaks_end, DISPLAY_SZ), DISPLAY_SZ, DISPLAY_SZ)

  # determine which blocks are pended
  pended_chunks <- sapply(seq_along(breaks_start), function(x) {
    chunk_intersects_block <- sapply(pending_blocks, function(blk) {
      if (!is.null(blk)) {
        block_pended_1 <- any(breaks_start[x]:breaks_end[x] %in% blk$rows) && any(t(breaks_start)[x]:t(breaks_end)[x] %in% blk$cols)
        block_pended_2 <- any(breaks_start[x]:breaks_end[x] %in% blk$cols) && any(t(breaks_start)[x]:t(breaks_end)[x] %in% blk$rows)
        (block_pended_1 || block_pended_2)
      } else F
    })
    any(chunk_intersects_block)
  })

  prog_ind <- c(" \u25ef", " \u25d4", " \u25d1", " \u25d5", " \u2713")

  # create display matrix
  disp_matrix <- apply(matrix(1:DISPLAY_SZ^2, DISPLAY_SZ, DISPLAY_SZ), c(1,2), function(x) {
    if (pended_chunks[x]) return(" \u2193")
    m <- M[breaks_start[x]:breaks_end[x], t(breaks_start)[x]:t(breaks_end)[x]]
    fill <- floor(4 * sum(!is.na(m)) / length(m))
    prog_ind[fill + 1]
  })

  # print the display
  display_rows <- apply(disp_matrix, 2, function(x) paste0(x, collapse = ""))
  label <- paste0(" ", n," x ", n, " (", format(floor(sum(!is.na(M))*100/n^2), nsmall = 0, digits = 0),"%)")
  label <- format(label, width = 2*DISPLAY_SZ, justify = "c")
  bar <- paste0(rep("\u2500", 2*DISPLAY_SZ), collapse = "")
  message <- paste0(c(
    paste0(" \u250c", bar                 , "\u2500\u2510"),
    paste0(" \u2502", label               , " \u2502"),
    paste0(" \u251c", bar                 , "\u2500\u2524"),
    paste0(" \u2502", display_rows,         " \u2502"),
    paste0(" \u2514", bar                 , "\u2500\u2518")
  ), collapse = "\n")
  message(message)
}

custom_LDlinkR_LDmatrix <- function(snps, pop , r2d, token) {
  url <- "https://ldlink.nci.nih.gov/LDlinkRest/ldmatrix"
  snps_to_upload <- paste(unlist(snps), collapse = "\n")
  pop_to_upload <- paste(unlist(pop), collapse = "+")
  jsonbody <- list(snps = snps_to_upload, pop = pop_to_upload, r2_d = r2d)
  url_str <- paste(url, "?", "&token=", token, sep = "")
  # if (httr::http_error(url)) {
  #   message("The LDlink server is down or not accessible. Please try again later.")
  #   return(NULL)
  # }
  # else {
  #   message("\nLDlink server is working...\n")
  # }
  raw_out <- httr::POST(url = url_str, body = jsonbody, encode = "json")
  httr::stop_for_status(raw_out)
  data_out <- read.delim(textConnection(httr::content(raw_out, "text", encoding = "UTF-8")), header = T, sep = "\t")
  return(data_out)
}

retrieve_LDLink_parallel <- function(snps_list, token_list, pop , r2d) {
  url_base <- "https://ldlink.nci.nih.gov/LDlinkRest/ldmatrix"

  if (!is.list(snps_list)) snps_list <- list(snps_list)
  snps_list <- snps_list[!sapply(snps_list, function(x) length(x) == 0)]
  n <- length(snps_list)
  results <- list()

  for (snps in snps_list) {
    if (!(length(snps) > 1) & (length(snps) <= 1000)) stop("Error: Each LDLink input must be between 2 to 1000 variants.")
  }

  # define urls
  urls <- sapply(token_list, function(token) {
    paste0(url_base, "?token=", token)
  })

  # define IDs
  ids = paste0("request ", seq_len(n))

  # create callback fns
  callback_fns_done <- lapply(ids, function(id) {
    function(res) {
      data <- read.delim(textConnection(httr__parse_text(res$content, encoding = "UTF-8")), header = T, sep = "\t", row.names = 1)
      error_chk <- grepl("error", data, ignore.case = T)
      if (any(error_chk)) {
        stop("Error detected in returned LDLink data:\n\n", paste0(data[,error_chk], collapse = "\n"), call. = F)
      }
      results[[id]] <<- as.matrix(data)
      message("Received data for ", nrow(results[[id]]), " snps (", id, " of ", n, ").")
    }
  })

  callback_fns_fail <- lapply(ids, function(id) {
    function(res) {
      results[[id]] <<- NULL
      message("Timeout: no data received for", id, " of ", n, ".")
    }
  })

  # create the request pool
  pool = curl::new_pool(total_con = 10 * max(10, n), host_con = max(10, n))

  # create handles & add handles to the pool
  for (i in 1:n) {
    snps_to_upload <- paste(unlist(snps_list[[i]]), collapse = "\n")
    jsonbody <-
      list(snps = snps_to_upload, pop = pop, r2_d = r2d) %>%
      jsonlite::toJSON(., auto_unbox = TRUE, digits = 22) %>%
      as.character()

    curl::new_handle() %>%
      curl::handle_setheaders("Content-Type" = "application/json") %>%
      curl::handle_setopt(
        customrequest = "POST",
        url = enc2utf8(urls[i]),
        postfields = jsonbody
      ) %>%
      curl::multi_add(., done = callback_fns_done[[i]], fail = callback_fns_fail[[i]], pool = pool)

    message("Querying LDLink (", length(snps_list[[i]]), " snps)...")
  }

  # make the requests
  curl::multi_run(pool = pool, timeout = 60 * 5 * max(1, ceiling(n/3)))

  # return results
  results[ids]
}

## ------------------------------------ PLink Version -------------------- ----------------
#' @title Retrieve LD matrix with plink 1.9 and a local genotype file
#' @description This function is a wrapper around a system call to plink 1.9 to generate LD statistics for a list of SNPs. It requires a set of .bed/.bim/.bam files as input, which can be provided in the same format as plink's --bfile parameter. Alternatively, you can provide a .tar (possibly compressed) file containing the .bed/.bim/.bam files, and this function will unpack it on-the-fly.
#' @param snps List of variants
#' @param pop A 1000 Genomes Project population, default = "EUR"
#' @param plink Path to plink v1.9 executabl.
#' @param genofile Location of the local plink file containing 1000G data. This can be one of the the .bed/.bim/.bam files, and the remaining filenames are assumed to have the same basename as the provided file. You can also provide a file basename in the same format as plink's --bfile parameter. Alternatively, a tar file (possibly compressed) containing the .bed/.bim/.bam files can be provided, and it will be temporarily unpacked/decompressed. NOTE: the .bam files are assume to list alleles in ALT/REF order. plink then assigns ALT to A1 and REF to A2 in plink, using the `--keep-allele-order` parameter.
#' @param ld_window_kb From PLINK: By default, when a limited window report is requested, every pair of variants with at least (10-1) variants between them, or more than 1000 kilobases apart, is ignored. You can change the first threshold with --ld-window, and the second threshold with --ld-window-kb.
#' @param ld_window From PLINK: By default, when a limited window report is requested, every pair of variants with at least (10-1) variants between them, or more than 1000 kilobases apart, is ignored. You can change the first threshold with --ld-window, and the second threshold with --ld-window-kb.
#' @param num_threads Number of threads for plink
#' @param mode "r" or "r2". Whether to calculate r or r2.
#' @param sys.path Optional path variable to (temporarily) append to R's PATH environment variable. Useful for automatically finding executables, such as plink, tar decompressors, etc.
#' @param maf MAF cutoff for plink filtering.
#' @param hwe HWE cutoff for plink filtering.
#' @param geno geno cutoff plink filtering.
#' @param ... Additional options to be sent to plink
#' @export
ld_matrix_1000g_plink <- function(snps,
                            genofile,
                            mode = c("r", "r2"),
                            plink = Sys.which("plink"),
                            pop = "EUR",
                            maf = 0.01,
                            hwe = 10e-6,
                            geno = 0.1,
                            ld_window_kb = 10000,
                            ld_window = 100000,
                            num_threads = parallel::detectCores(),
                            sys.path = NULL,
                            ...) {
  ## ---- Validate options ----
  orig_path <- Sys.getenv("PATH")
  if (!is.null(sys.path) && !grepl(sys.path, orig_path)) {
    Sys.setenv(PATH = paste(orig_path, sys.path, sep = ":"))
    on.exit(add = T, expr = {
      Sys.setenv(PATH = orig_path)
    })
  }
  stopifnot("Could not find plink executable" = file.exists(plink))
  stopifnot("plink file is not executable" = file.access(plink, 1) == 0)
  plink_version <- system(paste(plink, "--version"), intern = T)
  stopifnot("PLINK v1.90 not detected" = grepl("^PLINK v1\\.90", plink_version))
  avail_pop = c("YRI", "LWK", "GWD", "MSL", "ESN", "ASW",
                "ACB", "MXL", "PUR", "CLM", "PEL", "CHB", "JPT",
                "CHS", "CDX", "KHV", "CEU", "TSI", "FIN", "GBR",
                "IBS", "GIH", "PJL", "BEB", "STU", "ITU", "ALL",
                "AFR", "AMR", "EAS", "EUR", "SAS")
  pop <- match.arg(pop, avail_pop)
  snps <- unique(unlist(snps))
  mode <- match.arg(mode)

  rsid_pattern <- "^rs\\d{1,}"
  chr_coord_pattern <- "(^chr)(\\d{1,2}|X|x|Y|y):(\\d{1,9})$"
  for (i in 1:length(snps)) {
    if (!((grepl(rsid_pattern, snps[i], ignore.case = TRUE)) |
          (grepl(chr_coord_pattern, snps[i], ignore.case = TRUE)))) {
      warning("Invalid query format for variant: ", snps[i], ".", immediate. = T, call. = F)
    }
  }

  genofile <- tools::file_path_as_absolute(genofile)

  ## ---- optionally unpack tar ----
  tar_check <- "\\.tar(\\.(zip|gz|zst|xz|bz2|lzma))?$"
  unpack_tar <- grepl(tar_check, genofile, perl = T)
  if (unpack_tar) {
    unpack_location <- tempfile()
    dir.create(unpack_location, showWarnings = F)
    system2("tar", c("-xvf", genofile, "-C", unpack_location))
    genofile <- unpack_location %//% stringr::str_remove(basename(genofile), tar_check)
    on.exit(add = T, expr = {
      unlink(unpack_location, force = T, recursive = T)
    })
  }

  ## ---- check for files ----
  genofile_dirname <- dirname(genofile)
  genofile_basename <- basename(tools::file_path_sans_ext(genofile))
  genofile <- genofile_dirname %//% genofile_basename
  bed_exists <- file.exists(genofile %++% ".bed")
  bim_exists <- file.exists(genofile %++% ".bim")
  fam_exists <- file.exists(genofile %++% ".fam")

  if (!(bed_exists && bim_exists && fam_exists)) {
    stop("Could not find ", genofile_basename, ".bed/.bim/.fam")
  }

  ## ---- Perform plink lookup ----
  # create tmp files
  sample_list_file <- tempfile(fileext = ".txt")
  snp_list_file <- tempfile(fileext = ".txt")
  outfile <- tempfile()
  on.exit(add = T, expr = {
    unlink(c(sample_list_file, snp_list_file, paste0(outfile, "*")), force = T, expand = T)
  })
  # write the sample file in FID IID format
  readr::read_tsv(genofile %++% ".fam", col_types = "cc____", col_names = F) %>%
    dplyr::filter(.[[2]] %in% population_samples[[pop]]) %>%
    readr::write_tsv(sample_list_file, col_names = F)
  writeLines(snps, snp_list_file)

  plink_args <- c(
    paste0("--", mode),
    "--bfile", genofile,
    "--keep-allele-order",
    "--allow-extra-chr",
    "--keep", sample_list_file,
    "--extract", snp_list_file,
    "--ld-window-r2", "0",
    "--ld-window-kb", ld_window_kb,
    "--ld-window", ld_window,
    "--maf", maf,
    "--hwe", hwe,
    "--geno", geno,
    "--threads", num_threads,
    "--out", outfile,
    ...
  )
  res <- system2(plink, plink_args)
  if (res != 0) stop("plink command failed", call. = F)

  ld_data <- read.table(outfile %++% ".ld", header = T, strip.white = T, colClasses = c("factor", "integer", "character", "factor", "integer", "character", "double"))

  snp_levels <- unique(c(unique(ld_data$SNP_A), unique(ld_data$SNP_B)))
  ld_data$SNP_A <- factor(ld_data$SNP_A, levels = snp_levels)
  ld_data$SNP_B <- factor(ld_data$SNP_B, levels = snp_levels)
  ld_data <- dplyr::distinct(ld_data, SNP_A, SNP_B, .keep_all = T)
  return(ld_data)
}

#' @title Convert a paired list of LD values to a symmetric matrix
#' @description Convert a dataframe that lists pairs of snps with their LD values into a symmetric LD matrix
#'
#' @param data Input dataframe
#' @param SNP_A Column name containing names of SNP_A
#' @param SNP_B Column name containing names of SNP_B
#' @param missing_value Value to use to fill missing entries
#' @param R Column name containing R (or R2) values
#'
#' @return Symmetric matrix
#' @export
ld_list_to_matrix <- function(data,
                              SNP_A = "SNP_A",
                              SNP_B = "SNP_B",
                              R = "R",
                              missing_value = NA) {

  snps_A <- dplyr::pull(data, {{SNP_A}})
  snps_B <- dplyr::pull(data, {{SNP_B}})

  if (!(is.factor(snps_A) && is.factor(snps_B) && identical(levels(snps_A), levels(snps_B)))) {
    snps_levels <- unique(c(snps_A, snps_B))
    snps_A <- factor(snps_A, levels = snps_levels)
    snps_B <- factor(snps_B, levels = snps_levels)
  } else {
    snps_levels <- levels(snps_A)
  }

  res <-
    dplyr::tibble(
      SNP_A = snps_A,
      SNP_B = snps_B,
      R = dplyr::pull(data, {{R}})
    ) %>%
    dplyr::distinct(SNP_A, SNP_B, .keep_all = T) %>%
    dplyr::filter(SNP_A != SNP_B) %>%
    dplyr::bind_rows(
      tibble::tibble(
        "SNP_A" = factor(seq_along(snps_levels), levels = seq_along(snps_levels), labels = snps_levels),
        "SNP_B" = SNP_A,
        "R" = as.numeric(1))
    ) %>%
    tidyr::pivot_wider(names_from = SNP_B, values_from = R, names_sort = T, values_fill = missing_value) %>%
    tibble::column_to_rownames("SNP_A")

  res <- as.matrix(res)
  stopifnot(identical(dimnames(res)[[1]], dimnames(res)[[2]]))
  res[lower.tri(res)] <- t(res)[lower.tri(res)]

  matrix(as.numeric(res), nrow = nrow(res), ncol = ncol(res), dimnames = dimnames(res))
}
