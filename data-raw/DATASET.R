## code to prepare `DATASET` dataset goes here

samples_dir <- "~/Documents/+RESEARCH/MalhotraLab/Archives.nosync/1000G/1000G_population_samples/"
samples_files <- list.files(path = samples_dir, pattern = "*.txt")
samples_names <- xfun::sans_ext(samples_files)

population_samples <- sapply(samples_names, function(n) readLines(paste0(samples_dir, n, ".txt")))

usethis::use_data(population_samples, overwrite = TRUE, internal = T, compress = "gzip")
