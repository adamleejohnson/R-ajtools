#' Embed audio in R Markdown documents
#'
#' `embed_audio()` provides a standard way to embed audio in R Markdown
#' documents when the output format is HTML, and to print placeholder text
#' when the output format is not HTML.
#'
#' `embed_audio()` is a wrapper for the HTML5 `<audio>` element that prints
#' HTML `<audio>` code in HTML documents built by R Markdown and placeholder
#' text in non-HTML documents built by R Markdown. This function may be useful
#' for conditional output that depends on the output format. For example, you
#' may embed audio in an R Markdown document when the output format is HTML,
#' and print placeholder text when the output format is LaTeX.
#'
#' The function determines output format using [knitr::is_html_output()]. By
#' default, these formats are considered as HTML formats: `c('markdown',
#' 'epub', 'html', 'html5', 'revealjs', 's5', 'slideous', 'slidy')`.
#'
#' @param src A path to the media file.
#' @param type The type of media file specified in `src`.
#' @param attribute A character vector specifying which attributes to use.
#'   "none" can be used if no attributes are desired.
#' @param id A character string specifying a unique ID for the element.
#'   Can be used by CSS or JavaScript to perform certain tasks for
#'   the element with the specific ID.
#' @param placeholder The placeholder text to use when the output format is
#'   not HTML.
#' @return If `knitr::is_html_output()` is `TRUE`, returns HTML `<audio>` code.
#'   If `knitr::is_html_output()` is `FALSE`, returns placeholder text.
#' @note This function is supposed to be used in R code chunks or inline R code
#'   expressions. You are recommended to use forward slashes (/) as path
#'   separators instead of backslashes in the file paths.
#' @export
embed_audio <- function(src,
                        type = c("mpeg", "ogg", "wav"),
                        attribute = c("controls", "autoplay", "loop",
                                      "muted", "preload", "none"),
                        id = "",
                        placeholder = "") {
  # check if src has a valid media file extension
  is.audio(src)
  # check if the audio sources exist
  is.local(src)
  # check whether default values should be used for type or attribute
  if(missing(type)) {type <- "mpeg"}
  if(missing(attribute)) {attribute <- "controls"}
  # evaluate choices
  type <- match.arg(type,
                    c("mpeg", "ogg", "wav"),
                    several.ok = TRUE)
  attribute <- match.arg(attribute,
                         c("controls", "autoplay", "loop",
                           "muted", "preload", "none"),
                         several.ok = TRUE)
  # collapse attribute choices to a character string
  attribute <- paste(attribute, sep = " ", collapse = " ")
  # make attribute empty if "none" is in the character string
  if (all(grepl("none", attribute))) {attribute <- ""}
  # compare length of src and type character vectors
  if (length(src) != length(type)) {
    message("Arguments `src` and `type` are different lengths; ",
            "recycling the shorter vector.")
  }
  # print output
  if (knitr::is_html_output()) {
    # open <audio> tag
    if (missing(id)) {
      audio <- sprintf("<audio %1$s>", attribute)
    } else {
      audio <- sprintf("<audio id='%2$s' %1$s>", attribute, id)
    }
    # create <source> strings from vectors, then collapse to single string
    encoded_src <- xfun::base64_uri(src)
    audio_source <- paste(sprintf("<source src='%1$s' type='audio/%2$s'>",
                                  encoded_src, type), sep = "", collapse = "")
    # print the completed HTML <audio> output
    htmltools::HTML(audio, audio_source,
                    "Your browser does not support the audio tag; ",
                    "for browser support, please see: ",
                    "https://www.w3schools.com/tags/tag_audio.asp",
                    "</audio>")
  } else htmltools::HTML(placeholder)
}

#' Embed video in R Markdown documents
#'
#' `embed_video()` provides a standard way to embed video in R Markdown
#' documents when the output format is HTML, and to print placeholder text
#' when the output format is not HTML.
#'
#' `embed_video()` is a wrapper for the HTML5 `<video>` element that prints
#' HTML `<video>` code in HTML documents built by R Markdown and placeholder
#' text in non-HTML documents built by R Markdown. This function may be useful
#' for conditional output that depends on the output format. For example, you
#' may embed video in an R Markdown document when the output format is HTML,
#' and print placeholder text when the output format is LaTeX.
#'
#' The function determines output format using [knitr::is_html_output()]. By
#' default, these formats are considered as HTML formats: `c('markdown',
#' 'epub', 'html', 'html5', 'revealjs', 's5', 'slideous', 'slidy')`.
#'
#' @inheritParams embed_audio
#' @param width The width of the video, in pixels.
#' @param height The height of the video, in pixels.
#' @param thumbnail A path to an image.
#' @return If `knitr::is_html_output()` is `TRUE`, returns HTML `<video>` code.
#'   If `knitr::is_html_output()` is `FALSE`, returns placeholder text.
#' @note This function is supposed to be used in R code chunks or inline R code
#'   expressions. You are recommended to use forward slashes (/) as path
#'   separators instead of backslashes in the file paths.
#' @export
embed_video <- function(src,
                        type = c("mp4", "webm", "ogg"),
                        width = "320",
                        height = "240",
                        attribute = c("controls", "autoplay", "loop",
                                      "muted", "preload", "none"),
                        thumbnail = NULL,
                        id = "",
                        placeholder = "") {
  # check if src has a valid media file extension
  is.video(src)
  # check if the video sources exist
  is.local(src)
  # check whether default values should be used for type or attribute
  if(missing(type)) {type <- "mp4"}
  if(missing(attribute)) {attribute <- "controls"}
  # evaluate choices
  type <- match.arg(type,
                    c("mp4", "webm", "ogg"),
                    several.ok = TRUE)
  attribute <- match.arg(attribute,
                         c("controls", "autoplay", "loop",
                           "muted", "preload", "none"),
                         several.ok = TRUE)
  # collapse attribute choices to a character string
  attribute <- paste(attribute, sep = " ", collapse = " ")
  # make attribute empty if "none" is in the character string
  if (all(grepl("none", attribute))) {attribute <- ""}
  # compare length of src and type character vectors
  if (length(src) != length(type)) {
    message("Vectors `src` and `type` are different lengths; ",
            "recycling the shorter vector.")
  }
  # print output
  if (knitr::is_html_output()) {
    # decide whether to include thumbnail and id
    if (!missing(thumbnail) & !missing(id)) {
      # thumbnail and id
      video <- sprintf("<video id='%5$s' width='%1$s'
                       height='%2$s' %3$s poster='%4$s'>",
                       width, height, attribute, thumbnail, id)
    } else if (!missing(thumbnail) & missing(id)) {
      # thumbnail
      video <- sprintf("<video width='%1$s' height='%2$s' %3$s poster='%4$s'>",
                       width, height, attribute, thumbnail)
    } else if (missing(thumbnail) & !missing(id)) {
      # id
      video <- sprintf("<video id='%4$s' width='%1$s' height='%2$s' %3$s>",
                       width, height, attribute, id)
    } else {
      # no thumbnail or id
      video <- sprintf("<video width='%1$s' height='%2$s' %3$s>",
                       width, height, attribute)
    }
    # create <source> strings from vectors, then collapse to single string
    encoded_src <- xfun::base64_uri(src)
    video_source <- paste(sprintf("<source src='%1$s' type='video/%2$s'>",
                                  encoded_src, type), sep = "", collapse = "")
    # print the completed HTML <video> output
    htmltools::HTML(video, video_source,
                    "Your browser does not support the video tag; ",
                    "for browser support, please see: ",
                    "https://www.w3schools.com/tags/tag_video.asp",
                    "</video>")
  } else htmltools::HTML(placeholder)
}

#' Return strings without a URL scheme
#'
#' Given a character vector, returns a logical indicating whether the
#' paths in the vector point to existing local files.
#'
#' @param x A character vector.
is.local <- function(x) {
  # check if paths have a URL scheme
  paths <- is.url(x)
  # name paths
  names(paths) <- x
  # remove paths with a URL scheme
  paths <- paths[(paths %in% FALSE)]
  # check which paths exist
  paths.exist <- file.exists(names(paths))
  # name path.exists
  names(paths.exist) <- names(paths)
  # return result or error
  if (length(paths) == 0) {
    # default to NULL if no path names without a URL scheme are present
    NULL
  } else if (!all(paths.exist)) {
    # remove paths that exist
    paths.exist <- paths.exist[(paths.exist %in% FALSE)]
    # notify user of the paths causing the error
    if (length(paths.exist) > 1) {
      stop("The files: ", paste(names(paths.exist), sep = ", ",
                                collapse = ", "), " do not exist. Please use valid filepaths.")
    } else {
      stop("The file: ", paste0(names(paths.exist)), " does not exist. ",
      "Please use a valid filepath.")
    }
  } else all(file.exists(names(paths))) # return TRUE
}

#' Match string for audio suffix
#'
#' Given a character vector, returns a logical vector indicating
#' which elements have a valid audio file extension.
#'
#' @param x A character vector.
is.audio <- function(x) {
  # check if paths have a valid media file extension
  paths <- grepl(".mp3|.ogg|.wav", x)
  # name paths
  names(paths) <- x
  # check if all paths have valid media file extensions
  if (!all(grepl(".mp3|.ogg|.wav", x))) {
    # remove paths with a valid media file extension
    paths <- paths[(paths %in% FALSE)]
    # notify user of the paths causing the error
    if (length(paths) > 1) {
      stop(paste(names(paths), sep = ", ", collapse = ", "),
           " do not end with a valid audio file extension; ",
           "valid extensions are .mp3, .ogg, and .wav.")
    } else {
      stop(names(paths), " does not end with a valid audio file extension; ",
           "valid extensions are .mp3, .ogg, and .wav.")
    }
  } else all(grepl(".mp3|.ogg|.wav", x)) # return TRUE
}

#' Match string for video suffix
#'
#' Given a character vector, returns a logical vector indicating
#' which elements have a valid video file extension.
#'
#' @param x A character vector.
is.video <- function(x) {
  # check if paths have a valid media file extension
  paths <- grepl(".mp4|.webm|.ogg", x)
  # name paths
  names(paths) <- x
  # check if all paths have valid media file extensions
  if (!all(grepl(".mp4|.webm|.ogg", x))) {
    # remove paths with a valid media file extension
    paths <- paths[(paths %in% FALSE)]
    # notify user of the paths causing the error
    if (length(paths) > 1) {
      stop(paste(names(paths), sep = ", ", collapse = ", "),
           " do not end with a valid video file extension; ",
           "valid extensions are .mp4, .webm, and .ogg.")
    } else {
      stop(names(paths), " does not end with a valid video file extension; ",
           "valid extensions are .mp4, .webm, and .ogg.")
    }
  } else all(grepl(".mp4|.webm|.ogg", x)) # return TRUE
}
