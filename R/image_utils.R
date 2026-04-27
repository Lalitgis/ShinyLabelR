#' Read image dimensions using magick
#'
#' @param path File path or URL
#' @return Named list: width, height
#' @export
sl_image_info <- function(path) {
  tryCatch({
    img  <- magick::image_read(path)
    info <- magick::image_info(img)
    list(width = info$width[1], height = info$height[1])
  }, error = function(e) {
    warning("[ShinyLabel] Could not read image: ", path, " — ", e$message)
    list(width = 0L, height = 0L)
  })
}

#' Encode image file as base64 data URI for the canvas
#'
#' @param path File path to image
#' @return Character string: data URI
#' @export
sl_image_b64 <- function(path) {
  ext  <- tolower(tools::file_ext(path))
  mime <- switch(ext,
    "jpg"  = "image/jpeg",
    "jpeg" = "image/jpeg",
    "png"  = "image/png",
    "gif"  = "image/gif",
    "bmp"  = "image/bmp",
    "webp" = "image/webp",
    "image/jpeg"
  )
  raw_bytes <- readBin(path, "raw", file.info(path)$size)
  b64       <- base64enc::base64encode(raw_bytes)
  paste0("data:", mime, ";base64,", b64)
}

#' Download a URL image to a temp file, return local path + dimensions
#'
#' @param url Character URL
#' @return list: local_path, width, height, filename
#' @export
sl_fetch_url_image <- function(url) {
  ext      <- tools::file_ext(url)
  if (nchar(ext) == 0 || nchar(ext) > 4) ext <- "jpg"
  tmp_path <- tempfile(fileext = paste0(".", ext))

  tryCatch({
    utils::download.file(url, tmp_path, mode = "wb", quiet = TRUE)
    info     <- sl_image_info(tmp_path)
    filename <- basename(url)
    if (nchar(filename) == 0 || !grepl("\\.", filename)) {
      filename <- paste0("url_image_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    }
    list(
      local_path = tmp_path,
      width      = info$width,
      height     = info$height,
      filename   = filename
    )
  }, error = function(e) {
    stop("Failed to download image from URL: ", e$message)
  })
}

#' Validate that a file is a supported image format
#'
#' @param path File path
#' @return logical
#' @export
sl_is_image <- function(path) {
  ext <- tolower(tools::file_ext(path))
  ext %in% c("jpg", "jpeg", "png", "gif", "bmp", "webp", "tiff", "tif")
}

#' Get all image files from a directory
#'
#' @param dir_path Directory path
#' @param recursive Recurse into subdirectories?
#' @return Character vector of full paths
#' @export
sl_list_images <- function(dir_path, recursive = FALSE) {
  all_files <- list.files(dir_path, full.names = TRUE, recursive = recursive)
  all_files[vapply(all_files, sl_is_image, logical(1))]
}
