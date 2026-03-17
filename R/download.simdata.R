#' Title
#'
#' @param overwrite 
#' @param quiet 
#'
#' @returns
#' @export
#'
#' @examples
download.simdata <- function(overwrite = FALSE,
                             quiet = FALSE){
  
  dest_dir <- tools::R_user_dir("araponga", "cache")
  remote_url <- "https://zenodo.org/records/18982472/files/sim_data_parquet.zip?download=1"
  expected_md5 <- "7b3a60ffdadbf60f93b33a30c8088475"
  
  stopifnot(is.character(remote_url), nzchar(remote_url))
  
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  # build a stable zip filename (strip query params)
  zip_path <- file.path(dest_dir, "sim_data_parquet.zip")
  
  if (dir.exists(zip_path) && !overwrite) {
    return(normalizePath(zip_path, winslash = "/"))
  }
  
  # Download to temporary file first to avoid leaving partial files on error
  tmp_zip <- paste0(zip_path, ".partial")
  on.exit({
    if (file.exists(tmp_zip)) try(file.remove(tmp_zip), silent = TRUE)
  }, add = TRUE)
  
  # downloader choice: curl -> httr -> utils::download.file
  downloaded <- FALSE
  ua <- paste0("araponga (mailto:jocateme@gmail.com)")
  
  message("Downloading zipped dataset (99MB)")
  if (requireNamespace("curl", quietly = TRUE)) {
    h <- curl::new_handle()
    curl::handle_setheaders(h, "User-Agent" = ua)
    try({
      curl::curl_download(remote_url, tmp_zip, quiet = quiet, handle = h)
      downloaded <- TRUE
    }, silent = FALSE)
  }
  
  if (!downloaded && requireNamespace("httr", quietly = TRUE)) {
    try({
      resp <- httr::GET(remote_url,
                        httr::write_disk(tmp_zip, overwrite = TRUE),
                        httr::user_agent(ua),
                        httr::progress())
      httr::stop_for_status(resp)
      downloaded <- TRUE
    }, silent = FALSE)
  }
  
  if (!downloaded) {
    # fallback to base download.file (no progress, less reliable for redirects)
    try({
      utils::download.file(remote_url, tmp_zip, mode = "wb", quiet = quiet)
      downloaded <- TRUE
    }, silent = FALSE)
  }
  
  if (!downloaded || !file.exists(tmp_zip)) {
    stop("Download failed. Check your `remote_url` and network connection.")
  }
  
  # rename final zip
  if (file.exists(zip_path) && overwrite) file.remove(zip_path)
  file.rename(tmp_zip, zip_path)
  
  # verify checksum
  actual_md5 <- digest::digest(zip_path, algo = "md5", file = TRUE)
  if (!identical(tolower(actual_md5), tolower(expected_md5))) {
    file.remove(zip_path)
    stop("Checksum mismatch: downloaded file may be corrupted.\n",
         "Expected: ", expected_md5, "\nGot:      ", actual_md5)
  }
  
  # unzip into dest_dir
  utils::unzip(zip_path, exdir = dest_dir)
  
  # determine extracted folder: prefer the expected name, otherwise pick the newest dir
  if (dir.exists(zip_path)) {
    out_dir <- zip_path
  } else {
    # try to find a newly created directory under dest_dir
    ds <- list.dirs(dest_dir, full.names = TRUE, recursive = FALSE)
    # ignore cache root
    ds <- setdiff(ds, dest_dir)
    if (length(ds) == 1L) {
      out_dir <- ds[1]
    } else {
      # if multiple candidates, try to detect a dir containing parquet files
      parquet_dir <- NULL
      for (d in ds) {
        if (length(list.files(d, pattern = "\\.parquet$", recursive = TRUE)) > 0) {
          parquet_dir <- d
          break
        }
      }
      if (!is.null(parquet_dir)) {
        out_dir <- parquet_dir
      } else {
        stop("Could not locate extracted simulation dataset. Check archive structure.")
      }
    }
  }
  
  unlink(zip_path)
  unlink(paste0(dest_dir, "/__MACOSX/"), recursive = TRUE)
  
  out_dir <- normalizePath(out_dir, winslash = "/")
  message("Simulation dataset available at: ", out_dir)
  invisible(out_dir)
}