#' Download the precomputed simulation dataset
#'
#' @description
#' Download the precomputed simulation dataset used by [find.3d()] and related functions.
#'
#' @param overwrite Logical scalar. If `TRUE`, re-download the archive even if a cached copy
#'  is already present.
#' @param quiet Logical scalar. If `TRUE`, suppress download progress and nonessential messages
#'  where supported by the underlying downloader.
#'
#' @return An invisible character scalar giving the normalized path to the extracted simulation
#' dataset directory.
#'
#' @details
#' The dataset is stored under `tools::R_user_dir("araponga", "cache")`. If the archive is already
#' available locally and `overwrite = FALSE`, the cached dataset is reused. See [find.3d()] for how
#' the dataset was constructed.
#'
#' @export
download.simdata <- function(overwrite = FALSE,
                             quiet = FALSE){
  
  
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("`overwrite` must be a logical scalar.", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    stop("`quiet` must be a logical scalar.", call. = FALSE)
  }
  
  dest_dir <- tools::R_user_dir("araponga", "cache")
  dataset_dir <- file.path(dest_dir, "sim_data_parquet")
  remote_url <- "https://zenodo.org/records/18982472/files/sim_data_parquet.zip?download=1"
  expected_md5 <- "7b3a60ffdadbf60f93b33a30c8088475"
  
  has_parquet_files <- function(path) {
    length(list.files(path, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)) > 0
  }
  
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Reuse a complete existing cache
  if (dir.exists(dataset_dir) && has_parquet_files(dataset_dir) && !overwrite) {
    return(normalizePath(dataset_dir, winslash = "/"))
  }
  
  # Remove incomplete or stale cache before rebuilding
  if (dir.exists(dataset_dir)) {
    unlink(dataset_dir, recursive = TRUE, force = TRUE)
  }
  
  zip_path <- file.path(dest_dir, "sim_data_parquet.zip")
  tmp_zip <- paste0(zip_path, ".partial")
  tmp_extract <- tempfile("sim_data_extract_", tmpdir = dest_dir)
  dir.create(tmp_extract, showWarnings = FALSE)
  
  on.exit({
    if (file.exists(tmp_zip)) {
      unlink(tmp_zip)
    }
    if (dir.exists(tmp_extract)) {
      unlink(tmp_extract, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  
  ua <- "araponga"
  
  if (!quiet) {
    message("Downloading zipped dataset...")
  }
  
  downloaded <- FALSE
  
  if (requireNamespace("curl", quietly = TRUE)) {
    h <- curl::new_handle()
    curl::handle_setheaders(h, "User-Agent" = ua)
    downloaded <- tryCatch({
      curl::curl_download(remote_url, tmp_zip, quiet = quiet, handle = h)
      TRUE
    }, error = function(e) FALSE)
  }
  
  if (!downloaded && requireNamespace("httr", quietly = TRUE)) {
    downloaded <- tryCatch({
      resp <- httr::GET(
        remote_url,
        httr::write_disk(tmp_zip, overwrite = TRUE),
        httr::user_agent(ua),
        if (!quiet) httr::progress()
      )
      httr::stop_for_status(resp)
      TRUE
    }, error = function(e) FALSE)
  }
  
  if (!downloaded) {
    downloaded <- tryCatch({
      utils::download.file(remote_url, tmp_zip, mode = "wb", quiet = quiet)
      TRUE
    }, error = function(e) FALSE)
  }
  
  if (!downloaded || !file.exists(tmp_zip)) {
    stop("Download failed. Check the URL and your network connection.", call. = FALSE)
  }
  
  if (file.exists(zip_path)) {
    unlink(zip_path)
  }
  ok <- file.rename(tmp_zip, zip_path)
  if (!ok || !file.exists(zip_path)) {
    stop("Could not move the downloaded archive into the cache directory.", call. = FALSE)
  }
  
  actual_md5 <- unname(tools::md5sum(zip_path))
  if (!identical(tolower(actual_md5), tolower(expected_md5))) {
    unlink(zip_path)
    stop(
      paste0(
        "Checksum mismatch: downloaded file may be corrupted.\n",
        "Expected: ", expected_md5, "\n",
        "Got:      ", actual_md5
      ),
      call. = FALSE
    )
  }
  
  utils::unzip(zip_path, exdir = tmp_extract)
  
  # Identify the extracted dataset location.
  if (has_parquet_files(tmp_extract)) {
    source_dir <- tmp_extract
  } else {
    top_dirs <- list.files(tmp_extract, full.names = TRUE, recursive = FALSE)
    top_dirs <- top_dirs[file.info(top_dirs)$isdir %in% TRUE]
    
    if (length(top_dirs) == 1L && has_parquet_files(top_dirs[1])) {
      source_dir <- top_dirs[1]
    } else {
      parquet_dirs <- top_dirs[vapply(top_dirs, has_parquet_files, logical(1))]
      if (length(parquet_dirs) == 1L) {
        source_dir <- parquet_dirs[1]
      } else {
        stop("Could not locate extracted simulation dataset. Check archive structure.", call. = FALSE)
      }
    }
  }
  
  if (dir.exists(dataset_dir)) {
    unlink(dataset_dir, recursive = TRUE, force = TRUE)
  }
  ok <- file.rename(source_dir, dataset_dir)
  if (!ok || !dir.exists(dataset_dir)) {
    stop("Could not move extracted dataset into the cache directory.", call. = FALSE)
  }
  
  if (file.exists(zip_path)) {
    unlink(zip_path)
  }
  
  out_dir2 <- paste0(out_dir, "2")
  file.rename(out_dir,
              out_dir2)
  file.rename(file.path(out_dir2, "sim_data_parquet"),
               out_dir)
  unlink(out_dir2, recursive = TRUE)
  
  out_dir <- normalizePath(dataset_dir, winslash = "/")
  if (!quiet) {
    message("Simulation dataset available at: ", out_dir)
  }
  invisible(out_dir)
}