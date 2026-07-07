# Internal constants for the external simulation dataset.

.simdata_version <- "1.1.0"
.simdata_md5 <- "8bc7f8256e60e76612e871104e2f2756"
.simdata_record_id <- "21169900"
.simdata_file <- "sim_data_parquet.zip"
.simdata_dirname <- "sim_data_parquet"
.simdata_meta_file <- "ARAPONGA_SIMDATA_VERSION"

.simdata_url <- function() {
  paste0(
    "https://zenodo.org/records/",
    .simdata_record_id,
    "/files/",
    .simdata_file,
    "?download=1"
  )
}

.simdata_is_current <- function() {
  dest_dir <- tools::R_user_dir("araponga", "cache")
  dataset_dir <- file.path(dest_dir, .simdata_dirname)
  meta_file <- file.path(dest_dir, .simdata_meta_file)
  
  if (!dir.exists(dataset_dir) || !file.exists(meta_file)) {
    return(FALSE)
  }
  
  parquet_files <- list.files(
    dataset_dir,
    pattern = "\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(parquet_files) == 0) {
    return(FALSE)
  }
  
  meta <- readLines(meta_file, warn = FALSE)
  
  any(meta == paste0("simdata_version: ", .simdata_version)) &&
    any(meta == paste0("zip_md5: ", .simdata_md5))
}