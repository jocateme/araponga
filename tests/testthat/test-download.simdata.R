test_that("download.simdata validates arguments", {
  expect_error(
    download.simdata(overwrite = NA),
    "`overwrite` must be a logical scalar."
  )
  expect_error(
    download.simdata(overwrite = c(TRUE, FALSE)),
    "`overwrite` must be a logical scalar."
  )
  expect_error(
    download.simdata(overwrite = 1),
    "`overwrite` must be a logical scalar."
  )
  
  expect_error(
    download.simdata(quiet = NA),
    "`quiet` must be a logical scalar."
  )
  expect_error(
    download.simdata(quiet = c(TRUE, FALSE)),
    "`quiet` must be a logical scalar."
  )
  expect_error(
    download.simdata(quiet = 1),
    "`quiet` must be a logical scalar."
  )
})

test_that("download.simdata reuses existing current cache without downloading", {
  skip_on_cran()
  
  fake_cache <- tempfile("araponga-cache-")
  fake_dataset <- file.path(fake_cache, "sim_data_parquet")
  dir.create(fake_dataset, recursive = TRUE)
  
  writeLines("not real parquet content", file.path(fake_dataset, "part-0.parquet"))
  
  writeLines(
    c(
      "simdata_version: 1.1.0",
      "zip_md5: 8bc7f8256e60e76612e871104e2f2756"
    ),
    file.path(fake_cache, "ARAPONGA_SIMDATA_VERSION")
  )
  
  local_mocked_bindings(
    R_user_dir = function(package, which) fake_cache,
    .package = "tools"
  )
  
  res <- download.simdata(overwrite = FALSE, quiet = TRUE)
  
  expect_type(res, "character")
  expect_length(res, 1)
  expect_equal(
    normalizePath(res, winslash = "/"),
    normalizePath(fake_dataset, winslash = "/")
  )
})

test_that("download.simdata rejects unversioned existing cache", {
  skip_on_cran()
  
  fake_cache <- tempfile("araponga-cache-")
  fake_dataset <- file.path(fake_cache, "sim_data_parquet")
  dir.create(fake_dataset, recursive = TRUE)
  
  writeLines("not real parquet content", file.path(fake_dataset, "part-0.parquet"))
  
  local_mocked_bindings(
    R_user_dir = function(package, which) fake_cache,
    .package = "tools"
  )
  
  expect_error(
    download.simdata(overwrite = FALSE, quiet = TRUE),
    "older or unversioned|outdated|overwrite = TRUE"
  )
})
