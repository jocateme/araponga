test_that("find.3d validates pitch2d and arguments before lookup", {
  expect_error(find.3d(), "`pitch2d` must be provided.")
  expect_error(find.3d(numeric(0), label_error = 1), "`pitch2d` must be provided.")
  expect_error(find.3d("a", label_error = 1), "`pitch2d` must be a finite numeric vector.")
  expect_error(find.3d(c(0, Inf), label_error = 1), "`pitch2d` must be a finite numeric vector.")
  expect_error(find.3d(181, label_error = 1), "`pitch2d` must satisfy -180 < value <= 180 degrees.")
  expect_error(find.3d(-180, label_error = 1), "`pitch2d` must satisfy -180 < value <= 180 degrees.")

  expect_error(find.3d(0, find = 1, label_error = 1), "`find` must be NULL or a character vector.")
  expect_error(find.3d(0, find = character(0), label_error = 1), "`find` must be NULL or a character vector.")
  expect_error(find.3d(0, find = c("pitch", NA), label_error = 1), "`find` must be NULL or a character vector.")
  expect_error(
    find.3d(0, find = "not-an-angle", label_error = 1),
    "`find` contains invalid"
  )

  expect_error(find.3d(0, candidate_view_elevations = "a", label_error = 1),
               "`candidate_view_elevations` must be a finite numeric vector or NULL.")
  expect_error(find.3d(0, candidate_view_elevations = 91, label_error = 1),
               "`candidate_view_elevations` must satisfy -90 <= value <= 90 degrees.")

  expect_error(find.3d(0, candidate_pitches = "a", label_error = 1),
               "`candidate_pitches` must be a finite numeric vector or NULL.")
  expect_error(find.3d(0, candidate_pitches = 91, label_error = 1),
               "`candidate_pitches` must satisfy -90 <= value <= 90 degrees.")

  expect_error(find.3d(0, candidate_yaws = "a", label_error = 1),
               "`candidate_yaws` must be a finite numeric vector or NULL.")
  expect_error(find.3d(0, candidate_yaws = -180, label_error = 1),
               "`candidate_yaws` must satisfy -180 < value <= 180 degrees.")

  expect_error(find.3d(c(0, 1), label_error = 0), "If supplied, `label_error` must be a positive finite numeric scalar.")
  expect_error(find.3d(0, label_error = 0), "`label_error` must be > 0.")
  expect_error(find.3d(0, label_error = NA_real_), "`label_error` must be a finite numeric scalar.")
  expect_error(find.3d(0, label_error = c(1, 2)), "`label_error` must be a finite numeric scalar.")

  expect_error(find.3d(0, label_error = 1, label_nsamp = 0),
               "`label_nsamp` must be a positive integer scalar.")
  expect_error(find.3d(0, label_error = 1, label_nsamp = 1.5),
               "`label_nsamp` must be a positive integer scalar.")
  expect_error(find.3d(0, label_error = 1, label_nsamp = "x"),
               "`label_nsamp` must be a positive integer scalar.")

  expect_error(find.3d(0, label_error = 1, sim_download = NA),
               "`sim_download` must be a logical scalar.")
})

test_that("find.yaw and find.pitch validate paired", {
  expect_error(find.yaw(0, label_error = 1, paired = NA), "`paired` must be a logical scalar.")
  expect_error(find.pitch(0, label_error = 1, paired = 1), "`paired` must be a logical scalar.")
})

test_that("CRAN-friendly when current simulation data are absent", {
  skip_if(.simdata_is_current())
  
  expect_error(
    find.3d(0, label_error = 1, sim_download = FALSE),
    "current simulation dataset was not found locally"
  )
  
  expect_error(
    find.yaw(0, label_error = 1, sim_download = FALSE),
    "current simulation dataset was not found locally"
  )
  
  expect_error(
    find.pitch(0, label_error = 1, sim_download = FALSE),
    "current simulation dataset was not found locally"
  )
})

test_that("find wrappers return the expected structure when data are available", {
  skip_if_not(.simdata_is_current())

  p2d <- pitch2d.from.xy(100, 0, 0, 0)

  yaw_vals <- find.yaw(p2d, label_error = 1, sim_download = FALSE)
  expect_type(yaw_vals, "integer")
  expect_true(is.integer(yaw_vals))
  expect_identical(yaw_vals, sort(unique(yaw_vals)))
  expect_true(all(yaw_vals > -180 & yaw_vals <= 180))

  pitch_vals <- find.pitch(p2d, label_error = 1, sim_download = FALSE)
  expect_type(pitch_vals, "integer")
  expect_true(is.integer(pitch_vals))
  expect_true(all(pitch_vals >= -90 & pitch_vals <= 90))

  yaw_pair <- find.yaw(p2d, label_error = 1, paired = TRUE, sim_download = FALSE)
  expect_s3_class(yaw_pair, "data.frame")
  expect_true(all(c("pitch", "yaw") %in% names(yaw_pair)))
  expect_true(nrow(yaw_pair) >= 0)

  pitch_pair <- find.pitch(p2d, label_error = 1, paired = TRUE, sim_download = FALSE)
  expect_s3_class(pitch_pair, "data.frame")
  expect_true(all(c("yaw", "pitch") %in% names(pitch_pair)))
  expect_true(nrow(pitch_pair) >= 0)

  all_cols <- find.3d(p2d, label_error = 1, sim_download = FALSE)
  expect_s3_class(all_cols, "data.frame")
  expect_true(all(c("pitch", "yaw", "pitch2d", "view_elevation") %in% names(all_cols)))
})

test_that("vectorized pitch2d input does not require label_error", {
  skip_if_not(.simdata_is_current())
  res <- find.3d(c(-10, 0, 10), sim_download = FALSE)
  expect_s3_class(res, "data.frame")
})
