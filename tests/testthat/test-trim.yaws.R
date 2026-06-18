test_that("trim.yaws validates required arguments and scalar parameters", {
  expect_error(trim.yaws(min_sep = 0, max_sep = 1), "Both 'ccw_yaws' and 'cw_yaws' must be supplied.")
  expect_error(trim.yaws(ccw_yaws = 1, min_sep = 0, max_sep = 1), "Both 'ccw_yaws' and 'cw_yaws' must be supplied.")
  expect_error(trim.yaws(cw_yaws = 1, min_sep = 0, max_sep = 1), "Both 'ccw_yaws' and 'cw_yaws' must be supplied.")

  expect_error(trim.yaws(1, 2, min_sep = "a", max_sep = 1), "'min_sep' must be a single finite numeric value.")
  expect_error(trim.yaws(1, 2, min_sep = c(0, 1), max_sep = 1), "'min_sep' must be a single finite numeric value.")
  expect_error(trim.yaws(1, 2, min_sep = NA_real_, max_sep = 1), "'min_sep' must be a single finite numeric value.")

  expect_error(trim.yaws(1, 2, min_sep = 0, max_sep = "a"), "'max_sep' must be a single finite numeric value.")
  expect_error(trim.yaws(1, 2, min_sep = 0, max_sep = c(1, 2)), "'max_sep' must be a single finite numeric value.")
  expect_error(trim.yaws(1, 2, min_sep = 0, max_sep = NA_real_), "'max_sep' must be a single finite numeric value.")

  expect_error(trim.yaws(1, 2, min_sep = -1, max_sep = 1), "'min_sep' must be >= 0.")
  expect_error(trim.yaws(1, 2, min_sep = 0, max_sep = -1), "'max_sep' must be >= 0.")
  expect_error(trim.yaws(1, 2, min_sep = 2, max_sep = 1), "'min_sep' must be <= 'max_sep'.")
})

test_that("trim.yaws validates yaw vectors and bounds", {
  expect_error(trim.yaws("a", 1, min_sep = 0, max_sep = 1), "'ccw_yaws' and 'cw_yaws' must be numeric vectors.")
  expect_error(trim.yaws(1, "a", min_sep = 0, max_sep = 1), "'ccw_yaws' and 'cw_yaws' must be numeric vectors.")
  expect_error(trim.yaws(c(1, NA), 1, min_sep = 0, max_sep = 1), "'ccw_yaws' and 'cw_yaws' must not contain NA values.")
  expect_error(trim.yaws(1, c(1, NA), min_sep = 0, max_sep = 1), "'ccw_yaws' and 'cw_yaws' must not contain NA values.")

  expect_error(trim.yaws(-180, 1, min_sep = 0, max_sep = 1), "Yaw angles must satisfy -180 < yaw <= 180 degrees.")
  expect_error(trim.yaws(1, -180, min_sep = 0, max_sep = 1), "Yaw angles must satisfy -180 < yaw <= 180 degrees.")
  expect_error(trim.yaws(181, 1, min_sep = 0, max_sep = 1), "Yaw angles must satisfy -180 < yaw <= 180 degrees.")
  expect_error(trim.yaws(1, 181, min_sep = 0, max_sep = 1), "Yaw angles must satisfy -180 < yaw <= 180 degrees.")
})

test_that("trim.yaws trims to mutually consistent sets and de-duplicates inputs", {
  res <- trim.yaws(
    ccw_yaws = c(30, 10, 10, 20),
    cw_yaws  = c(5, 0, 0),
    min_sep = 10,
    max_sep = 15
  )

  expect_type(res, "list")
  expect_named(res, c("trimmed_ccw_yaws", "trimmed_cw_yaws"))
  expect_equal(res$trimmed_ccw_yaws, c(10, 20))
  expect_equal(res$trimmed_cw_yaws, c(0, 5))

  # Re-applying the function should not change already-trimmed sets.
  res2 <- trim.yaws(
    ccw_yaws = res$trimmed_ccw_yaws,
    cw_yaws = res$trimmed_cw_yaws,
    min_sep = 10,
    max_sep = 15
  )
  expect_equal(res2, res)
})

test_that("trim.yaws can return empty sets when no mutual partners exist", {
  res <- trim.yaws(
    ccw_yaws = -10,
    cw_yaws  = 130,
    min_sep = 0,
    max_sep = 180
  )

  expect_equal(res$trimmed_ccw_yaws, numeric(0))
  expect_equal(res$trimmed_cw_yaws, numeric(0))
})

test_that("trim.yaws handles empty input vectors", {
  res1 <- trim.yaws(
    ccw_yaws = numeric(0),
    cw_yaws  = 1,
    min_sep = 0,
    max_sep = 180
  )
  expect_equal(res1$trimmed_ccw_yaws, numeric(0))
  expect_equal(res1$trimmed_cw_yaws, numeric(0))

  res2 <- trim.yaws(
    ccw_yaws = 1,
    cw_yaws  = numeric(0),
    min_sep = 0,
    max_sep = 180
  )
  expect_equal(res2$trimmed_ccw_yaws, numeric(0))
  expect_equal(res2$trimmed_cw_yaws, numeric(0))
})
