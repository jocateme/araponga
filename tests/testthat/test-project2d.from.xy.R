test_that("project2d.from.xy validates required coordinates", {
  expect_error(
    project2d.from.xy(),
    "All coordinate arguments"
  )

  expect_error(
    project2d.from.xy(x_tip = 1, y_tip = 0, x_base = 0),
    "All coordinate arguments"
  )
})


test_that("project2d.from.xy rejects empty and non-numeric coordinates", {
  expect_error(
    project2d.from.xy(numeric(0), numeric(0), numeric(0), numeric(0)),
    "empty coordinate"
  )

  expect_error(
    project2d.from.xy("1", 0, 0, 0),
    "must be numeric"
  )

  expect_error(
    project2d.from.xy(1, "0", 0, 0),
    "must be numeric"
  )
})


test_that("project2d.from.xy requires finite coordinates", {
  expect_error(
    project2d.from.xy(c(1, NA), c(0, 0), c(0, 0), c(0, 0)),
    "finite values"
  )

  expect_error(
    project2d.from.xy(c(1, Inf), c(0, 0), c(0, 0), c(0, 0)),
    "finite values"
  )

  expect_error(
    project2d.from.xy(c(1, NaN), c(0, 0), c(0, 0), c(0, 0)),
    "finite values"
  )
})


test_that("project2d.from.xy enforces scalar-or-common-length recycling", {
  expect_error(
    project2d.from.xy(
      x_tip = c(1, 2),
      y_tip = c(1, 2, 3),
      x_base = c(0, 0),
      y_base = c(0, 0)
    ),
    "same length or be scalars"
  )

  expect_no_error(
    project2d.from.xy(
      x_tip = c(1, 0, -1),
      y_tip = c(0, 1, 0),
      x_base = 0,
      y_base = 0
    )
  )
})


test_that("project2d.from.xy validates plot", {
  expect_error(
    project2d.from.xy(1, 0, 0, 0, plot = NA),
    "`plot` must be a logical scalar"
  )

  expect_error(
    project2d.from.xy(1, 0, 0, 0, plot = 1),
    "`plot` must be a logical scalar"
  )

  expect_error(
    project2d.from.xy(1, 0, 0, 0, plot = c(TRUE, FALSE)),
    "`plot` must be a logical scalar"
  )
})


test_that("project2d.from.xy returns an araponga2d data frame", {
  observed <- project2d.from.xy(10, 20, 1, 2)

  expect_s3_class(observed, "araponga2d")
  expect_s3_class(observed, "data.frame")
  expect_equal(nrow(observed), 1)
  expect_named(
    observed,
    c("x_tip", "y_tip", "x_base", "y_base", "pitch2d", "length2d")
  )

  expect_equal(observed$x_tip, 10)
  expect_equal(observed$y_tip, 20)
  expect_equal(observed$x_base, 1)
  expect_equal(observed$y_base, 2)
})


test_that("project2d.from.xy returns expected cardinal directions and lengths", {
  observed <- project2d.from.xy(
    x_tip = c(1, 0, -1, 0),
    y_tip = c(0, 1, 0, -1),
    x_base = 0,
    y_base = 0
  )

  expect_equal(observed$pitch2d, c(0, 90, 180, -90))
  expect_equal(observed$length2d, rep(1, 4))
})


test_that("project2d.from.xy returns expected diagonal directions", {
  observed <- project2d.from.xy(
    x_tip = c(1, -1, -1, 1),
    y_tip = c(1, 1, -1, -1),
    x_base = 0,
    y_base = 0
  )

  expect_equal(observed$pitch2d, c(45, 135, -135, -45))
  expect_equal(observed$length2d, rep(sqrt(2), 4))
})


test_that("project2d.from.xy uses base-to-tip displacement", {
  observed <- project2d.from.xy(
    x_tip = 10,
    y_tip = 20,
    x_base = 1,
    y_base = 2
  )

  expect_equal(
    observed$pitch2d,
    atan2(18, 9) * 180 / pi,
    tolerance = 1e-12
  )

  expect_equal(
    observed$length2d,
    sqrt(9^2 + 18^2),
    tolerance = 1e-12
  )
})


test_that("project2d.from.xy is vectorized and recycles scalar coordinates", {
  observed <- project2d.from.xy(
    x_tip = c(1, 0, -1, 0),
    y_tip = c(0, 1, 0, -1),
    x_base = 0,
    y_base = 0
  )

  expect_s3_class(observed, "araponga2d")
  expect_equal(nrow(observed), 4)
  expect_equal(observed$x_base, rep(0, 4))
  expect_equal(observed$y_base, rep(0, 4))
  expect_equal(observed$pitch2d, c(0, 90, 180, -90))
})


test_that("project2d.from.xy handles zero-length projections row by row", {
  observed <- project2d.from.xy(
    x_tip = c(1, 2, 3),
    y_tip = c(0, 0, 4),
    x_base = c(0, 2, 0),
    y_base = c(0, 0, 0)
  )

  expect_equal(observed$pitch2d, c(0, NA_real_, atan2(4, 3) * 180 / pi))
  expect_equal(observed$length2d, c(1, 0, 5))
})


test_that("project2d.from.xy treats only exact zero length as degenerate", {
  observed <- project2d.from.xy(
    x_tip = 1e-20,
    y_tip = 0,
    x_base = 0,
    y_base = 0
  )

  expect_equal(observed$pitch2d, 0)
  expect_equal(observed$length2d, 1e-20)
})


test_that("project2d.from.xy disables plotting for vectorized input", {
  expected <- project2d.from.xy(
    x_tip = c(1, 0),
    y_tip = c(0, 1),
    x_base = 0,
    y_base = 0
  )

  expect_warning(
    observed <- project2d.from.xy(
      x_tip = c(1, 0),
      y_tip = c(0, 1),
      x_base = 0,
      y_base = 0,
      plot = TRUE
    ),
    "plot = FALSE"
  )

  expect_equal(observed, expected)
})


test_that("project2d.from.xy scalar plotting handles defined and degenerate projections", {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(
    project2d.from.xy(1, 1, 0, 0, plot = TRUE)
  )

  expect_silent(
    project2d.from.xy(1, 2, 1, 2, plot = TRUE)
  )
})
