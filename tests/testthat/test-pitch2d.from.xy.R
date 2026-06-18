test_that("pitch2d.from.xy validates required coordinates", {
  expect_error(
    pitch2d.from.xy(),
    "All coordinate arguments \\(x_tip, y_tip, x_base, y_base\\) must be provided."
  )
  expect_error(
    pitch2d.from.xy(x_tip = 1, y_tip = 0, x_base = 0),
    "All coordinate arguments \\(x_tip, y_tip, x_base, y_base\\) must be provided."
  )
})

test_that("pitch2d.from.xy validates coordinate type, length, and NA values", {
  expect_error(
    pitch2d.from.xy(numeric(0), 0, 0, 0),
    "One or more empty coordinate arguments provided."
  )
  expect_error(
    pitch2d.from.xy("1", 0, 0, 0),
    "All coordinate arguments must be numeric."
  )
  expect_error(
    pitch2d.from.xy(1:2, 1:3, 0, 0),
    "Coordinate arguments must have the same length or be scalars."
  )
  expect_error(
    pitch2d.from.xy(c(1, NA), 0, 0, 0),
    "Coordinate arguments must not contain NA values."
  )
})

test_that("pitch2d.from.xy validates plot argument", {
  expect_error(
    pitch2d.from.xy(1, 0, 0, 0, plot = c(TRUE, FALSE)),
    "`plot` must be a logical scalar."
  )
  expect_error(
    pitch2d.from.xy(1, 0, 0, 0, plot = 1),
    "`plot` must be a logical scalar."
  )
})

test_that("pitch2d.from.xy returns expected cardinal directions", {
  expect_equal(as.numeric(pitch2d.from.xy(1, 0, 0, 0)), 0)
  expect_equal(as.numeric(pitch2d.from.xy(0, 1, 0, 0)), 90)
  expect_equal(as.numeric(pitch2d.from.xy(0, -1, 0, 0)), -90)
  expect_equal(as.numeric(pitch2d.from.xy(-1, 0, 0, 0)), 180)
})

test_that("pitch2d.from.xy handles diagonal directions", {
  expect_equal(as.numeric(pitch2d.from.xy(1, 1, 0, 0)), 45)
  expect_equal(as.numeric(pitch2d.from.xy(-1, 1, 0, 0)), 135)
  expect_equal(as.numeric(pitch2d.from.xy(-1, -1, 0, 0)), -135)
  expect_equal(as.numeric(pitch2d.from.xy(1, -1, 0, 0)), -45)
})

test_that("pitch2d.from.xy is vectorized and recycles scalar coordinates", {
  res <- pitch2d.from.xy(
    x_tip = c(1, 0, -1, 0),
    y_tip = c(0, 1, 0, -1),
    x_base = 0,
    y_base = 0
  )

  expect_type(res, "double")
  expect_equal(res, c(0, 90, 180, -90))
  expect_null(attr(res, "xy"))
})

test_that("pitch2d.from.xy attaches xy attribute only for scalar results", {
  res <- pitch2d.from.xy(10, 20, 1, 2)

  expect_equal(as.numeric(res), atan2(18, 9) * 180 / pi)

  xy <- attr(res, "xy")
  expect_type(xy, "list")
  expect_named(xy, c("x_tip", "y_tip", "x_base", "y_base"))
  expect_equal(xy$x_tip, 10)
  expect_equal(xy$y_tip, 20)
  expect_equal(xy$x_base, 1)
  expect_equal(xy$y_base, 2)
})

test_that("pitch2d.from.xy warns and disables plotting for vectorized inputs", {
  expect_warning(
    res <- pitch2d.from.xy(
      x_tip = c(1, 0),
      y_tip = c(0, 1),
      x_base = 0,
      y_base = 0,
      plot = TRUE
    ),
    "Setting `plot = FALSE`; plotting is only supported when all coordinate arguments are length 1."
  )

  expect_equal(res, c(0, 90))
})
