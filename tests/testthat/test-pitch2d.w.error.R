test_that("pitch2d.w.error validates observed2d class", {
  expect_error(
    pitch2d.w.error("a", label_error = 1),
    "`observed2d` must be an `araponga2d` object"
  )
  
  observed <- project2d.from.xy(1, 0, 0, 0)
  class(observed) <- "data.frame"
  
  expect_error(
    pitch2d.w.error(observed, label_error = 1),
    "`observed2d` must be an `araponga2d` object"
  )
})


test_that("pitch2d.w.error requires exactly one observation", {
  observed <- project2d.from.xy(
    x_tip = c(1, 2),
    y_tip = c(0, 0),
    x_base = 0,
    y_base = 0
  )
  
  expect_error(
    pitch2d.w.error(observed, label_error = 1),
    "exactly one observation"
  )
  
  expect_error(
    pitch2d.w.error(observed[FALSE, ], label_error = 1),
    "exactly one observation"
  )
})


test_that("pitch2d.w.error validates landmark columns", {
  observed <- project2d.from.xy(1, 0, 0, 0)
  observed$x_tip <- NULL
  
  expect_error(
    pitch2d.w.error(observed, label_error = 1),
    "must contain `x_tip`"
  )
})


test_that("pitch2d.w.error validates landmark coordinate values", {
  observed <- project2d.from.xy(1, 0, 0, 0)
  
  bad_type <- observed
  bad_type$x_tip <- "1"
  expect_error(
    pitch2d.w.error(bad_type, label_error = 1),
    "finite numeric values"
  )
  
  bad_na <- observed
  bad_na$x_tip <- NA_real_
  expect_error(
    pitch2d.w.error(bad_na, label_error = 1),
    "finite numeric values"
  )
  
  bad_inf <- observed
  bad_inf$x_tip <- Inf
  expect_error(
    pitch2d.w.error(bad_inf, label_error = 1),
    "finite numeric values"
  )
})


test_that("pitch2d.w.error validates label_error", {
  observed <- project2d.from.xy(1, 0, 0, 0)
  
  expect_error(
    pitch2d.w.error(observed),
    "label_error"
  )
  
  expect_error(
    pitch2d.w.error(observed, label_error = "1"),
    "finite numeric scalar"
  )
  
  expect_error(
    pitch2d.w.error(observed, label_error = c(1, 2)),
    "finite numeric scalar"
  )
  
  expect_error(
    pitch2d.w.error(observed, label_error = NA_real_),
    "finite numeric scalar"
  )
  
  expect_error(
    pitch2d.w.error(observed, label_error = Inf),
    "finite numeric scalar"
  )
  
  expect_error(
    pitch2d.w.error(observed, label_error = -1),
    "finite numeric scalar"
  )
  
  expect_no_error(
    pitch2d.w.error(observed, label_error = 0)
  )
})


test_that("pitch2d.w.error returns the expected interval structure", {
  observed <- project2d.from.xy(10, 0, 0, 0)
  result <- pitch2d.w.error(observed, label_error = 1)
  
  expect_type(result, "list")
  expect_named(result, c("from", "to", "width", "wrap", "all"))
  expect_type(result$wrap, "logical")
  expect_length(result$wrap, 1)
  expect_true(is.finite(result$from))
  expect_true(is.finite(result$to))
  expect_true(is.finite(result$width))
  expect_true(result$width >= 0 && result$width <= 360)
})


test_that("pitch2d.w.error with zero error returns the observed pitch", {
  observed <- project2d.from.xy(10, 5, 0, 0)
  result <- pitch2d.w.error(observed, label_error = 0)
  
  expect_equal(result$from, observed$pitch2d, tolerance = 1e-12)
  expect_equal(result$to, observed$pitch2d, tolerance = 1e-12)
  expect_equal(result$width, 0)
  expect_false(result$wrap)
  expect_equal(result$all, list())
})


test_that("pitch2d.w.error returns expected non-wrapping bounds", {
  observed <- project2d.from.xy(10, 0, 0, 0)
  result <- pitch2d.w.error(observed, label_error = 1)
  
  boundary <- atan2(2, 8) * 180 / pi
  
  expect_equal(result$from, -boundary, tolerance = 1e-12)
  expect_equal(result$to, boundary, tolerance = 1e-12)
  expect_equal(result$width, 2 * boundary, tolerance = 1e-12)
  expect_false(result$wrap)
})


test_that("pitch2d.w.error correctly handles intervals crossing 180", {
  observed <- project2d.from.xy(-10, 0, 0, 0)
  result <- pitch2d.w.error(observed, label_error = 1)
  
  boundary <- atan2(2, -8) * 180 / pi
  
  expect_equal(result$from, boundary, tolerance = 1e-12)
  expect_equal(result$to, -boundary, tolerance = 1e-12)
  expect_equal(result$width, 360 - 2 * boundary, tolerance = 1e-12)
  expect_true(result$wrap)
})


test_that("pitch2d.w.error returns the full circle when the origin lies inside the error region", {
  observed <- project2d.from.xy(1, 1, 0, 0)
  result <- pitch2d.w.error(observed, label_error = 1)
  
  expect_equal(
    result,
    list(
      from = -180,
      to = 180,
      width = 360,
      wrap = FALSE,
      all = list()
    )
  )
})


test_that("pitch2d.w.error does not treat origin on the boundary as full circle", {
  observed <- project2d.from.xy(2, 2, 0, 0)
  result <- pitch2d.w.error(observed, label_error = 1)
  
  expect_lt(result$width, 360)
  expect_equal(result$from, 0, tolerance = 1e-12)
  expect_equal(result$to, 90, tolerance = 1e-12)
  expect_false(result$wrap)
})


test_that("pitch2d.w.error depends only on the base-to-tip displacement", {
  observed_1 <- project2d.from.xy(10, 5, 0, 0)
  observed_2 <- project2d.from.xy(110, 205, 100, 200)
  
  expect_equal(
    pitch2d.w.error(observed_1, label_error = 1),
    pitch2d.w.error(observed_2, label_error = 1),
    tolerance = 1e-12
  )
})


test_that("pitch2d.w.error handles exact coincident landmarks", {
  observed <- project2d.from.xy(1, 2, 1, 2)
  
  expect_error(
    pitch2d.w.error(observed, label_error = 0),
    "undefined 2D pitches"
  )
  
  expect_equal(
    pitch2d.w.error(observed, label_error = 1),
    list(
      from = -180,
      to = 180,
      width = 360,
      wrap = FALSE,
      all = list()
    )
  )
})

