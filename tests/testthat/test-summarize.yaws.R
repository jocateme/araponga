test_that("summarize.yaws validates required input", {
  expect_error(
    summarize.yaws(),
    "`yaws` must be a non-empty numeric vector."
  )
  expect_error(
    summarize.yaws(numeric(0)),
    "`yaws` must be a non-empty numeric vector."
  )
  expect_error(
    summarize.yaws("a"),
    "`yaws` must be numeric."
  )
  expect_error(
    summarize.yaws(c(-180, 0)),
    "`yaws` must satisfy -180 < yaw <= 180 degrees."
  )
  expect_error(
    summarize.yaws(c(0, 181)),
    "`yaws` must satisfy -180 < yaw <= 180 degrees."
  )
})

test_that("summarize.yaws excludes NA values with a warning", {
  expect_warning(
    res <- summarize.yaws(c(NA, -10, 20)),
    "1NA `yaws` excluded.",
    fixed = TRUE
  )

  expect_equal(res$from, -10)
  expect_equal(res$to, 20)
  expect_equal(res$width, 30)
  expect_false(res$wrap)
  expect_equal(res$all, list())

  expect_warning(
    expect_error(
      summarize.yaws(c(NA_real_, NA_real_)),
      "All `yaws` are NA; nothing to summarize."
    ),
    "2NA `yaws` excluded.",
    fixed = TRUE
  )
})

test_that("summarize.yaws returns zero-width interval for a singleton", {
  res <- summarize.yaws(30)

  expect_type(res, "list")
  expect_named(res, c("from", "to", "width", "wrap", "all"))
  expect_equal(res$from, 30)
  expect_equal(res$to, 30)
  expect_equal(res$width, 0)
  expect_false(res$wrap)
  expect_equal(res$all, list())
})

test_that("summarize.yaws handles duplicated singleton inputs", {
  res <- summarize.yaws(c(30, 30, 30))

  expect_equal(res$from, 30)
  expect_equal(res$to, 30)
  expect_equal(res$width, 0)
  expect_false(res$wrap)
  expect_equal(res$all, list())
})

test_that("summarize.yaws summarizes a non-wrapping interval", {
  res <- summarize.yaws(c(-60, -20, 0, 60))

  expect_equal(res$from, -60)
  expect_equal(res$to, 60)
  expect_equal(res$width, 120)
  expect_false(res$wrap)
  expect_equal(res$all, list())
})

test_that("summarize.yaws summarizes a wrapping interval", {
  res <- summarize.yaws(c(-175, -170, 171, 179))

  expect_equal(res$from, 171)
  expect_equal(res$to, -170)
  expect_equal(res$width, 19)
  expect_true(res$wrap)
  expect_equal(res$all, list())
})

test_that("summarize.yaws handles boundary value 180", {
  res <- summarize.yaws(c(170, 180, -170))

  expect_equal(res$from, 170)
  expect_equal(res$to, -170)
  expect_equal(res$width, 20)
  expect_true(res$wrap)
  expect_equal(res$all, list())
})

test_that("summarize.yaws warns and returns alternatives for ties by default", {
  expect_warning(
    res <- summarize.yaws(c(-90, 90)),
    "More than one smallest continuous interval found."
  )

  expect_equal(res$width, 180)
  expect_length(res$all, 1)
  expect_equal(res$all[[1]]$width, 180)

  intervals <- list(
    c(from = res$from, to = res$to),
    c(from = res$all[[1]]$from, to = res$all[[1]]$to)
  )

  expect_true(any(vapply(intervals, function(x) identical(unname(x), c(90, -90)), logical(1))))
  expect_true(any(vapply(intervals, function(x) identical(unname(x), c(-90, 90)), logical(1))))
})

test_that("summarize.yaws errors on ties when requested", {
  expect_error(
    summarize.yaws(c(-90, 90), tie_action = "error"),
    "More than one smallest continuous interval found."
  )
})

test_that("summarize.yaws validates tie_action through match.arg", {
  expect_error(
    summarize.yaws(c(-10, 10), tie_action = "bad"),
    "'arg' should be one of"
  )
})
