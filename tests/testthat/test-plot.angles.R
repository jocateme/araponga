test_that("plot.angles validates angles input", {
  expect_error(
    plot.angles(),
    "`angles` must be provided \\(numeric vector, list, or data.frame\\)."
  )
  expect_error(
    plot.angles(numeric(0)),
    "`angles` must be provided \\(numeric vector, list, or data.frame\\)."
  )
})

test_that("plot.angles validates type and facing choices", {
  expect_error(
    plot.angles(1:3, type = "bad"),
    "'arg' should be one of"
  )
  expect_error(
    plot.angles(1:3, facing = "bad"),
    "'arg' should be one of"
  )
})

test_that("plot.angles validates angle values", {
  expect_error(
    plot.angles(c(-180, 0)),
    "All `angles` must satisfy -180 < angle <= 180 degrees."
  )
  expect_error(
    plot.angles(c(0, 181)),
    "All `angles` must satisfy -180 < angle <= 180 degrees."
  )
})

test_that("plot.angles rejects non-numeric angle values", {
  expect_error(
    plot.angles(list(yaw = "a")),
    "`angles` must be numeric."
  )
  expect_error(
    plot.angles(data.frame(yaw = "a")),
    "`angles` must be numeric."
  )
})

test_that("plot.angles runs for numeric vectors", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(plot.angles(60:70, type = "pitch", labels = FALSE))
  expect_no_error(plot.angles(-30:-40, type = "view_elevation", labels = FALSE))
  expect_no_error(plot.angles(-100:-45, type = "yaw", labels = FALSE))
  expect_no_error(plot.angles(c(-90, 0, 90, 180), labels = FALSE))
})

test_that("plot.angles runs when adding to an existing plot", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  plot.angles(0:10, type = "yaw", labels = FALSE)
  expect_no_error(plot.angles(20:30, type = "yaw", add = TRUE, labels = FALSE))
})
