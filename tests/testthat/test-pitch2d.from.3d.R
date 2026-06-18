test_that("pitch2d.from.3d validates scalar numeric inputs", {
  expect_error(
    pitch2d.from.3d("a", 0, 0),
    "`pitch` must be a numeric scalar."
  )
  expect_error(
    pitch2d.from.3d(c(0, 1), 0, 0),
    "`pitch` must be a numeric scalar."
  )
  expect_error(
    pitch2d.from.3d(NA_real_, 0, 0),
    "`pitch` must be a numeric scalar."
  )

  expect_error(
    pitch2d.from.3d(0, "a", 0),
    "`yaw` must be a numeric scalar."
  )
  expect_error(
    pitch2d.from.3d(0, c(0, 1), 0),
    "`yaw` must be a numeric scalar."
  )
  expect_error(
    pitch2d.from.3d(0, NA_real_, 0),
    "`yaw` must be a numeric scalar."
  )

  expect_error(
    pitch2d.from.3d(0, 0, "a"),
    "`view_elevation` must be a numeric scalar."
  )
  expect_error(
    pitch2d.from.3d(0, 0, c(0, 1)),
    "`view_elevation` must be a numeric scalar."
  )
  expect_error(
    pitch2d.from.3d(0, 0, NA_real_),
    "`view_elevation` must be a numeric scalar."
  )
})

test_that("pitch2d.from.3d validates plot argument", {

  expect_error(
    pitch2d.from.3d(0, 0, 0, plot = c(TRUE, FALSE)),
    "`plot` must be a logical scalar."
  )
  expect_error(
    pitch2d.from.3d(0, 0, 0, plot = 1),
    "`plot` must be a logical scalar."
  )
})

test_that("pitch2d.from.3d validates angle bounds", {
  expect_error(
    pitch2d.from.3d(0, 0, -91),
    "`view_elevation` must satisfy -90 <= `view_elevation` <= 90 degrees."
  )
  expect_error(
    pitch2d.from.3d(0, 0, 91),
    "`view_elevation` must satisfy -90 <= `view_elevation` <= 90 degrees."
  )

  expect_error(
    pitch2d.from.3d(-91, 0, 0),
    "`pitch` must satisfy -90 <= `pitch` <= 90 degrees."
  )
  expect_error(
    pitch2d.from.3d(91, 0, 0),
    "`pitch` must satisfy -90 <= `pitch` <= 90 degrees."
  )

  expect_error(
    pitch2d.from.3d(0, -180, 0),
    "`yaw` must satisfy -180 < `yaw` <= 180"
  )
  expect_error(
    pitch2d.from.3d(0, 181, 0),
    "`yaw` must satisfy -180 < `yaw` <= 180"
  )
})

test_that("pitch2d.from.3d returns expected values for simple orientations", {
  expect_equal(unname(pitch2d.from.3d(0, 0, 0)), 0, tolerance = 1e-12)
  expect_equal(unname(pitch2d.from.3d(90, 0, 0)), 90, tolerance = 1e-12)
  expect_equal(unname(pitch2d.from.3d(-90, 0, 0)), -90, tolerance = 1e-12)
  expect_equal(unname(pitch2d.from.3d(0, 180, 0)), 180, tolerance = 1e-12)
})

test_that("pitch2d.from.3d agrees with rotate3d formula", {
  cases <- data.frame(
    pitch = c(15, 15, -20, 45, 0),
    yaw = c(0, -30, 45, 90, 180),
    view_elevation = c(0, 0, -30, 20, 0)
  )

  for (i in seq_len(nrow(cases))) {
    R_total <- rotate3d(
      pitch = cases$pitch[i],
      yaw = cases$yaw[i],
      roll = cases$view_elevation[i]
    )
    expected <- atan2(R_total[2, 1], R_total[1, 1]) * 180 / pi

    expect_equal(
      unname(pitch2d.from.3d(
        pitch = cases$pitch[i],
        yaw = cases$yaw[i],
        view_elevation = cases$view_elevation[i]
      )),
      expected,
      tolerance = 1e-12
    )
  }
})

test_that("pitch2d.from.3d accepts boundary values", {
  expect_no_error(pitch2d.from.3d(-90, -179.999, -90))
  expect_no_error(pitch2d.from.3d(90, 180, 90))
})

test_that("pitch2d.from.3d returns a finite numeric scalar", {
  res <- pitch2d.from.3d(15, -30, -20)

  expect_type(res, "double")
  expect_length(res, 1)
  expect_true(is.finite(res))
  expect_true(res > -180 && res <= 180)
})
