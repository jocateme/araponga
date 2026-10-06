test_that("project2d.from.3d requires all three angle arguments", {
  expect_error(
    project2d.from.3d(),
    "All angle arguments"
  )

  expect_error(
    project2d.from.3d(pitch = 0),
    "All angle arguments"
  )

  expect_error(
    project2d.from.3d(pitch = 0, yaw = 0),
    "All angle arguments"
  )
})


test_that("project2d.from.3d rejects empty and non-numeric angle arguments", {
  expect_error(
    project2d.from.3d(numeric(0), numeric(0), numeric(0)),
    "empty angle"
  )

  expect_error(
    project2d.from.3d("0", 0, 0),
    "must be numeric"
  )

  expect_error(
    project2d.from.3d(0, "0", 0),
    "must be numeric"
  )

  expect_error(
    project2d.from.3d(0, 0, "0"),
    "must be numeric"
  )
})


test_that("project2d.from.3d requires equal-length angle vectors", {
  expect_error(
    project2d.from.3d(
      pitch = c(0, 10),
      yaw = c(0, 10, 20),
      view_elevation = c(0, 10)
    ),
    "must have the same length"
  )

  # scalar recycling of angle arguments is intentionally not supported
  expect_error(
    project2d.from.3d(
      pitch = c(0, 10),
      yaw = 0,
      view_elevation = c(0, 10)
    ),
    "must have the same length"
  )
})


test_that("project2d.from.3d requires finite angle values", {
  expect_error(
    project2d.from.3d(c(0, NA), c(0, 0), c(0, 0)),
    "finite"
  )

  expect_error(
    project2d.from.3d(c(0, 0), c(0, Inf), c(0, 0)),
    "finite"
  )

  expect_error(
    project2d.from.3d(c(0, 0), c(0, 0), c(0, NaN)),
    "finite"
  )
})


test_that("project2d.from.3d validates angle ranges", {
  expect_no_error(
    project2d.from.3d(
      pitch = c(-179.9, 180),
      yaw = c(-179.9, 180),
      view_elevation = c(-90, 90)
    )
  )

  expect_error(
    project2d.from.3d(-180, 0, 0),
    "`pitch` must satisfy"
  )

  expect_error(
    project2d.from.3d(180.1, 0, 0),
    "`pitch` must satisfy"
  )

  expect_error(
    project2d.from.3d(0, -180, 0),
    "`yaw` must satisfy"
  )

  expect_error(
    project2d.from.3d(0, 180.1, 0),
    "`yaw` must satisfy"
  )

  expect_error(
    project2d.from.3d(0, 0, -90.1),
    "`view_elevation` must satisfy"
  )

  expect_error(
    project2d.from.3d(0, 0, 90.1),
    "`view_elevation` must satisfy"
  )
})


test_that("project2d.from.3d validates full_length", {
  expect_error(
    project2d.from.3d(0, 0, 0, full_length = numeric(0)),
    "`full_length`"
  )

  expect_error(
    project2d.from.3d(0, 0, 0, full_length = "100"),
    "`full_length`"
  )

  expect_error(
    project2d.from.3d(0, 0, 0, full_length = 0),
    "positive finite"
  )

  expect_error(
    project2d.from.3d(0, 0, 0, full_length = -1),
    "positive finite"
  )

  expect_error(
    project2d.from.3d(0, 0, 0, full_length = Inf),
    "positive finite"
  )

  expect_error(
    project2d.from.3d(
      pitch = c(0, 10),
      yaw = c(0, 0),
      view_elevation = c(0, 0),
      full_length = c(10, 20, 30)
    ),
    "length 1 or the same length"
  )
})


test_that("project2d.from.3d validates plot", {
  expect_error(
    project2d.from.3d(0, 0, 0, plot = NA),
    "`plot` must be a logical scalar"
  )

  expect_error(
    project2d.from.3d(0, 0, 0, plot = 1),
    "`plot` must be a logical scalar"
  )

  expect_error(
    project2d.from.3d(0, 0, 0, plot = c(TRUE, FALSE)),
    "`plot` must be a logical scalar"
  )
})


test_that("project2d.from.3d returns expected structure without full_length", {
  observed <- project2d.from.3d(30, 20, 10)

  expect_type(observed, "list")
  expect_named(
    observed,
    c("pitch2d", "projection_factor", "dx_factor", "dy_factor")
  )

  expect_length(observed$pitch2d, 1)
  expect_length(observed$projection_factor, 1)
  expect_length(observed$dx_factor, 1)
  expect_length(observed$dy_factor, 1)
})


test_that("project2d.from.3d returns expected structure with full_length", {
  observed <- project2d.from.3d(30, 20, 10, full_length = 100)

  expect_type(observed, "list")
  expect_named(
    observed,
    c(
      "pitch2d", "length2d", "dx", "dy",
      "projection_factor", "dx_factor", "dy_factor"
    )
  )

  expect_equal(observed$length2d, 100 * observed$projection_factor)
  expect_equal(observed$dx, 100 * observed$dx_factor)
  expect_equal(observed$dy, 100 * observed$dy_factor)
})


test_that("project2d.from.3d returns expected simple orientations", {
  right <- project2d.from.3d(0, 0, 0)
  expect_equal(right$pitch2d, 0)
  expect_equal(right$dx_factor, 1)
  expect_equal(right$dy_factor, 0)
  expect_equal(right$projection_factor, 1)

  up <- project2d.from.3d(90, 0, 0)
  expect_equal(up$pitch2d, 90)
  expect_equal(up$dx_factor, 0)
  expect_equal(up$dy_factor, 1)
  expect_equal(up$projection_factor, 1)

  left <- project2d.from.3d(0, 180, 0)
  expect_equal(left$pitch2d, 180)
  expect_equal(left$dx_factor, -1)
  expect_equal(left$dy_factor, 0)
  expect_equal(left$projection_factor, 1)
})


test_that("project2d.from.3d canonicalizes individual theoretical zero components", {
  observed <- project2d.from.3d(
    pitch = 0,
    yaw = 90,
    view_elevation = 30
  )

  expect_identical(observed$dx_factor, 0)
  expect_equal(observed$dy_factor, 0.5, tolerance = 1e-12)
  expect_equal(observed$pitch2d, 90)
  expect_equal(observed$projection_factor, 0.5, tolerance = 1e-12)
})


test_that("project2d.from.3d returns NA pitch for end-on projections", {
  observed <- project2d.from.3d(
    pitch = c(0, 0),
    yaw = c(90, 0),
    view_elevation = c(0, 0)
  )

  expect_equal(observed$pitch2d, c(NA_real_, 0))
  expect_equal(observed$projection_factor, c(0, 1))
  expect_equal(observed$dx_factor, c(0, 1))
  expect_equal(observed$dy_factor, c(0, 0))
})


test_that("project2d.from.3d scales absolute components and length by full_length", {
  relative <- project2d.from.3d(
    pitch = c(10, 30, 60),
    yaw = c(20, -40, 80),
    view_elevation = c(-10, 15, 30)
  )

  full_length <- c(50, 100, 150)

  absolute <- project2d.from.3d(
    pitch = c(10, 30, 60),
    yaw = c(20, -40, 80),
    view_elevation = c(-10, 15, 30),
    full_length = full_length
  )

  expect_equal(absolute$pitch2d, relative$pitch2d)
  expect_equal(absolute$projection_factor, relative$projection_factor)
  expect_equal(absolute$dx_factor, relative$dx_factor)
  expect_equal(absolute$dy_factor, relative$dy_factor)
  expect_equal(absolute$length2d, full_length * relative$projection_factor)
  expect_equal(absolute$dx, full_length * relative$dx_factor)
  expect_equal(absolute$dy, full_length * relative$dy_factor)
})


test_that("project2d.from.3d recycles scalar full_length", {
  observed <- project2d.from.3d(
    pitch = c(0, 30, 60),
    yaw = c(0, 0, 0),
    view_elevation = c(0, 0, 0),
    full_length = 100
  )

  expect_equal(observed$length2d, rep(100, 3), tolerance = 1e-12)
})


test_that("project2d.from.3d is vectorized elementwise", {
  pitch <- c(-60, -30, 0, 25, 70)
  yaw <- c(-120, -45, 0, 60, 150)
  view_elevation <- c(-40, -10, 20, 35, 50)

  vectorized <- project2d.from.3d(pitch, yaw, view_elevation)

  scalar_pitch2d <- vapply(
    seq_along(pitch),
    function(i) project2d.from.3d(
      pitch[i], yaw[i], view_elevation[i]
    )$pitch2d,
    numeric(1)
  )

  scalar_q <- vapply(
    seq_along(pitch),
    function(i) project2d.from.3d(
      pitch[i], yaw[i], view_elevation[i]
    )$projection_factor,
    numeric(1)
  )

  expect_equal(vectorized$pitch2d, scalar_pitch2d, tolerance = 1e-12)
  expect_equal(vectorized$projection_factor, scalar_q, tolerance = 1e-12)
})


test_that("project2d.from.3d agrees with rotate3d projected x axis", {
  pitch <- c(-170, -135, -35, 20, 135, 180)
  yaw <- c(-140, -80, -25, 40, 100, 160)
  view_elevation <- c(-50, -20, 10, 25, 45, 70)

  observed <- project2d.from.3d(pitch, yaw, view_elevation)

  expected_dx <- expected_dy <- numeric(length(pitch))

  for(i in seq_along(pitch)){
    R <- rotate3d(pitch[i], yaw[i], view_elevation[i])
    expected_dx[i] <- R[1, 1]
    expected_dy[i] <- R[2, 1]
  }

  tol <- sqrt(.Machine$double.eps)
  expected_dx[abs(expected_dx) <= tol] <- 0
  expected_dy[abs(expected_dy) <= tol] <- 0

  expect_equal(observed$dx_factor, expected_dx, tolerance = 1e-12)
  expect_equal(observed$dy_factor, expected_dy, tolerance = 1e-12)
})


test_that("project2d.from.3d projection factors are finite and bounded", {
  grid <- expand.grid(
    pitch = seq(-150, 180, by = 30),
    yaw = seq(-150, 180, by = 30),
    view_elevation = seq(-90, 90, by = 30),
    KEEP.OUT.ATTRS = FALSE
  )

  observed <- project2d.from.3d(
    grid$pitch,
    grid$yaw,
    grid$view_elevation
  )

  expect_true(all(is.finite(observed$projection_factor)))
  expect_true(all(observed$projection_factor >= 0))
  expect_true(all(observed$projection_factor <= 1 + 1e-12))

  defined <- observed$pitch2d[!is.na(observed$pitch2d)]
  expect_true(all(is.finite(defined)))
  expect_true(all(defined > -180))
  expect_true(all(defined <= 180))
  expect_false(any(is.nan(observed$pitch2d)))
  expect_false(any(is.infinite(observed$pitch2d)))
})


test_that("extended pitches have equivalent canonical representations", {
  extended <- project2d.from.3d(
    pitch = c(135, -135),
    yaw = c(10, 10),
    view_elevation = c(20, 20)
  )

  canonical <- project2d.from.3d(
    pitch = c(45, -45),
    yaw = c(-170, -170),
    view_elevation = c(20, 20)
  )

  expect_equal(extended, canonical, tolerance = 1e-12)
})


test_that("project2d.from.3d disables plotting for vectorized input", {
  expected <- project2d.from.3d(
    pitch = c(0, 30),
    yaw = c(0, 0),
    view_elevation = c(0, 0)
  )

  expect_warning(
    observed <- project2d.from.3d(
      pitch = c(0, 30),
      yaw = c(0, 0),
      view_elevation = c(0, 0),
      plot = TRUE
    ),
    "plot = FALSE"
  )

  expect_equal(observed, expected)
})


test_that("project2d.from.3d scalar plotting handles defined and degenerate projections", {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(
    project2d.from.3d(
      pitch = 15,
      yaw = -30,
      view_elevation = -20,
      full_length = 100,
      plot = TRUE
    )
  )

  expect_silent(
    project2d.from.3d(
      pitch = 0,
      yaw = 90,
      view_elevation = 0,
      plot = TRUE
    )
  )
})
