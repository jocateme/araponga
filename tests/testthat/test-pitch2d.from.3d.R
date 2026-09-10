test_that("pitch2d.from.3d returns expected values for simple orientations", {
  
  # with zero yaw and view elevation, projected pitch equals 3D pitch
  pitch <- c(-179, -150, -90, -30, 0, 30, 90, 150, 180)
  
  expect_equal(
    pitch2d.from.3d(
      pitch = pitch,
      yaw = rep(0, length(pitch)),
      view_elevation = rep(0, length(pitch))
    ),
    pitch,
    tolerance = 1e-12
  )
  
  # horizontal object pointing left
  expect_equal(
    pitch2d.from.3d(
      pitch = 0,
      yaw = 180,
      view_elevation = 0
    ),
    180,
    tolerance = 1e-12
  )
})

test_that("pitch2d.from.3d is vectorized elementwise", {
  
  pitch <- c(-60, -30, 0, 25, 70)
  yaw <- c(-120, -45, 0, 60, 150)
  view_elevation <- c(-40, -10, 20, 35, 50)
  
  vectorized <- pitch2d.from.3d(
    pitch = pitch,
    yaw = yaw,
    view_elevation = view_elevation
  )
  
  scalar <- vapply(
    seq_along(pitch),
    function(i) {
      pitch2d.from.3d(
        pitch = pitch[i],
        yaw = yaw[i],
        view_elevation = view_elevation[i]
      )
    },
    numeric(1)
  )
  
  expect_equal(vectorized, scalar, tolerance = 1e-12)
  expect_length(vectorized, length(pitch))
})

test_that("pitch2d.from.3d agrees with rotate3d", {
  
  pitch <- c(-170, -135, -35, 20, 135, 180)
  yaw <- c(-140, -80, -25, 40, 100, 160)
  view_elevation <- c(-50, -20, 10, 25, 45, 70)
  
  observed <- pitch2d.from.3d(
    pitch = pitch,
    yaw = yaw,
    view_elevation = view_elevation
  )
  
  expected <- vapply(
    seq_along(pitch),
    function(i) {
      
      R <- rotate3d(
        pitch = pitch[i],
        yaw = yaw[i],
        roll = view_elevation[i]
      )
      
      out <- rad2deg(atan2(R[2, 1], R[1, 1]))
      
      if (out == -180) {
        out <- 180
      }
      
      out
    },
    numeric(1)
  )
  
  expect_equal(observed, expected, tolerance = 1e-12)
})

test_that("pitch2d.from.3d returns NA for zero-length projections", {
  
  expect_true(
    is.na(
      pitch2d.from.3d(
        pitch = 0,
        yaw = 90,
        view_elevation = 0
      )
    )
  )
  
  expect_true(
    is.na(
      pitch2d.from.3d(
        pitch = 0,
        yaw = -90,
        view_elevation = 0
      )
    )
  )
  
  # degeneracy is handled independently for each orientation
  observed <- pitch2d.from.3d(
    pitch = c(0, 0),
    yaw = c(90, 0),
    view_elevation = c(0, 0)
  )
  
  expect_equal(
    observed,
    c(NA_real_, 0)
  )
})

test_that("defined pitch2d.from.3d outputs are finite and in (-180, 180]", {
  
  grid <- expand.grid(
    pitch = seq(-150, 180, by = 30),
    yaw = seq(-150, 180, by = 30),
    view_elevation = seq(-90, 90, by = 30),
    KEEP.OUT.ATTRS = FALSE
  )
  
  observed <- pitch2d.from.3d(
    pitch = grid$pitch,
    yaw = grid$yaw,
    view_elevation = grid$view_elevation
  )
  
  defined <- observed[!is.na(observed)]
  
  expect_true(all(is.finite(defined)))
  expect_true(all(defined > -180))
  expect_true(all(defined <= 180))
  
  expect_false(any(is.nan(observed)))
  expect_false(any(is.infinite(observed)))
})

test_that("pitch2d.from.3d requires all three angle arguments", {
  
  expect_error(
    pitch2d.from.3d(),
    "All angle arguments"
  )
  
  expect_error(
    pitch2d.from.3d(pitch = 0),
    "All angle arguments"
  )
  
  expect_error(
    pitch2d.from.3d(pitch = 0, yaw = 0),
    "All angle arguments"
  )
})

test_that("pitch2d.from.3d rejects empty and non-numeric angle arguments", {
  
  expect_error(
    pitch2d.from.3d(
      numeric(0),
      numeric(0),
      numeric(0)
    ),
    "empty angle"
  )
  
  expect_error(
    pitch2d.from.3d(
      "0",
      0,
      0
    ),
    "must be numeric"
  )
  
  expect_error(
    pitch2d.from.3d(
      0,
      "0",
      0
    ),
    "must be numeric"
  )
  
  expect_error(
    pitch2d.from.3d(
      0,
      0,
      "0"
    ),
    "must be numeric"
  )
})

test_that("pitch2d.from.3d requires equal-length angle vectors", {
  
  expect_error(
    pitch2d.from.3d(
      pitch = c(0, 10),
      yaw = c(0, 10, 20),
      view_elevation = c(0, 10)
    ),
    "must have the same length"
  )
  
  # scalar recycling is intentionally not supported
  expect_error(
    pitch2d.from.3d(
      pitch = c(0, 10),
      yaw = 0,
      view_elevation = c(0, 10)
    ),
    "must have the same length"
  )
})

test_that("pitch2d.from.3d requires finite angle values", {
  
  expect_error(
    pitch2d.from.3d(
      c(0, NA),
      c(0, 0),
      c(0, 0)
    ),
    "finite"
  )
  
  expect_error(
    pitch2d.from.3d(
      c(0, 0),
      c(0, Inf),
      c(0, 0)
    ),
    "finite"
  )
  
  expect_error(
    pitch2d.from.3d(
      c(0, 0),
      c(0, 0),
      c(0, NaN)
    ),
    "finite"
  )
})

test_that("pitch2d.from.3d validates angle ranges", {
  
  # endpoints are valid
  expect_no_error(
    pitch2d.from.3d(
      pitch = c(-179.9, 180),
      yaw = c(-179.9, 180),
      view_elevation = c(-90, 90)
    )
  )
  
  expect_error(
    pitch2d.from.3d(
      pitch = -180,
      yaw = 0,
      view_elevation = 0
    ),
    "`pitch` must satisfy"
  )
  
  expect_error(
    pitch2d.from.3d(
      pitch = 180.1,
      yaw = 0,
      view_elevation = 0
    ),
    "`pitch` must satisfy"
  )
  
  expect_error(
    pitch2d.from.3d(0, -180, 0),
    "`yaw` must satisfy"
  )
  
  expect_error(
    pitch2d.from.3d(0, 180.1, 0),
    "`yaw` must satisfy"
  )
  
  expect_error(
    pitch2d.from.3d(0, 0, -90.1),
    "`view_elevation` must satisfy"
  )
  
  expect_error(
    pitch2d.from.3d(0, 0, 90.1),
    "`view_elevation` must satisfy"
  )
})

test_that("pitch2d.from.3d validates plot", {
  
  expect_error(
    pitch2d.from.3d(0, 0, 0, plot = NA),
    "`plot` must be a logical scalar"
  )
  
  expect_error(
    pitch2d.from.3d(0, 0, 0, plot = 1),
    "`plot` must be a logical scalar"
  )
  
  expect_error(
    pitch2d.from.3d(
      0, 0, 0,
      plot = c(TRUE, FALSE)
    ),
    "`plot` must be a logical scalar"
  )
})

test_that("plotting is disabled for vectorized input", {
  
  expected <- pitch2d.from.3d(
    pitch = c(0, 30),
    yaw = c(0, 0),
    view_elevation = c(0, 0)
  )
  
  expect_warning(
    observed <- pitch2d.from.3d(
      pitch = c(0, 30),
      yaw = c(0, 0),
      view_elevation = c(0, 0),
      plot = TRUE
    ),
    "plot = FALSE"
  )
  
  expect_equal(observed, expected)
})

test_that("scalar plotting handles defined and degenerate projections", {
  
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  expect_silent(
    pitch2d.from.3d(
      pitch = 15,
      yaw = -30,
      view_elevation = -20,
      plot = TRUE
    )
  )
  
  expect_silent(
    pitch2d.from.3d(
      pitch = 0,
      yaw = 90,
      view_elevation = 0,
      plot = TRUE
    )
  )
})

test_that("extended pitches have equivalent canonical representations", {
  
  extended <- pitch2d.from.3d(
    pitch = c(135, -135),
    yaw = c(10, 10),
    view_elevation = c(20, 20)
  )
  
  canonical <- pitch2d.from.3d(
    pitch = c(45, -45),
    yaw = c(-170, -170),
    view_elevation = c(20, 20)
  )
  
  expect_equal(
    extended,
    canonical,
    tolerance = 1e-12
  )
})