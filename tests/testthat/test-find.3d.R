test_that("find.3d returns compatible orientations", {
  
  observed <- find.3d(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = c(20, 30, 40),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expected <- data.frame(
    pitch = 30,
    yaw = 0,
    view_elevation = 0,
    pitch2d = 30
  )
  
  expect_equal(
    observed,
    expected,
    tolerance = 1e-12
  )
})

test_that("find.3d returns only requested columns", {
  
  observed <- find.3d(
    pitch2d = c(29.9, 30.1),
    find = c("yaw", "pitch"),
    candidate_pitches = c(20, 30, 40),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_named(
    observed,
    c("yaw", "pitch")
  )
  
  expect_equal(
    observed,
    data.frame(
      yaw = 0,
      pitch = 30
    )
  )
})

test_that("find.3d correctly handles pitch2d intervals crossing 180", {
  
  observed <- find.3d(
    pitch2d = c(179, -179),
    find = c("pitch", "pitch2d"),
    candidate_pitches = c(-2, -1, 1, 2),
    candidate_yaws = 180,
    candidate_view_elevations = 0
  )
  
  observed <- observed[order(observed$pitch), , drop = FALSE]
  rownames(observed) <- NULL
  
  expected <- data.frame(
    pitch = c(-1, 1),
    pitch2d = c(-179, 179)
  )
  
  expect_equal(
    observed,
    expected,
    tolerance = 1e-10
  )
  
})

test_that("find.3d excludes undefined projected pitches", {
  
  observed <- find.3d(
    pitch2d = c(-0.1, 0.1),
    candidate_pitches = 0,
    candidate_yaws = c(0, 90),
    candidate_view_elevations = 0
  )
  
  expect_equal(nrow(observed), 1)
  expect_equal(observed$pitch, 0)
  expect_equal(observed$yaw, 0)
  expect_equal(observed$view_elevation, 0)
  expect_equal(observed$pitch2d, 0)
  
  expect_true(all(is.finite(observed$pitch2d)))
})

test_that("find.3d removes duplicate candidate combinations", {
  
  observed <- find.3d(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = c(30, 30),
    candidate_yaws = c(0, 0),
    candidate_view_elevations = c(0, 0)
  )
  
  expect_equal(nrow(observed), 1)
  
  expect_equal(
    observed,
    data.frame(
      pitch = 30,
      yaw = 0,
      view_elevation = 0,
      pitch2d = 30
    )
  )
})

test_that("find.3d returns zero rows when no combination is compatible", {
  
  observed <- find.3d(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = c(-30, 0),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_s3_class(observed, "data.frame")
  expect_equal(nrow(observed), 0)
  
  expect_named(
    observed,
    c("pitch", "yaw", "view_elevation", "pitch2d")
  )
})

test_that("find.3d validates pitch2d", {
  
  expect_error(
    find.3d(),
    "`pitch2d` must be provided"
  )
  
  expect_error(
    find.3d(numeric(0)),
    "`pitch2d` must be provided"
  )
  
  expect_error(
    find.3d("0"),
    "finite numeric vector"
  )
  
  expect_error(
    find.3d(c(0, NA)),
    "finite numeric vector"
  )
  
  expect_error(
    find.3d(c(0, Inf)),
    "finite numeric vector"
  )
  
  expect_error(
    find.3d(-180),
    "-180 < value <= 180"
  )
  
  expect_error(
    find.3d(180.1),
    "-180 < value <= 180"
  )
})

test_that("find.3d validates find", {
  
  args <- list(
    pitch2d = c(-1, 1),
    candidate_pitches = 0,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(
      find.3d,
      c(args, list(find = numeric(0)))
    ),
    "`find` must be a character vector"
  )
  
  expect_error(
    do.call(
      find.3d,
      c(args, list(find = "bad"))
    ),
    "invalid value"
  )
  
  expect_error(
    do.call(
      find.3d,
      c(args, list(find = c("all", "pitch")))
    ),
    "cannot be combined"
  )
  
  # duplicate requested columns are removed
  observed <- do.call(
    find.3d,
    c(args, list(find = c("pitch", "pitch")))
  )
  
  expect_named(observed, "pitch")
})

test_that("find.3d validates candidate angle sets", {
  
  p2d <- c(-1, 1)
  
  expect_error(
    find.3d(
      p2d,
      candidate_pitches = numeric(0),
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    "candidate_pitches"
  )
  
  expect_error(
    find.3d(
      p2d,
      candidate_pitches = c(0, NA),
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    "candidate_pitches"
  )
  
  # extended pitches are accepted
  expect_no_error(
    find.3d(
      p2d,
      candidate_pitches = c(-179.9, 180),
      candidate_yaws = 0,
      candidate_view_elevations = 0
    )
  )
  
  # -180 is excluded
  expect_error(
    find.3d(
      p2d,
      candidate_pitches = -180,
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    "`candidate_pitches` must satisfy"
  )
  
  expect_error(
    find.3d(
      p2d,
      candidate_pitches = 180.1,
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    "`candidate_pitches` must satisfy"
  )
  
  expect_error(
    find.3d(
      p2d,
      candidate_pitches = 0,
      candidate_yaws = -180,
      candidate_view_elevations = 0
    ),
    "`candidate_yaws` must satisfy"
  )
  
  expect_error(
    find.3d(
      p2d,
      candidate_pitches = 0,
      candidate_yaws = 180.1,
      candidate_view_elevations = 0
    ),
    "`candidate_yaws` must satisfy"
  )
  
  expect_error(
    find.3d(
      p2d,
      candidate_pitches = 0,
      candidate_yaws = 0,
      candidate_view_elevations = -90.1
    ),
    "`candidate_view_elevations` must satisfy"
  )
  
  expect_error(
    find.3d(
      p2d,
      candidate_pitches = 0,
      candidate_yaws = 0,
      candidate_view_elevations = 90.1
    ),
    "`candidate_view_elevations` must satisfy"
  )
  
  # all included endpoints are accepted
  expect_no_error(
    find.3d(
      p2d,
      candidate_pitches = c(-179.9, 180),
      candidate_yaws = c(-179.9, 180),
      candidate_view_elevations = c(-90, 90)
    )
  )
})

test_that("find.3d validates default_step", {
  
  p2d <- c(-1, 1)
  
  expect_error(
    find.3d(p2d, default_step = 0),
    "`default_step`"
  )
  
  expect_error(
    find.3d(p2d, default_step = Inf),
    "`default_step`"
  )
  
  expect_error(
    find.3d(p2d, default_step = 7),
    "evenly divide 180"
  )
  
  expect_no_error(
    find.3d(
      p2d,
      default_step = 90
    )
  )
})

test_that("find.3d requires label_error for scalar pitch2d", {
  
  expect_error(
    find.3d(
      pitch2d = 30,
      candidate_pitches = 30,
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    "`label_error` is required"
  )
})

test_that("find.3d validates label_error", {
  
  args <- list(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = 30,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(
      find.3d,
      c(args, list(label_error = 0))
    ),
    "positive finite numeric scalar"
  )
  
  expect_error(
    do.call(
      find.3d,
      c(args, list(label_error = Inf))
    ),
    "positive finite numeric scalar"
  )
})

test_that("find.3d validates label_nsamp", {
  
  p2d <- c(-1, 1)
  
  expect_error(
    find.3d(
      p2d,
      label_nsamp = 0
    ),
    "positive integer scalar"
  )
  
  expect_error(
    find.3d(
      p2d,
      label_nsamp = 2.5
    ),
    "positive integer scalar"
  )
  
  expect_error(
    find.3d(
      p2d,
      label_nsamp = Inf
    ),
    "positive integer scalar"
  )
})

test_that("find.3d enforces max_combinations", {
  
  expect_error(
    find.3d(
      pitch2d = c(-1, 1),
      candidate_pitches = c(-10, 0, 10),
      candidate_yaws = c(-20, 0, 20),
      candidate_view_elevations = c(0, 10),
      max_combinations = 17
    ),
    "18 combinations"
  )
  
  expect_no_error(
    find.3d(
      pitch2d = c(-1, 1),
      candidate_pitches = c(-10, 0, 10),
      candidate_yaws = c(-20, 0, 20),
      candidate_view_elevations = c(0, 10),
      max_combinations = 18
    )
  )
  
  expect_no_error(
    find.3d(
      pitch2d = c(-1, 1),
      candidate_pitches = 0,
      candidate_yaws = 0,
      candidate_view_elevations = 0,
      max_combinations = Inf
    )
  )
})

test_that("find.pitch returns compatible pitches", {
  
  observed <- find.pitch(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = c(20, 30, 40),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_equal(observed, 30)
})

test_that("find.yaw returns compatible yaws", {
  
  observed <- find.yaw(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = 30,
    candidate_yaws = c(-20, 0, 20),
    candidate_view_elevations = 0
  )
  
  expect_equal(observed, 0)
})

test_that("find.pitch and find.yaw return paired results", {
  
  pitch_result <- find.pitch(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = c(20, 30, 40),
    candidate_yaws = 0,
    candidate_view_elevations = 0,
    paired = TRUE
  )
  
  expect_equal(
    pitch_result,
    data.frame(
      yaw = 0,
      pitch = 30
    )
  )
  
  yaw_result <- find.yaw(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = 30,
    candidate_yaws = c(-20, 0, 20),
    candidate_view_elevations = 0,
    paired = TRUE
  )
  
  expect_equal(
    yaw_result,
    data.frame(
      pitch = 30,
      yaw = 0
    )
  )
})

test_that("find.pitch and find.yaw validate paired", {
  
  expect_error(
    find.pitch(
      pitch2d = c(-1, 1),
      paired = NA
    ),
    "`paired` must be a logical scalar"
  )
  
  expect_error(
    find.yaw(
      pitch2d = c(-1, 1),
      paired = c(TRUE, FALSE)
    ),
    "`paired` must be a logical scalar"
  )
})

test_that("find.3d supports extended candidate pitches", {
  
  p2d <- pitch2d.from.3d(
    pitch = 135,
    yaw = 10,
    view_elevation = 20
  )
  
  observed <- find.3d(
    pitch2d = c(p2d - 1e-6, p2d + 1e-6),
    find = c("pitch", "yaw", "view_elevation"),
    candidate_pitches = c(45, 135),
    candidate_yaws = c(-170, 10),
    candidate_view_elevations = 20
  )
  
  expect_true(
    any(
      observed$pitch == 135 &
        observed$yaw == 10 &
        observed$view_elevation == 20
    )
  )
  
  expect_true(
    any(
      observed$pitch == 45 &
        observed$yaw == -170 &
        observed$view_elevation == 20
    )
  )
})

test_that("find.3d works when view elevation is the chunking angle", {
  
  observed <- find.3d(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = 30,
    candidate_yaws = 0,
    candidate_view_elevations = c(-20, -10, 0, 10, 20)
  )
  
  expect_true(
    any(
      observed$pitch == 30 &
        observed$yaw == 0 &
        observed$view_elevation == 0 &
        abs(observed$pitch2d - 30) < 1e-10
    )
  )
})

test_that("find.3d handles scalar pitch2d with labeling error", {
  
  p2d <- pitch2d.from.xy(10, 1, -12, 20)
  
  expect_no_error(
    find.3d(
      pitch2d = p2d,
      candidate_pitches = seq(-20, 20, by = 10),
      candidate_yaws = 0,
      candidate_view_elevations = 0,
      label_error = 2,
      label_nsamp = 25
    )
  )
})