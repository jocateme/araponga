test_that("find.3d returns compatible orientations from direct pitch2d constraints", {
  observed <- find.3d(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = c(20, 30, 40),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_equal(
    observed,
    data.frame(
      pitch = 30,
      yaw = 0,
      view_elevation = 0
    )
  )
})




test_that("scalar direct pitch2d constraints include an exact matching projection", {
  observed <- find.3d(
    pitch2d = 30,
    candidate_pitches = c(20, 30, 40),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_equal(observed$pitch, 30)
})

test_that("find.3d returns only requested columns in requested order", {
  observed <- find.3d(
    pitch2d = c(29.9, 30.1),
    find = c("yaw", "pitch"),
    candidate_pitches = c(20, 30, 40),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_named(observed, c("yaw", "pitch"))
  expect_equal(observed, data.frame(yaw = 0, pitch = 30))
})


test_that("find.3d removes duplicate find values", {
  observed <- find.3d(
    pitch2d = c(29.9, 30.1),
    find = c("pitch", "pitch"),
    candidate_pitches = 30,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_named(observed, "pitch")
  expect_equal(observed$pitch, 30)
})


test_that("find.3d correctly handles pitch2d intervals crossing 180", {
  observed <- find.3d(
    pitch2d = c(178.9, -178.9),
    find = "pitch",
    candidate_pitches = c(-2, -1, 1, 2),
    candidate_yaws = 180,
    candidate_view_elevations = 0
  )
  
  expect_equal(sort(observed$pitch), c(-1, 1))
})


test_that("find.3d excludes undefined projected pitches when pitch is constrained", {
  observed <- find.3d(
    pitch2d = c(-0.1, 0.1),
    candidate_pitches = 0,
    candidate_yaws = c(0, 90),
    candidate_view_elevations = 0
  )
  
  expect_equal(
    observed,
    data.frame(
      pitch = 0,
      yaw = 0,
      view_elevation = 0
    )
  )
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
    data.frame(pitch = 30, yaw = 0, view_elevation = 0)
  )
})


test_that("find.3d returns a zero-row data frame when no combination is compatible", {
  observed <- find.3d(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = c(-30, 0),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_s3_class(observed, "data.frame")
  expect_equal(nrow(observed), 0)
  expect_named(observed, c("pitch", "yaw", "view_elevation"))
})


test_that("find.3d validates find", {
  args <- list(
    pitch2d = 0,
    candidate_pitches = 0,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(find.3d, c(args, list(find = numeric(0)))),
    "`find` must be a character vector"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(find = NA_character_))),
    "`find` must be a character vector"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(find = "bad"))),
    "invalid value"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(find = c("all", "pitch")))),
    "cannot be combined"
  )
})


test_that("find.3d validates default_step", {
  args <- list(
    pitch2d = 0,
    candidate_pitches = 0,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(find.3d, c(args, list(default_step = 0))),
    "`default_step`"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(default_step = Inf))),
    "`default_step`"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(default_step = c(1, 2)))),
    "`default_step`"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(default_step = 7))),
    "evenly divide 180"
  )
  
  expect_no_error(
    find.3d(
      pitch2d = 0,
      default_step = 90
    )
  )
})


test_that("find.3d validates candidate pitch sets", {
  args <- list(
    pitch2d = 0,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_pitches = numeric(0)))),
    "candidate_pitches"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_pitches = "0"))),
    "candidate_pitches"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_pitches = c(0, NA)))),
    "candidate_pitches"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_pitches = -180))),
    "`candidate_pitches` must satisfy"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_pitches = 180.1))),
    "`candidate_pitches` must satisfy"
  )
  
  expect_no_error(
    do.call(find.3d, c(args, list(candidate_pitches = c(-179.9, 180))))
  )
})


test_that("find.3d validates candidate yaw sets", {
  args <- list(
    pitch2d = 0,
    candidate_pitches = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_yaws = numeric(0)))),
    "candidate_yaws"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_yaws = c(0, Inf)))),
    "candidate_yaws"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_yaws = -180))),
    "`candidate_yaws` must satisfy"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_yaws = 180.1))),
    "`candidate_yaws` must satisfy"
  )
  
  expect_no_error(
    do.call(find.3d, c(args, list(candidate_yaws = c(-179.9, 180))))
  )
})


test_that("find.3d validates candidate view-elevation sets", {
  args <- list(
    pitch2d = 0,
    candidate_pitches = 0,
    candidate_yaws = 0
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_view_elevations = numeric(0)))),
    "candidate_view_elevations"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_view_elevations = c(0, NA)))),
    "candidate_view_elevations"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_view_elevations = -90.1))),
    "`candidate_view_elevations` must satisfy"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(candidate_view_elevations = 90.1))),
    "`candidate_view_elevations` must satisfy"
  )
  
  expect_no_error(
    do.call(find.3d, c(args, list(candidate_view_elevations = c(-90, 90))))
  )
})


test_that("find.3d validates max_combinations", {
  args <- list(
    pitch2d = 0,
    candidate_pitches = 0,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(find.3d, c(args, list(max_combinations = 0))),
    "positive numeric scalar"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(max_combinations = NA_real_))),
    "positive numeric scalar"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(max_combinations = c(1, 2)))),
    "positive numeric scalar"
  )
  
  expect_no_error(
    do.call(find.3d, c(args, list(max_combinations = Inf)))
  )
})


test_that("find.3d enforces max_combinations after removing duplicate candidates", {
  expect_error(
    find.3d(
      pitch2d = 0,
      candidate_pitches = c(-10, 0, 10),
      candidate_yaws = c(-20, 0, 20),
      candidate_view_elevations = c(0, 10),
      max_combinations = 17
    ),
    "18 combinations"
  )
  
  expect_no_error(
    find.3d(
      pitch2d = 0,
      candidate_pitches = c(0, 0),
      candidate_yaws = c(0, 0),
      candidate_view_elevations = c(0, 0),
      max_combinations = 1
    )
  )
})


test_that("find.3d requires exactly one observational input route", {
  observed2d <- project2d.from.xy(1, 0, 0, 0)
  
  expect_error(
    find.3d(
      candidate_pitches = 0,
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    "Supply either `observed2d`"
  )
  
  expect_error(
    find.3d(
      observed2d = observed2d,
      label_error = 0,
      pitch2d = 0,
      candidate_pitches = 0,
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    "not both"
  )
})


test_that("find.3d validates observed2d", {
  args <- list(
    label_error = 0,
    candidate_pitches = 0,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(find.3d, c(args, list(observed2d = data.frame(x = 1)))),
    "`observed2d` must be an `araponga2d` object"
  )
  
  two_rows <- project2d.from.xy(c(1, 2), c(0, 0), 0, 0)
  expect_error(
    do.call(find.3d, c(args, list(observed2d = two_rows))),
    "exactly one observation"
  )
  
  missing_column <- project2d.from.xy(1, 0, 0, 0)
  missing_column$x_tip <- NULL
  expect_error(
    do.call(find.3d, c(args, list(observed2d = missing_column))),
    "must contain `x_tip`"
  )
  
  bad_coordinate <- project2d.from.xy(1, 0, 0, 0)
  bad_coordinate$x_tip <- Inf
  expect_error(
    do.call(find.3d, c(args, list(observed2d = bad_coordinate))),
    "finite numeric values"
  )
})


test_that("find.3d validates label_error and allows zero", {
  observed2d <- project2d.from.xy(1, 0, 0, 0)
  
  args <- list(
    observed2d = observed2d,
    candidate_pitches = 0,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(find.3d, args),
    "`label_error` is required"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(label_error = -1))),
    "non-negative finite numeric scalar"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(label_error = Inf))),
    "non-negative finite numeric scalar"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(label_error = c(0, 1)))),
    "non-negative finite numeric scalar"
  )
  
  expect_no_error(
    do.call(find.3d, c(args, list(label_error = 0)))
  )
  
  expect_error(
    find.3d(
      pitch2d = 0,
      label_error = 1,
      candidate_pitches = 0,
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    "can only be used with `observed2d`"
  )
})


test_that("find.3d validates direct pitch2d", {
  args <- list(
    candidate_pitches = 0,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(find.3d, c(args, list(pitch2d = numeric(0)))),
    "non-empty finite numeric vector"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(pitch2d = "0"))),
    "non-empty finite numeric vector"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(pitch2d = c(0, NA)))),
    "non-empty finite numeric vector"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(pitch2d = -180))),
    "-180 < value <= 180"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(pitch2d = 180.1))),
    "-180 < value <= 180"
  )
  
  expect_no_error(
    do.call(find.3d, c(args, list(pitch2d = 0)))
  )
})


test_that("find.3d validates direct length2d and full_length", {
  args <- list(
    candidate_pitches = 0,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_error(
    do.call(find.3d, c(args, list(length2d = numeric(0), full_length = 1))),
    "`length2d`"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(length2d = -1, full_length = 1))),
    "non-negative finite"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(length2d = Inf, full_length = 1))),
    "non-negative finite"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(length2d = 1))),
    "`full_length` is required"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(pitch2d = 0, full_length = 1))),
    "can only constrain direct inputs when `length2d` is supplied"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(length2d = 1, full_length = 0))),
    "positive finite"
  )
  
  expect_error(
    do.call(find.3d, c(args, list(length2d = 1, full_length = c(1, Inf)))),
    "positive finite"
  )
  
  expect_no_error(
    do.call(find.3d, c(args, list(length2d = 0, full_length = 1)))
  )
})


test_that("direct length2d constraints use projection factor", {
  observed <- find.3d(
    length2d = c(49, 51),
    full_length = 100,
    candidate_pitches = 0,
    candidate_yaws = c(0, 60, 90),
    candidate_view_elevations = 0
  )
  
  expect_equal(
    observed,
    data.frame(pitch = 0, yaw = 60, view_elevation = 0)
  )
})


test_that("full_length vectors define a continuous range for direct length constraints", {
  observed <- find.3d(
    length2d = 52,
    full_length = c(110, 90, 100),
    candidate_pitches = 0,
    candidate_yaws = c(0, 60, 90),
    candidate_view_elevations = 0
  )
  
  expect_equal(
    observed,
    data.frame(pitch = 0, yaw = 60, view_elevation = 0)
  )
})


test_that("direct pitch2d and length2d are applied as independent constraints", {
  observed <- find.3d(
    pitch2d = 0,
    length2d = 50,
    full_length = 100,
    candidate_pitches = 0,
    candidate_yaws = c(0, 60, 120, 180),
    candidate_view_elevations = 0
  )
  
  expect_equal(
    observed,
    data.frame(pitch = 0, yaw = 60, view_elevation = 0)
  )
})


test_that("observed2d route with zero labeling error reproduces exact pitch", {
  observed2d <- project2d.from.xy(10, 0, 0, 0)
  
  observed <- find.3d(
    observed2d = observed2d,
    label_error = 0,
    candidate_pitches = c(-10, 0, 10),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_equal(
    observed,
    data.frame(pitch = 0, yaw = 0, view_elevation = 0)
  )
})


test_that("observed2d route propagates landmark-labeling uncertainty", {
  observed2d <- project2d.from.xy(10, 0, 0, 0)
  
  observed <- find.3d(
    observed2d = observed2d,
    label_error = 1,
    candidate_pitches = c(-20, -10, 0, 10, 20),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_equal(sort(observed$pitch), c(-10, 0, 10))
})


test_that("observed2d plus full_length jointly constrain projected components", {
  observed2d <- project2d.from.xy(50, 0, 0, 0)
  
  observed <- find.3d(
    observed2d = observed2d,
    label_error = 0,
    full_length = 100,
    candidate_pitches = 0,
    candidate_yaws = c(0, 60, 90),
    candidate_view_elevations = 0
  )
  
  expect_equal(
    observed,
    data.frame(pitch = 0, yaw = 60, view_elevation = 0)
  )
})


test_that("observed2d plus full_length requires a common length for dx and dy", {
  # The candidate segment lies on dx = dy. Its dx range overlaps [1, 3]
  # and its dy range overlaps [7, 9], but never at the same full_length.
  observed2d <- project2d.from.xy(2, 8, 0, 0)
  
  observed <- find.3d(
    observed2d = observed2d,
    label_error = 0.5,
    full_length = c(1, 14),
    candidate_pitches = 45,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_equal(nrow(observed), 0)
})


test_that("observed2d plus full_length retains intersecting predicted segments", {
  observed2d <- project2d.from.xy(5, 5, 0, 0)
  
  observed <- find.3d(
    observed2d = observed2d,
    label_error = 0.5,
    full_length = c(1, 14),
    candidate_pitches = 45,
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_equal(
    observed,
    data.frame(pitch = 45, yaw = 0, view_elevation = 0)
  )
})


test_that("segment-rectangle helper detects ordinary intersections", {
  expect_true(
    araponga:::.pred.segment.intersects.obs.rectangle(
      pred_dx_factor = 0.8,
      pred_dy_factor = 0.4,
      obs_full_length_range = c(10, 20),
      obs_dx_range = c(10, 14),
      obs_dy_range = c(5, 8)
    )
  )
})


test_that("segment-rectangle helper requires a common full_length", {
  expect_false(
    araponga:::.pred.segment.intersects.obs.rectangle(
      pred_dx_factor = 1,
      pred_dy_factor = 1,
      obs_full_length_range = c(1, 10),
      obs_dx_range = c(1, 3),
      obs_dy_range = c(7, 9)
    )
  )
})


test_that("segment-rectangle helper handles negative component factors", {
  expect_true(
    araponga:::.pred.segment.intersects.obs.rectangle(
      pred_dx_factor = -0.8,
      pred_dy_factor = 0.4,
      obs_full_length_range = c(10, 20),
      obs_dx_range = c(-14, -10),
      obs_dy_range = c(5, 8)
    )
  )
})


test_that("segment-rectangle helper handles zero dx_factor", {
  expect_true(
    araponga:::.pred.segment.intersects.obs.rectangle(
      pred_dx_factor = 0,
      pred_dy_factor = 0.5,
      obs_full_length_range = c(10, 20),
      obs_dx_range = c(-2, 2),
      obs_dy_range = c(6, 9)
    )
  )
  
  expect_false(
    araponga:::.pred.segment.intersects.obs.rectangle(
      pred_dx_factor = 0,
      pred_dy_factor = 0.5,
      obs_full_length_range = c(10, 20),
      obs_dx_range = c(2, 4),
      obs_dy_range = c(6, 9)
    )
  )
})


test_that("segment-rectangle helper handles zero dy_factor", {
  expect_true(
    araponga:::.pred.segment.intersects.obs.rectangle(
      pred_dx_factor = 0.5,
      pred_dy_factor = 0,
      obs_full_length_range = c(10, 20),
      obs_dx_range = c(6, 9),
      obs_dy_range = c(-1, 1)
    )
  )
  
  expect_false(
    araponga:::.pred.segment.intersects.obs.rectangle(
      pred_dx_factor = 0.5,
      pred_dy_factor = 0,
      obs_full_length_range = c(10, 20),
      obs_dx_range = c(6, 9),
      obs_dy_range = c(1, 2)
    )
  )
})


test_that("segment-rectangle helper handles a segment collapsed at the origin", {
  expect_true(
    araponga:::.pred.segment.intersects.obs.rectangle(
      pred_dx_factor = 0,
      pred_dy_factor = 0,
      obs_full_length_range = c(10, 20),
      obs_dx_range = c(-1, 1),
      obs_dy_range = c(-1, 1)
    )
  )
  
  expect_false(
    araponga:::.pred.segment.intersects.obs.rectangle(
      pred_dx_factor = 0,
      pred_dy_factor = 0,
      obs_full_length_range = c(10, 20),
      obs_dx_range = c(1, 2),
      obs_dy_range = c(-1, 1)
    )
  )
})


test_that("segment-rectangle helper treats boundary contact as intersection", {
  expect_true(
    araponga:::.pred.segment.intersects.obs.rectangle(
      pred_dx_factor = 1,
      pred_dy_factor = 0.5,
      obs_full_length_range = c(10, 20),
      obs_dx_range = c(20, 25),
      obs_dy_range = c(10, 12)
    )
  )
})


test_that("segment-rectangle helper is vectorized over predicted factors", {
  observed <- araponga:::.pred.segment.intersects.obs.rectangle(
    pred_dx_factor = c(0.8, 1),
    pred_dy_factor = c(0.4, 1),
    obs_full_length_range = c(10, 20),
    obs_dx_range = c(10, 14),
    obs_dy_range = c(5, 8)
  )
  
  expect_equal(observed, c(TRUE, FALSE))
})


test_that("segment-rectangle helper plotting does not alter its result", {
  expected <- araponga:::.pred.segment.intersects.obs.rectangle(
    pred_dx_factor = 0.8,
    pred_dy_factor = 0.4,
    obs_full_length_range = c(10, 20),
    obs_dx_range = c(10, 14),
    obs_dy_range = c(5, 8)
  )
  
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  observed <- araponga:::.pred.segment.intersects.obs.rectangle(
    pred_dx_factor = 0.8,
    pred_dy_factor = 0.4,
    obs_full_length_range = c(10, 20),
    obs_dx_range = c(10, 14),
    obs_dy_range = c(5, 8),
    plot = TRUE
  )
  
  expect_identical(observed, expected)
})


test_that("find.3d works regardless of which candidate angle is chunked", {
  cases <- list(
    list(
      pitch = c(0, 10),
      yaw = c(-20, 0, 20),
      elevation = 0
    ),
    list(
      pitch = c(-10, 0, 10),
      yaw = c(0, 20),
      elevation = 0
    ),
    list(
      pitch = c(0, 10),
      yaw = 0,
      elevation = c(-20, 0, 20)
    )
  )
  
  for(x in cases){
    observed <- find.3d(
      length2d = c(0, 1),
      full_length = 1,
      candidate_pitches = x$pitch,
      candidate_yaws = x$yaw,
      candidate_view_elevations = x$elevation
    )
    
    expect_equal(
      nrow(observed),
      length(unique(x$pitch)) *
        length(unique(x$yaw)) *
        length(unique(x$elevation))
    )
    
    expect_equal(nrow(unique(observed)), nrow(observed))
  }
})


test_that("find.3d supports extended candidate pitches", {
  projected <- project2d.from.3d(
    pitch = 135,
    yaw = 10,
    view_elevation = 20
  )
  
  observed <- find.3d(
    pitch2d = projected$pitch2d,
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


test_that("find.pitch returns sorted unique compatible pitches", {
  observed <- find.pitch(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = c(40, 30, 20, 30),
    candidate_yaws = 0,
    candidate_view_elevations = 0
  )
  
  expect_equal(observed, 30)
})


test_that("find.yaw returns sorted unique compatible yaws", {
  observed <- find.yaw(
    pitch2d = c(29.9, 30.1),
    candidate_pitches = 30,
    candidate_yaws = c(20, 0, -20, 0),
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
    data.frame(yaw = 0, pitch = 30)
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
    data.frame(pitch = 30, yaw = 0)
  )
})


test_that("find.pitch and find.yaw pass landmark inputs through to find.3d", {
  observed2d <- project2d.from.xy(50, 0, 0, 0)
  
  expect_equal(
    find.yaw(
      observed2d = observed2d,
      label_error = 0,
      full_length = 100,
      candidate_pitches = 0,
      candidate_yaws = c(0, 60, 90),
      candidate_view_elevations = 0
    ),
    60
  )
  
  expect_equal(
    find.pitch(
      observed2d = project2d.from.xy(10, 0, 0, 0),
      label_error = 0,
      candidate_pitches = c(-10, 0, 10),
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    0
  )
})


test_that("find.pitch and find.yaw validate paired", {
  expect_error(
    find.pitch(
      pitch2d = 0,
      paired = NA,
      candidate_pitches = 0,
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    "`paired` must be a logical scalar"
  )
  
  expect_error(
    find.yaw(
      pitch2d = 0,
      paired = c(TRUE, FALSE),
      candidate_pitches = 0,
      candidate_yaws = 0,
      candidate_view_elevations = 0
    ),
    "`paired` must be a logical scalar"
  )
})