test_that("pitch2d.w.error validates pitch2d input", {
  expect_error(
    pitch2d.w.error("a", label_error = 1),
    "`pitch2d` must be a finite numeric scalar."
  )
  expect_error(
    pitch2d.w.error(c(1, 2), label_error = 1),
    "`pitch2d` must be a finite numeric scalar."
  )
  expect_error(
    pitch2d.w.error(NA_real_, label_error = 1),
    "`pitch2d` must be a finite numeric scalar."
  )
  expect_error(
    pitch2d.w.error(10, label_error = 1),
    "`pitch2d` must carry a valid `xy` attribute as returned by `pitch2d.from.xy\\(\\)`."
  )
})

test_that("pitch2d.w.error validates xy attribute", {
  p2d <- 10
  attr(p2d, "xy") <- list(x_tip = 1, y_tip = 2, x_base = 3)
  expect_error(
    pitch2d.w.error(p2d, label_error = 1),
    "`pitch2d` must carry a valid `xy` attribute as returned by `pitch2d.from.xy\\(\\)`."
  )

  p2d <- 10
  attr(p2d, "xy") <- list(x_tip = 1, y_tip = 2, x_base = 3, y_base = "4")
  expect_error(
    pitch2d.w.error(p2d, label_error = 1),
    "`pitch2d` must carry a valid `xy` attribute as returned by `pitch2d.from.xy\\(\\)`."
  )
})

test_that("pitch2d.w.error validates label_error", {
  p2d <- pitch2d.from.xy(1, 0, 0, 0)

  expect_error(
    pitch2d.w.error(p2d, label_error = "a"),
    "`label_error` must be a positive numeric scalar."
  )
  expect_error(
    pitch2d.w.error(p2d, label_error = c(1, 2)),
    "`label_error` must be a positive numeric scalar."
  )
  expect_error(
    pitch2d.w.error(p2d, label_error = NA_real_),
    "`label_error` must be a positive numeric scalar."
  )
  expect_error(
    pitch2d.w.error(p2d, label_error = 0),
    "`label_error` must be a positive numeric scalar."
  )
  expect_error(
    pitch2d.w.error(p2d, label_error = -1),
    "`label_error` must be a positive numeric scalar."
  )
})

test_that("pitch2d.w.error validates label_nsamp", {
  p2d <- pitch2d.from.xy(1, 0, 0, 0)

  expect_error(
    pitch2d.w.error(p2d, label_error = 1, label_nsamp = "a"),
    "`label_nsamp` must be a positive integer scalar."
  )
  expect_error(
    pitch2d.w.error(p2d, label_error = 1, label_nsamp = c(1, 2)),
    "`label_nsamp` must be a positive integer scalar."
  )
  expect_error(
    pitch2d.w.error(p2d, label_error = 1, label_nsamp = NA_real_),
    "`label_nsamp` must be a positive integer scalar."
  )
  expect_error(
    pitch2d.w.error(p2d, label_error = 1, label_nsamp = 0),
    "`label_nsamp` must be a positive integer scalar."
  )
  expect_error(
    pitch2d.w.error(p2d, label_error = 1, label_nsamp = 1.5),
    "`label_nsamp` must be a positive integer scalar."
  )
})

test_that("pitch2d.w.error validates add_boundaries", {
  p2d <- pitch2d.from.xy(1, 0, 0, 0)

  expect_error(
    pitch2d.w.error(p2d, label_error = 1, add_boundaries = NA),
    "`add_boundaries` must be a logical scalar."
  )
  expect_error(
    pitch2d.w.error(p2d, label_error = 1, add_boundaries = c(TRUE, FALSE)),
    "`add_boundaries` must be a logical scalar."
  )
  expect_error(
    pitch2d.w.error(p2d, label_error = 1, add_boundaries = 1),
    "`add_boundaries` must be a logical scalar."
  )
})

test_that("pitch2d.w.error returns sorted unique simulated pitches", {
  p2d <- pitch2d.from.xy(10, 5, 0, 0)
  res <- pitch2d.w.error(p2d, label_error = 1, label_nsamp = 16)

  expect_type(res, "double")
  expect_true(length(res) > 0)
  expect_equal(res, sort(unique(res)))
  expect_true(all(res > -180 & res <= 180))
})

test_that("pitch2d.w.error uses ceiling fourth-root grid size", {
  p2d <- pitch2d.from.xy(10, 5, 0, 0)

  res_1 <- pitch2d.w.error(p2d, label_error = 1, label_nsamp = 1)
  expect_length(res_1, 1)

  res_16 <- pitch2d.w.error(p2d, label_error = 1, label_nsamp = 16)
  expect_lte(length(res_16), 16)
  expect_true(length(res_16) >= length(res_1))

  res_17 <- pitch2d.w.error(p2d, label_error = 1, label_nsamp = 17)
  expect_lte(length(res_17), 81)
  expect_true(length(res_17) >= length(res_16))
})

test_that("pitch2d.w.error with one grid point returns original pitch", {
  p2d <- pitch2d.from.xy(10, 5, 0, 0)
  res <- pitch2d.w.error(p2d, label_error = 1, label_nsamp = 1)

  expect_equal(res, as.numeric(p2d))
})

test_that("pitch2d.w.error can add non-wrapping boundary angles", {
  p2d <- pitch2d.from.xy(10, 0, 0, 0)

  without_boundaries <- pitch2d.w.error(
    p2d,
    label_error = 2,
    label_nsamp = 16,
    add_boundaries = FALSE
  )
  with_boundaries <- pitch2d.w.error(
    p2d,
    label_error = 2,
    label_nsamp = 16,
    add_boundaries = TRUE
  )

  expect_true(0 %in% with_boundaries)
  expect_true(all(without_boundaries %in% with_boundaries))
  expect_equal(with_boundaries, sort(unique(with_boundaries)))
})

test_that("pitch2d.w.error can add wrapping boundary angle 180", {
  p2d <- pitch2d.from.xy(-10, 0, 0, 0)

  with_boundaries <- pitch2d.w.error(
    p2d,
    label_error = 2,
    label_nsamp = 16,
    add_boundaries = TRUE
  )

  expect_true(180 %in% with_boundaries)
  expect_equal(with_boundaries, sort(unique(with_boundaries)))
  expect_true(all(with_boundaries > -180 & with_boundaries <= 180))
})

