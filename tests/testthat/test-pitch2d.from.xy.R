test_that("pitch2d_from_xy works", {
  expect_equal(pitch2d.from.xy(1, 0, 0, 0), 0)
  expect_equal(pitch2d.from.xy(1, 1, 0, 0), 45)
  expect_equal(pitch2d.from.xy(-1, 0, 0, 0), 180)
  expect_equal(pitch2d.from.xy(0, 1, 0, 0), 90)
  expect_equal(pitch2d.from.xy(0, -1, 0, 0), -90)
  expect_equal(pitch2d.from.xy(-1, -1, 0, 0), -135)
})
