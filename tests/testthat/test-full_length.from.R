test_that("full_length.from.camera calculates from focal length and sensor width", {
  
  out <- full_length.from.camera(
    length_physical = 10,
    distance = 1000,
    image_width = 2000,
    focal_length = 50,
    sensor_width = 20
  )
  
  expect_equal(out, c(50, 50))
})


test_that("camera parameterizations give equivalent results", {
  
  fov <- 2 * atan(20 / (2 * 50)) * 180 / pi
  
  from_focal <- full_length.from.camera(
    length_physical = 10,
    distance = 1000,
    image_width = 2000,
    focal_length = 50,
    sensor_width = 20
  )
  
  from_fov <- full_length.from.camera(
    length_physical = 10,
    distance = 1000,
    image_width = 2000,
    field_of_view = fov
  )
  
  expect_equal(from_fov, from_focal)
})


test_that("full_length.from.camera propagates input ranges", {
  
  out <- full_length.from.camera(
    length_physical = c(9, 11),
    distance = c(900, 1100),
    image_width = c(1900, 2100),
    focal_length = c(45, 55),
    sensor_width = c(18, 22)
  )
  
  expected <- c(
    9 * 1900 * (45 / 22) / 1100,
    11 * 2100 * (55 / 18) / 900
  )
  
  expect_equal(out, expected)
})


test_that("full_length.from.camera accepts unordered and multi-value ranges", {
  
  out1 <- full_length.from.camera(
    length_physical = c(11, 9, 10),
    distance = c(1100, 900, 1000),
    image_width = 2000,
    focal_length = c(55, 45, 50),
    sensor_width = 20
  )
  
  out2 <- full_length.from.camera(
    length_physical = c(9, 11),
    distance = c(900, 1100),
    image_width = 2000,
    focal_length = c(45, 55),
    sensor_width = 20
  )
  
  expect_equal(out1, out2)
})


test_that("full_length.from.camera requires one complete camera specification", {
  
  expect_error(
    full_length.from.camera(
      length_physical = 10,
      distance = 1000,
      image_width = 2000
    ),
    "Supply either field_of_view or both focal_length and sensor_width"
  )
  
  expect_error(
    full_length.from.camera(
      length_physical = 10,
      distance = 1000,
      image_width = 2000,
      focal_length = 50
    ),
    "Supply either field_of_view or both focal_length and sensor_width"
  )
  
  expect_error(
    full_length.from.camera(
      length_physical = 10,
      distance = 1000,
      image_width = 2000,
      field_of_view = 20,
      focal_length = 50,
      sensor_width = 20
    ),
    "not both"
  )
})


test_that("full_length.from.camera validates field of view", {
  
  expect_error(
    full_length.from.camera(
      length_physical = 10,
      distance = 1000,
      image_width = 2000,
      field_of_view = 180
    ),
    "less than 180 degrees"
  )
  
  expect_error(
    full_length.from.camera(
      length_physical = 10,
      distance = 1000,
      image_width = 2000,
      field_of_view = 0
    ),
    "field_of_view must contain finite, positive numeric values"
  )
})


test_that("full_length.from.reference calculates full length", {
  
  out <- full_length.from.reference(
    length_physical = 10,
    reference_physical = 20,
    reference_px = 100
  )
  
  expect_equal(out, c(50, 50))
})


test_that("full_length.from.reference propagates input ranges", {
  
  out <- full_length.from.reference(
    length_physical = c(9, 11),
    reference_physical = c(18, 22),
    reference_px = c(90, 110)
  )
  
  expected <- c(
    9 * 90 / 22,
    11 * 110 / 18
  )
  
  expect_equal(out, expected)
})


test_that("full-length functions reject invalid numeric inputs", {
  
  expect_error(
    full_length.from.reference(
      length_physical = 0,
      reference_physical = 20,
      reference_px = 100
    ),
    "length_physical must contain finite, positive numeric values"
  )
  
  expect_error(
    full_length.from.reference(
      length_physical = 10,
      reference_physical = NA_real_,
      reference_px = 100
    ),
    "reference_physical must contain finite, positive numeric values"
  )
  
  expect_error(
    full_length.from.camera(
      length_physical = 10,
      distance = Inf,
      image_width = 2000,
      focal_length = 50,
      sensor_width = 20
    ),
    "distance must contain finite, positive numeric values"
  )
})