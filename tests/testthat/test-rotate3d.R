expect_rotation_matrix <- function(x) {
  expect_type(x, "double")
  expect_equal(class(x), c("matrix", "array"))
  expect_equal(dim(x), c(3L, 3L))
}

test_that("Rz returns the identity matrix by default", {
  res <- Rz()

  expect_rotation_matrix(res)
  expect_equal(res, diag(3))
})

test_that("Ry returns the identity matrix by default", {
  res <- Ry()

  expect_rotation_matrix(res)
  expect_equal(res, diag(3))
})

test_that("Rx returns the identity matrix by default", {
  res <- Rx()

  expect_rotation_matrix(res)
  expect_equal(res, diag(3))
})

test_that("Rz returns expected rotations about the z axis", {
  expect_equal(
    Rz(90),
    matrix(
      c(0, -1, 0,
        1,  0, 0,
        0,  0, 1),
      nrow = 3,
      byrow = TRUE
    ),
    tolerance = 1e-12
  )

  expect_equal(
    as.vector(Rz(90) %*% c(1, 0, 0)),
    c(0, 1, 0),
    tolerance = 1e-12
  )
})

test_that("Ry returns expected rotations about the y axis", {
  expect_equal(
    Ry(90),
    matrix(
      c( 0, 0, 1,
         0, 1, 0,
        -1, 0, 0),
      nrow = 3,
      byrow = TRUE
    ),
    tolerance = 1e-12
  )

  expect_equal(
    as.vector(Ry(90) %*% c(1, 0, 0)),
    c(0, 0, -1),
    tolerance = 1e-12
  )
})

test_that("Rx returns expected rotations about the x axis", {
  expect_equal(
    Rx(90),
    matrix(
      c(1, 0,  0,
        0, 0, -1,
        0, 1,  0),
      nrow = 3,
      byrow = TRUE
    ),
    tolerance = 1e-12
  )

  expect_equal(
    as.vector(Rx(90) %*% c(0, 1, 0)),
    c(0, 0, 1),
    tolerance = 1e-12
  )
})

test_that("rotate3d returns identity when all rotations are zero", {
  res <- rotate3d()

  expect_rotation_matrix(res)
  expect_equal(res, diag(3))
})

test_that("rotate3d composes rotations in Rx %*% Ry %*% Rz order", {
  pitch <- 45
  yaw <- 30
  roll <- -80

  expect_equal(
    rotate3d(pitch = pitch, yaw = yaw, roll = roll),
    Rx(roll) %*% Ry(yaw) %*% Rz(pitch),
    tolerance = 1e-12
  )
})

test_that("rotate3d agrees with single-axis rotations when other angles are zero", {
  expect_equal(rotate3d(pitch = 90), Rz(90), tolerance = 1e-12)
  expect_equal(rotate3d(yaw = 90), Ry(90), tolerance = 1e-12)
  expect_equal(rotate3d(roll = 90), Rx(90), tolerance = 1e-12)
})

test_that("rotation matrices are orthonormal with determinant one", {
  mats <- list(
    Rz(37),
    Ry(-42),
    Rx(123),
    rotate3d(pitch = 45, yaw = 30, roll = -80)
  )

  for (mat in mats) {
    expect_equal(crossprod(mat), diag(3), tolerance = 1e-12)
    expect_equal(unname(det(mat)), 1, tolerance = 1e-12)
  }
})

test_that("Rz, Ry, and Rx validate scalar numeric arguments", {
  expect_error(Rz("a"), "`pitch` must be a numeric scalar.")
  expect_error(Rz(c(1, 2)), "`pitch` must be a numeric scalar.")

  # This reflects the current error message in Ry().
  expect_error(Ry("a"), "`roll` must be a numeric scalar.")
  expect_error(Ry(c(1, 2)), "`roll` must be a numeric scalar.")

  expect_error(Rx("a"), "`roll` must be a numeric scalar.")
  expect_error(Rx(c(1, 2)), "`roll` must be a numeric scalar.")
})
