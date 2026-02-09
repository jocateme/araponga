#' Create and apply 3D Euler rotations
#' 
#' @description
#' Create 3x3 rotation matrices from Euler angles (roll, yaw, pitch).
#'
#' @param pitch3d numeric; rotation about the z-axis, in degrees
#' @param yaw3d numeric; rotation about the y-axis, in degrees
#' @param roll3d numeric; rotation about the x-axis, in degrees
#'
#' @returns a 3x3 rotation matrix
#'
#' @examples
#' # rotate 30 degrees about x
#' Rx(30)
#' # rotate 45 about z, then 30 about y, then -80 about x
#' Rx(-80) %*% Ry(30) %*% Rz(45)
#' @name rotate3d
NULL

#' @describeIn rotate3d Rotate about the x-axis
#' @export
Rx <- function(roll3d) {
  th <- deg2rad(roll3d)
  matrix(c(1, 0, 0,
           0, cos(th), -sin(th),
           0, sin(th), cos(th)),
         nrow = 3,
         byrow = TRUE)
}

#' @describeIn rotate3d Rotate about the y-axis
#' @export
Ry <- function(yaw3d) {
  th <- deg2rad(yaw3d)
  matrix(c(cos(th), 0, sin(th),
           0, 1, 0,
           -sin(th), 0, cos(th)),
         nrow = 3,
         byrow = TRUE)
}

#' @describeIn rotate3d Rotate about the z-axis
#' @export
Rz <- function(pitch3d) {
  th <- deg2rad(pitch3d)
  matrix(c(cos(th), -sin(th), 0,
           sin(th), cos(th), 0,
           0, 0, 1),
         nrow = 3,
         byrow = TRUE)
}