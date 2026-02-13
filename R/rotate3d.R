#' Create and apply 3D Euler rotations
#' 
#' @description
#' Create 3x3 rotation matrices after applying pitch-yaw-roll ("ZYX") Euler rotations.
#'
#' @param pitch numeric; rotation about the z-axis, in degrees
#' @param yaw numeric; rotation about the y-axis, in degrees
#' @param roll numeric; rotation about the x-axis, in degrees
#'
#' @returns a 3x3 rotation matrix with Euler rotations applied
#'
#' @examples
#' # rotate 30 degrees about x
#' Rx(30)
#' # rotate 45 about z, then 30 about y, then -80 about x
#' rotate3d(45, 30, -80)
#' @name rotate3d
NULL

#' @describeIn rotate3d Create apply Euler rotations (roll, yaw, pitch)
#' @export
rotate3d <- function(pitch, yaw, roll){
  
  R_total <- Rx(roll) %*% Ry(yaw) %*% Rz(pitch)
  
  return(R_total)
}

#' @describeIn rotate3d Rotate about the x-axis
#' @export
Rx <- function(roll) {
  th <- deg2rad(roll)
  matrix(c(1, 0, 0,
           0, cos(th), -sin(th),
           0, sin(th), cos(th)),
         nrow = 3,
         byrow = TRUE)
}

#' @describeIn rotate3d Rotate about the y-axis
#' @export
Ry <- function(yaw) {
  th <- deg2rad(yaw)
  matrix(c(cos(th), 0, sin(th),
           0, 1, 0,
           -sin(th), 0, cos(th)),
         nrow = 3,
         byrow = TRUE)
}

#' @describeIn rotate3d Rotate about the z-axis
#' @export
Rz <- function(pitch) {
  th <- deg2rad(pitch)
  matrix(c(cos(th), -sin(th), 0,
           sin(th), cos(th), 0,
           0, 0, 1),
         nrow = 3,
         byrow = TRUE)
}