#' Create and apply 3D rotations
#' 
#' @description
#' Create a 3×3 rotation matrix corresponding to sequential Euler rotations about Z, then Y,
#' then X (i.e., ZYX or pitch–yaw–roll sequence).
#'
#' @param pitch Numeric scalar: angle of rotation about the Z axis, in degrees. Defaults to `0` (no
#'  rotation).
#' @param yaw Numeric scalar: angle of rotation about the Y axis, in degrees. Defaults to `0` (no rotation).
#' @param roll Numeric scalar: angle of rotation about the X axis, in degrees. Defaults to `0` (no
#'  rotation).
#'
#' @returns A numeric 3×3 matrix (class `matrix`).
#'
#' @details
#' `Rz()`, `Ry()`, and `Rx()` construct elementary rotation matrices about
#' the Z, Y, and X axes, respectively. `rotate3d()` combines these as
#'
#' \deqn{R = R_x(r) R_y(y) R_z(p),}
#'
#' where \eqn{p}, \eqn{y}, and \eqn{r} are pitch, yaw, and roll. When the
#' resulting matrix is applied to a column vector, the rotations therefore
#' occur in the order pitch, then yaw, then roll.
#'
#' Angles are not restricted to a particular interval because rotations are
#' periodic; any finite value in degrees is accepted.
#'
#' In [pitch2d.from.3d()], the same X-axis rotation represented here by
#' `roll` is used to incorporate view elevation. This represents relative
#' viewing geometry and should not be confused with image-plane camera roll.
#'
#' @examples
#' # rotate 45 about z, then 30 about y, then -80 about x
#' rotate3d(45, 30, -80)
#' 
#' @export
rotate3d <- function(pitch = 0, yaw = 0, roll = 0){
  
  R_total <- Rx(roll) %*% Ry(yaw) %*% Rz(pitch)
  
  return(R_total)
}

#' @rdname rotate3d
#' 
#' @export
Rz <- function(pitch = 0){
  
  if (!is.numeric(pitch) ||
      length(pitch) != 1 ||
      !is.finite(pitch)) {
    stop(
      "`pitch` must be a finite numeric scalar.",
      call. = FALSE
    )
  }
  
  th <- deg2rad(pitch)
  matrix(c(cos(th), -sin(th), 0,
           sin(th), cos(th), 0,
           0, 0, 1),
         nrow = 3,
         byrow = TRUE)
}

#' @rdname rotate3d
#' 
#' @export
Ry <- function(yaw = 0){
  
  if (!is.numeric(yaw) ||
      length(yaw) != 1 ||
      !is.finite(yaw)) {
    stop(
      "`yaw` must be a finite numeric scalar.",
      call. = FALSE
    )
  }
  
  th <- deg2rad(yaw)
  matrix(c(cos(th), 0, sin(th),
           0, 1, 0,
           -sin(th), 0, cos(th)),
         nrow = 3,
         byrow = TRUE)
}

#' @rdname rotate3d
#' 
#' @examples
#' # rotate 30 degrees about x
#' Rx(30)
#' 
#' @export
Rx <- function(roll = 0){
  
  if (!is.numeric(roll) ||
      length(roll) != 1 ||
      !is.finite(roll)) {
    stop(
      "`roll` must be a finite numeric scalar.",
      call. = FALSE
    )
  }
  
  th <- deg2rad(roll)
  matrix(c(1, 0, 0,
           0, cos(th), -sin(th),
           0, sin(th), cos(th)),
         nrow = 3,
         byrow = TRUE)
}