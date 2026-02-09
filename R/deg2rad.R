#' Convert degrees to radians, and vice versa
#'
#' @param deg numeric; angle to be converted, in degrees
#' @param rad numeric; angle to be converted, in radians
#'
#' @returns converted angle, in radians or degrees
#'
#' @examples
#' deg2rad(180)
#' rad2deg(pi)
#' @name deg2rad
NULL

#' @describeIn deg2rad Convert degrees to radians
#' @export
deg2rad <- function(deg) {
  return(deg*pi/180)
}

#' @describeIn deg2rad Convert radians to degrees
#' @export
rad2deg <- function(rad) {
  return(rad*180/pi)
}
