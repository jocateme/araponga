#' Convert degrees to radians, and vice versa
#'
#' @description
#' `deg2rad()` takes angles in degrees and returns them converted in radians.
#' 
#' `rad2deg()` takes angles in radians and returns them converted in degrees.
#'
#' @param deg Numeric vector of angles in degrees.
#' 
#' @return A numeric vector of angles, in radians or degrees. Length matches `deg` or `rad`.
#'  
#' @examples
#' deg2rad(c(0, 90, 180))
#' 
#' @export
deg2rad <- function(deg) {
  ## input checks
  if (missing(deg) || length(deg) == 0) {
    stop("`deg` must be a non-empty numeric vector.", call. = FALSE)
  }
  if (!is.numeric(deg)){
    stop("`deg` must be numeric.", call. = FALSE)
  }
  deg * pi / 180
}

#' @rdname deg2rad
#' 
#' @param rad Numeric vector of angles in radians.
#' 
#' @examples
#' rad2deg(c(0, pi/2, pi))
#' 
#' @export
rad2deg <- function(rad) {
  ## input checks
  if (missing(rad) || length(rad) == 0) {
    stop("`rad` must be a non-empty numeric vector.", call. = FALSE)
  }
  if (!is.numeric(rad)){
    stop("`rad` must be numeric.", call. = FALSE)
  }
  rad * 180 / pi
}
