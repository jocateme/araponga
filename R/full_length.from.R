#' Estimate full length from camera parameters
#'
#' Estimates the unforeshortened length of an object in image pixels from its physical length,
#' distance from the camera, and camera parameters.
#'
#' @param length_physical Positive numeric vector specifying the physical length of the object. When
#'  `length(length_physical) > 1`, the range of supplied values is treated as the range of possible physical
#'  lengths.
#' @param distance Positive numeric vector specifying the distance from the camera to the object, in the
#'  same units as `length_physical`. When `length(distance) > 1`, the range of supplied values is treated as
#'  the range of possible distances.
#' @param image_width Positive numeric vector specifying the image width, in pixels, corresponding to the
#'  supplied camera parameters and image scale.  When `length(image_width) > 1`, the range of supplied values
#'  is treated as the range of possible image widths.
#' @param focal_length Positive numeric vector specifying camera focal length. Must be in the same units
#'  as `sensor_width`. When `length(focal_length) > 1`, the range of supplied values is treated as the range of
#'  possible focal lengths. Required when `field_of_view` is not supplied.
#' @param sensor_width Positive numeric vector specifying sensor width. Must be
#'  in the same units as `focal_length`. When `length(sensor_width) > 1`, the range of supplied values is treated
#'  as the range of possible sensor widths. Required when `field_of_view` is not supplied.
#' @param field_of_view Positive numeric vector specifying horizontal field of view, in degrees in
#'  the interval (0, 180). When `length(field_of_view) > 1`, the range of supplied values is treated as the range
#'  of possible fields of view. Required when `focal_length` and `sensor_width` are not supplied.
#'
#' @returns Numeric vector of length 2 giving the minimum and maximum compatible `full_length`
#'  values, in pixels.
#'
#' @details
#' Camera geometry can be specified either by `focal_length` and `sensor_width`, or by
#' `field_of_view`. These alternatives are mutually exclusive.
#'
#' Input vectors represent ranges of possible values rather than paired observations. The returned
#' interval contains the minimum and maximum `full_length` values compatible with all supplied
#' ranges, allowing inputs to vary independently within those ranges.
#'
#' The calculation assumes scaled orthographic projection: image scale is determined at the supplied
#' camera-to-object distance and treated as constant across the object.
#'
#' @examples
#' # focal length and sensor width
#' full_length.from.camera(length_physical = 10,
#'                         distance = 1000,
#'                         image_width = 2000,
#'                         focal_length = 50,
#'                         sensor_width = 20)
#'
#' # equivalent camera geometry specified by field of view
#' full_length.from.camera(length_physical = 10,
#'                         distance = 1000,
#'                         image_width = 2000,
#'                         field_of_view = rad2deg(2 * atan(20 / (2 * 50))))
#'
#' # propagate uncertainty in physical length and distance
#' full_length.from.camera(length_physical = c(9, 11),
#'                         distance = c(900, 1100),
#'                         image_width = 2000,
#'                         focal_length = 50,
#'                         sensor_width = 20)
#'
#' @seealso [find.3d()]
#' @export

full_length.from.camera <- function(length_physical,
                                    distance,
                                    image_width,
                                    focal_length = NULL,
                                    sensor_width = NULL,
                                    field_of_view = NULL){
  
  length_physical <- .bounds(length_physical, "length_physical")
  distance <- .bounds(distance, "distance")
  image_width <- .bounds(image_width, "image_width")
  
  if(is.null(field_of_view)) {
    
    if(is.null(focal_length) || is.null(sensor_width)) {
      stop("Supply either field_of_view or both focal_length and sensor_width.")
    }
    
    focal_length <- .bounds(focal_length, "focal_length")
    sensor_width <- .bounds(sensor_width, "sensor_width")
    
    scale_min <- focal_length[1] / sensor_width[2]
    scale_max <- focal_length[2] / sensor_width[1]
    
  } else {
    
    if (!is.null(focal_length) || !is.null(sensor_width)) {
      stop("Supply either field_of_view or focal_length and sensor_width, not both.")
    }
    
    field_of_view <- .bounds(field_of_view, "field_of_view")
    
    if(field_of_view[2] >= 180){
      stop("field_of_view must be greater than 0 and less than 180 degrees.")
    }
    
    field_of_view <- deg2rad(field_of_view)
    
    scale_min <- 1 / (2 * tan(field_of_view[2] / 2))
    scale_max <- 1 / (2 * tan(field_of_view[1] / 2))
    
  }
  
  out <- c(length_physical[1] * image_width[1] * scale_min / distance[2],
           length_physical[2] * image_width[2] * scale_max / distance[1])
  
  return(out)
}

#' Estimate full length from a reference segment
#'
#' Estimates the unforeshortened length of a target object in image pixels from a reference segment of
#' known physical length.
#'
#' @param length_physical Positive numeric vector specifying the physical length of the target object.
#'  When `length(length_physical) > 1`, the range of supplied values is treated as the range of possible
#'  physical lengths.
#' @param reference_physical Positive numeric vector specifying the physical length of the reference segment,
#'  in the same units as `length_physical`. When `length(reference_physical) > 1`, the range of supplied values
#'  is treated as the range of possible reference lengths.
#' @param reference_px Positive numeric vector specifying the length of the reference segment in image
#'  pixels. When `length(reference_px) > 1`, the range of supplied values is treated as the range of possible
#'  pixel lengths.
#'
#' @returns Numeric vector of length 2 giving the minimum and maximum compatible `full_length`
#'  values, in pixels.
#'
#' @details
#' Input vectors represent ranges of possible values rather than paired observations. The returned
#' interval contains the minimum and maximum `full_length` values compatible with all supplied
#' ranges, allowing inputs to vary independently within those ranges.
#'
#' The reference segment should be approximately perpendicular to the camera's viewing axis and at
#' approximately the same distance from the camera as the target object. Foreshortening of the
#' reference segment or differences in camera distance can bias the estimated `full_length`.
#'
#' @examples
#' full_length.from.reference(length_physical = 10,
#'                            reference_physical = 20,
#'                            reference_px = 100)
#'
#' # propagate uncertainty in target and reference measurements
#' full_length.from.reference(length_physical = c(9, 11),
#'                            reference_physical = c(18, 22),
#'                            reference_px = c(90, 110))
#'
#' @seealso [find.3d()]
#' @export

full_length.from.reference <- function(length_physical,
                                       reference_physical,
                                       reference_px){
  
  length_physical <- .bounds(length_physical, "length_physical")
  reference_physical <- .bounds(reference_physical, "reference_physical")
  reference_px <- .bounds(reference_px, "reference_px")
  
  out <- c(length_physical[1] * reference_px[1] / reference_physical[2],
           length_physical[2] * reference_px[2] /reference_physical[1])
  
  return(out)
  
}

.bounds <- function(x,
                    name){
  
  if (!is.numeric(x) ||
      length(x) < 1L ||
      any(!is.finite(x)) ||
      any(x <= 0)){
    stop(name, " must contain finite, positive numeric values.")
  }
  
  range(x)
}