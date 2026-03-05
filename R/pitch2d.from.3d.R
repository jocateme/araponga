#' Calculate projected 2D pitch angle from orientations and view elevation
#'
#' @description
#' Compute the 2D pitch produced by 3D orientations (pitch, yaw) when viewed from a specified
#' elevation.
#'
#' @param pitch Numeric scalar: vertical angle relative to horizontal plane, in degrees, in the
#'  interval \[-90, 90\]. Convention: `90` = pointed up, `0` = horizontally aligned, `-90` = pointed
#'  down.
#' @param yaw Numeric scalar: horizontal angle around the vertical axis, in degrees, in the
#'  interval (-180, 180]. Convention: `0` = pointed right, `90` = pointed straight away,
#'  `-90` = pointed straight toward, `180` = pointed left.
#' @param view_elevation Numeric scalar: angle between camera and object, in degrees, in the
#'  interval \[-90, 90\]. Convention: `-90` = seen from straight below, `0` = eye level,
#'  `90` = seen from straight above.
#' @param plot Logical scalar. `TRUE` draws a diagnostic plot with original and rotated axes,
#'  and calculated 2D pitch angle. 
#'
#' @returns Numeric scalar: 2D pitch angle, in degrees, in the interval (-180°, 180°].
#'  Convention: `0` points right, `90` points up, `-90` points down, `180` points left.
#'
#' @details
#' The function constructs the full rotation matrix \eqn{R} using
#' [rotate3d](`pitch`, `yaw`, `view_elevation`) and computes the projected 2D pitch \eqn{p_{2}} as
#' \deqn{p_{2} = \operatorname{atan2}\!\big(R_{2,1},\,R_{1,1}\big)}
#' 
#' @examples
#' # object pointed up 15 degrees
#' pitch2d.from.3d(15, 0, 0, plot = TRUE)
#' # object pointed up 15 and 30 degrees toward camera
#' pitch2d.from.3d(15, -30, 0, plot = TRUE)
#' # object pointed up 15 degrees, looked at from 30 degrees below
#' pitch2d.from.3d(15, 0, -30, plot = TRUE)
#' 
#' @export
pitch2d.from.3d <- function(pitch,
                            yaw,
                            view_elevation,
                            plot = FALSE){
  
  ## ---- input validation ----
  if (!is.numeric(pitch) || length(pitch) != 1) stop("`pitch` must be a numeric scalar.", call. = FALSE)
  if (!is.numeric(yaw) || length(yaw) != 1) stop("`yaw` must be a numeric scalar.", call. = FALSE)
  if (!is.numeric(view_elevation) || length(view_elevation) != 1) stop("`view_elevation` must be a numeric scalar.", call. = FALSE)
  if (!is.logical(plot) || length(plot) != 1) stop("`plot` must be a logical scalar.", call. = FALSE)
  
  if(view_elevation < -90 | view_elevation > 90){
    stop("`view_elevation` must satisfy -90 <= `view_elevation` <= 90 degrees.")
  }
  if(pitch < -90 | pitch > 90){
    stop("`pitch` must satisfy -90 <= `pitch` <= 90 degrees.")
  }
  if(yaw <= -180 | yaw > 180){
    stop("`yaw` must satisfy -180 < `yaw` <= 180")
  }
  
  R_total <- rotate3d(pitch,
                      yaw,
                      view_elevation)
  
  pitch2d <- rad2deg(atan2(R_total[2,1], R_total[1,1]))
  
  if(plot){
    xlim = ylim = c(-1, 1)
    graphics::plot(x = c(R_total[1,1], 0),
                   y = c(R_total[2,1], 0),
                   type = "l",
                   xlab = "projected x (2D)",
                   ylab = "projected y (2D)",
                   xaxt = "n",
                   yaxt = "n",
                   xlim = xlim,
                   ylim = ylim,
                   col = "darkgreen",
                   las = 1)
    graphics::lines(x = c(R_total[1,2], 0),
                    y = c(R_total[2,2], 0),
                    col = "darkred")
    graphics::lines(x = c(R_total[1,3], 0),
                    y = c(R_total[2,3], 0),
                    col = "orange")
    
    graphics::lines(x = c(1, 0),
                    y = c(0, 0),
                    col = "darkgreen",
                    lty = 2)
    graphics::lines(x = c(0, 0),
                    y = c(1, 0),
                    col = "darkred",
                    lty = 2)
    
    graphics::text(x = c(R_total[1,1], R_total[1,2], R_total[1,3]),
                   y = c(R_total[2,1], R_total[2,2], R_total[2,3]),
                   lab = c("rotated x",
                           "rotated y",
                           "rotated z"),
                   col = c("darkgreen",
                           "darkred",
                           "orange"))
    
    graphics::text(x = c(1, 0),
                   y = c(0, 1),
                   lab = c("original x",
                           "original y"),
                   col = c("darkgreen",
                           "darkred"))
    
    angles <- deg2rad(seq(from = min(0, pitch2d),
                          to = max(0, pitch2d),
                          by = 0.01))
    r <- 0.2*min(diff(xlim), diff(ylim))
    graphics::lines(x = c(0,
                          r*cos(angles),
                          0),
                    y = c(0,
                          r*sin(angles),
                          0),
                    col = "darkblue")
    if(pitch2d < 0){
      ytxt <- min(0.1*diff(ylim) * sin(angles))
    } else {
      ytxt <- max(0.1*diff(ylim) * sin(angles))
    }
    graphics::text(x = max(0.1*diff(xlim) * cos(angles)),
                   y =  ytxt,
                   labels = paste0(round(pitch2d, 2), "\u00B0"),
                   col = "darkblue")
  }
  
  return(pitch2d)
}