#' Calculate projected 2D pitch angle from orientations and view elevation
#'
#' @param pitch numeric [90, -90]; vertical angle relative to horizontal plane, in degrees (up = 90, down = -90)
#' @param yaw numeric (-180, 180]; horizontal angle around the vertical axis, in degrees (0 = pointed right, 90 = pointed straight away, -90 = pointed straight torward, 180 = pointed left)
#' @param view_elevation numeric [-90, 90]; angle between observer and the object, in degrees (-90 straight below, 0 = eye level, 90 = straight up)
#' @param plot logical; if TRUE plots calculated 2D pitch angle.
#'
#' @returns 2D pitch angle, in degrees
#' @export
#'
#' @examples
#' # object pointed up 15 degrees
#' pitch2d.from.3d(15, 0, 0, plot = TRUE)
#' # object pointed up 15 and right 30 degrees
#' pitch2d.from.3d(15, -30, 0, plot = TRUE)
#' # object pointed up 15 degrees, looked at from 30 degrees below
#' pitch2d.from.3d(15, 0, -30, plot = TRUE)
pitch2d.from.3d <- function(pitch,
                                yaw,
                                view_elevation,
                                plot = FALSE){
  
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
                   col = "darkgreen")
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
                   lab = c("rotated x ()",
                           "rotated y ()",
                           "rotated z ()"),
                   col = c("darkgreen",
                           "darkred",
                           "orange"))
    
    graphics::text(x = c(1, 0),
                   y = c(0, 1),
                   lab = c("original x ()",
                           "original y ()"),
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
    if(pitch2d < 0){ytxt <- min(0.1*diff(ylim) * sin(angles))} else {ytxt <- max(0.1*diff(ylim) * sin(angles))}
    graphics::text(x = max(0.1*diff(xlim) * cos(angles)),
                   y =  ytxt,
                   labels = paste0(round(pitch2d, 2), "\u00B0"),
                   col = "darkblue")
  }
  
  return(pitch2d)
}