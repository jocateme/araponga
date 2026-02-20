#' Calculate projected 2D pitch from two landmarks
#'
#' @param x_tip numeric; x coordinate of object tip.
#' @param y_tip numeric; y coordinate of object tip.
#' @param x_base numeric; x coordinate of object base.
#' @param y_base numeric; y coordinate of object base.
#' @param plot logical; if TRUE plots calculated 2D pitch angle.
#'
#' @returns 2D pitch angle, in degrees
#' @export
#'
#' @examples
#' # pointed right
#' pitch2d.from.xy(1,0,0,0, plot = TRUE)
#' #' # pointed left
#' pitch2d.from.xy(-1,0,0,0, plot = TRUE)
#' #' # pointed up
#' pitch2d.from.xy(0,1,0,0, plot = TRUE)
#' #' # pointed down
#' pitch2d.from.xy(0,-1,0,0, plot = TRUE)
pitch2d.from.xy <- function(x_tip,
                                y_tip,
                                x_base,
                                y_base,
                                plot = FALSE){
  dx <- x_tip - x_base
  dy <- y_tip - y_base
  pitch2d <- rad2deg(atan2(dy, dx))
  
  if(plot){
    xlim = c(x_base - abs(x_tip - x_base) - 0.5, x_base + abs(x_tip - x_base) + 0.5)
    ylim = c(y_base - abs(y_tip - y_base) - 0.5, y_base + abs(y_tip - y_base) + 0.5)
    graphics::plot(x = c(x_tip, x_base),
                   y = c(y_tip, y_base),
                   type = "l",
                   xlab = "x",
                   ylab = "y",
                   xlim = xlim,
                   ylim = ylim)
    graphics::lines(x = c(x_base, max(xlim)),
                    y = c(y_base, y_base),
                    lty = 2)
    angles <- deg2rad(seq(from = min(0, pitch2d),
                          to = max(0, pitch2d),
                          by = 0.01))
    r <- 0.2*min(diff(xlim), diff(ylim))
    graphics::lines(x = c(x_base,
                          x_base + r*cos(angles),
                          x_base),
                    y = c(y_base,
                          y_base + r*sin(angles),
                          y_base),
                    col = "darkblue")
    if(pitch2d < 0){ytxt <- min(y_base + 0.1*diff(ylim) * sin(angles))} else {ytxt <- max(y_base + 0.1*diff(ylim) * sin(angles))}
    graphics::text(x = max(x_base + 0.1*diff(xlim) * cos(angles)),
                   y =  ytxt,
                   labels = paste0(round(pitch2d, 2), "\u00B0"),
                   col = "darkblue")
    graphics::points(x = c(x_base, x_tip),
                     y = c(y_base, y_tip),
                     col = c("darkgreen", "darkred"),
                     pch = 16)
  }
  
  return(pitch2d)
}
