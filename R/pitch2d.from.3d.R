#' Calculate projected 2D pitch from 3D orientations
#'
#' @description
#' Compute projected 2D pitch produced by combinations of 3D pitch, yaw, and view elevation.
#'
#' @param pitch Numeric vector: vertical orientation of the object relative to the horizontal plane, in
#'  degrees, in the interval \(-180, 180\]. Convention: `90` = pointed up, `0` = horizontally aligned,
#'  `-90` = pointed down.
#' @param yaw Numeric vector: horizontal orientation around the vertical axis, in degrees in the
#'  interval (-180, 180]. Convention: `0` = pointed right, `90` = pointed straight away,
#'  `-90` = pointed straight toward, `180` = pointed left.
#' @param view_elevation Numeric vector: camera elevation relative to the object, in degrees in
#'  the interval \[-90, 90\]. Convention: `-90` = seen from straight below, `0` = eye level,
#'  `90` = seen from straight above.
#' @param plot Logical scalar. `TRUE` draws a diagnostic plot with original and rotated axes,
#'  and calculated 2D pitch angle. Plotting is only supported when all angle arguments have length 1.
#'
#' @returns
#' A numeric vector of projected 2D pitch angles, in degrees in the interval (-180, 180], with
#' the same length as `pitch`, `yaw`, and `view_elevation`. When the projected object axis has effectively
#' zero length, its 2D pitch is undefined and `NA` is returned.
#'
#' @details
#' For the rotation matrix \eqn{R = R_x(e)R_y(y)R_z(p)}, where \eqn{p}, \eqn{y}, and \eqn{e} are pitch,
#' yaw, and view elevation in radians, respectively (as in [rotate3d()]), projected 2D pitch is defined
#' by
#'
#' \deqn{p_{2} = \operatorname{atan2}(R_{2,1}, R_{1,1})}
#'
#' with
#'
#' \deqn{R_{1,1} = \cos(y)\cos(p)}
#' \deqn{R_{2,1} = \cos(e)\sin(p) +
#'       \sin(e)\sin(y)\cos(p)}
#' 
#' `pitch2d.from.3d()` implements these equations.
#' 
#' When the object axis is effectively parallel to the viewing axis, its projected length is zero and
#' its 2D pitch is geometrically undefined. Such cases are returned as `NA`.
#'
#' `pitch`, `yaw`, and `view_elevation` are evaluated elementwise and must have the same length.
#' 
#' @examples
#' # scalar usage and plot
#' # object pointed up 15 degrees
#' pitch2d.from.3d(15, 0, 0, plot = TRUE)
#' # object pointed up 15 and 30 degrees toward camera
#' pitch2d.from.3d(15, -30, 0, plot = TRUE)
#' # object pointed up 15 degrees, looked at from 30 degrees below
#' pitch2d.from.3d(15, 0, -30, plot = TRUE)
#' 
#' # same orientations, now vectorized and no plotting
#' pitch2d.from.3d(pitch = c(15, 15, 15),
#'                 yaw = c(0, -30, 0),
#'                 view_elevation = c(0, 0, -30))
#' 
#' @seealso [rotate3d()], [pitch2d.from.xy()], [find.3d()]
#' @export
pitch2d.from.3d <- function(pitch,
                            yaw,
                            view_elevation,
                            plot = FALSE){
  
  ## ---- input validation ----
  if (missing(pitch) || missing(yaw) || missing(view_elevation)) {
    stop(
      "All angle arguments (`pitch`, `yaw`, `view_elevation`) must be provided.",
      call. = FALSE
    )
  }
  
  if (length(pitch) == 0 ||
      length(yaw) == 0 ||
      length(view_elevation) == 0) {
    stop("One or more empty angle arguments provided.", call. = FALSE)
  }
  
  if (!is.numeric(pitch) ||
      !is.numeric(yaw) ||
      !is.numeric(view_elevation)) {
    stop("All angle arguments must be numeric.", call. = FALSE)
  }
  
  if (length(pitch) != length(yaw) ||
      length(pitch) != length(view_elevation)) {
    stop(
      "`pitch`, `yaw`, and `view_elevation` must have the same length.",
      call. = FALSE
    )
  }
  
  if (any(!is.finite(pitch)) ||
      any(!is.finite(yaw)) ||
      any(!is.finite(view_elevation))) {
    stop(
      "All angle arguments must contain only finite values.",
      call. = FALSE
    )
  }
  
  if (any(pitch <= -180 | pitch > 180)) {
    stop(
      "`pitch` must satisfy -180 < value <= 180 degrees.",
      call. = FALSE
    )
  }
  
  if (any(yaw <= -180 | yaw > 180)) {
    stop(
      "`yaw` must satisfy -180 < value <= 180 degrees.",
      call. = FALSE
    )
  }
  
  if (any(view_elevation < -90 | view_elevation > 90)) {
    stop(
      "`view_elevation` must satisfy -90 <= value <= 90 degrees.",
      call. = FALSE
    )
  }
  
  if (!is.logical(plot) || length(plot) != 1 || is.na(plot)) {
    stop("`plot` must be a logical scalar.", call. = FALSE)
  }
  
  if (plot && length(pitch) != 1) {
    warning(
      "Setting `plot = FALSE`; plotting is only supported when all angle arguments are length 1.",
      call. = FALSE
    )
    plot <- FALSE
  }
  
  p <- deg2rad(pitch)
  y <- deg2rad(yaw)
  e <- deg2rad(view_elevation)
  
  R_11 <- cos(y) * cos(p)
  R_21 <- cos(e) * sin(p) + sin(e) * sin(y) * cos(p)
  
  pitch2d <- rad2deg(atan2(R_21, R_11))
  pitch2d[pitch2d <= -180] <- 180
  
  # zero-length projection = degenerate
  proj_length_sq <- R_21^2 + R_11^2
  degenerate <- proj_length_sq <= .Machine$double.eps
  pitch2d[degenerate] <- NA
  
  if(plot){
    
    R_total <- rotate3d(pitch,
                        yaw,
                        view_elevation)
    
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(xpd = TRUE)
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
                   labels = c("rotated x",
                           "rotated y",
                           "rotated z"),
                   col = c("darkgreen",
                           "darkred",
                           "orange"))
    
    graphics::text(x = c(1, 0),
                   y = c(0, 1),
                   labels = c("original x",
                           "original y"),
                   col = c("darkgreen",
                           "darkred"))
    
    if(!is.na(pitch2d)){
      
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
                      col = grDevices::adjustcolor("darkblue", alpha.f = 0.8))
      if(pitch2d < 0){
        ytxt <- min(0.1*diff(ylim) * sin(angles))
      } else {
        ytxt <- max(0.1*diff(ylim) * sin(angles))
      }
      graphics::text(x = max(0.1*diff(xlim) * cos(angles)),
                     y =  ytxt,
                     labels = paste0(round(pitch2d, 2), "\u00B0"),
                     col = grDevices::adjustcolor("darkblue", alpha.f = 0.8))
      
    }
    
  }
  
  return(pitch2d)
}