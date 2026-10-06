#' Project a 3D directed vector into two dimensions
#'
#' @description
#' Calculate the projected 2D pitch and length (relative or absolute) produced by combinations of 3D pitch,
#' yaw, and view elevation.
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
#' @param full_length Optional positive numeric vector giving the unprojected (no foreshortening) length of
#'  the directed vector. Values may be expressed in any linear unit. The returned `length2d` is expressed in
#'  the same unit. Scalar is recycled; otherwise must have the same length as the angle arguments.
#' @param plot Logical scalar. `TRUE` draws a diagnostic plot with original and rotated axes,
#'  projected 2D pitch and, if `full_length` is supplied, projected 2D length. Plotting is only supported
#'  when all angle arguments have length 1.
#'
#' @returns
#' A `list` containing:
#' * `pitch2d`: projected 2D pitch angle(s), in degrees in the interval (-180, 180].
#' * `projection_factor`: projected length relative to `full_length`, ranging from 0 to 1.
#' * `dx_factor`, `dy_factor`: projected _x_ and _y_ components per unit `full_length`.
#' * `length2d`: if `full_length` is supplied, projected length in the same units.
#' * `dx`, `dy`: if `full_length` is supplied, projected _x_ and _y_ components in the same units.
#' 
#' When the vector is effectively parallel to the viewing axis, `projection_factor` is zero and 2D pitch
#' is undefined (`NA`).
#'
#' @details
#' For the rotation matrix
#' 
#' \deqn{R = R_x(e)R_y(y)R_z(p),}
#' 
#' where \eqn{p}, \eqn{y}, and \eqn{e} are respectively pitch, yaw, and view elevation in radians
#' (as in [rotate3d()]), the projected components of the directed vector are
#' 
#' \deqn{f_x = R_{1,1} = \cos(y)\cos(p)}
#' 
#' and
#' 
#' \deqn{f_y = R_{2,1} = \cos(e)\sin(p) + \sin(e)\sin(y)\cos(p).}
#' 
#' These are returned as `dx_factor` and `dy_factor`, respectively. Projected 2D pitch is then
#'
#' \deqn{p_{2} = \operatorname{atan2}(f_y, f_x)}
#' 
#' and projection factor is
#' 
#' \deqn{q = \sqrt{dx^2 + dy^2}.}
#' 
#' If `full_length` \eqn{L} is supplied, the projected components are
#' 
#' \deqn{dx = L f_x}
#'
#' and
#'
#' \deqn{dy = L f_y,}
#'
#' giving projected length
#'
#' \deqn{L_2 = L q.}
#' 
#' `full_length` therefore need not represent a physical length. For example, a pixel length measured from
#' the same object under a known orientation with `projection_factor == 1` may be supplied as a reference
#' length, provided image scale is unchanged. In that case, `length2d`, `dx`, and `dy` are returned in pixels.
#'
#' `pitch`, `yaw`, and `view_elevation` are evaluated elementwise and must have the same length.
#' 
#' @examples
#' # projected 2D pitch of object pointed up 15º and 30º toward camera, seen from 20º below
#' project2d.from.3d(
#'   pitch = 15,
#'   yaw = -30,
#'   view_elevation = -20,
#'   plot = TRUE
#' )
#'
#' # also predict projected length from a 100-pixel reference length
#' project2d.from.3d(
#'   pitch = 15,
#'   yaw = -30,
#'   view_elevation = -20,
#'   full_length = 100,
#'   plot = TRUE
#' )
#'
#' # vectorized usage
#' project2d.from.3d(
#'   pitch = c(15, 15, 15),
#'   yaw = c(0, -30, 0),
#'   view_elevation = c(0, 0, -30),
#'   full_length = 100
#' )
#' 
#' @seealso [rotate3d()], [project2d.from.xy()], [find.3d()]
#' @export
project2d.from.3d <- function(pitch,
                              yaw,
                              view_elevation,
                              full_length = NULL,
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
  
  if (!is.null(full_length)) {
    
    if (!is.numeric(full_length) ||
        length(full_length) == 0 ||
        any(!is.finite(full_length)) ||
        any(full_length <= 0)) {
      stop(
        "`full_length` must contain positive finite numeric values.",
        call. = FALSE
      )
    }
    
    if (!(length(full_length) %in% c(1, length(pitch)))) {
      stop(
        "`full_length` must have length 1 or the same length as the angle arguments.",
        call. = FALSE
      )
    }
    
    full_length <- rep(full_length, length.out = length(pitch))
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
  
  # floating point
  tol <- sqrt(.Machine$double.eps)
  R_11[abs(R_11) <= tol] <- 0
  R_21[abs(R_21) <= tol] <- 0
  
  project2d <- .project2d.from.components(R_11, R_21)
  pitch2d <- project2d$pitch2d
  q <- project2d$length2d
  
  if(!is.null(full_length)){
    length2d <- full_length*q
  }
  
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
    
    if(!is.null(full_length)){
      
      graphics::text(x = mean(c(R_total[1,1], 0)),
                     y = mean(c(R_total[2,1], 0)),
                     labels = round(length2d, 2),
                     col = "darkgreen")
        
    }
    
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
  
  if(is.null(full_length)){
    total2d <- list(pitch2d = pitch2d,
                    projection_factor = q,
                    dx_factor = R_11,
                    dy_factor = R_21)
  } else {
    total2d <- list(pitch2d = pitch2d,
                    length2d = length2d,
                    dx = R_11 * full_length,
                    dy = R_21 * full_length,
                    projection_factor = q,
                    dx_factor = R_11,
                    dy_factor = R_21)
  }
  
  return(total2d)
}