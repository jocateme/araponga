#' Calculate projected 2D pitch from two landmarks
#'
#' @description
#' Compute the 2D pitch angle defined by two landmarks: a tip point and a base point.
#' 
#' @param x_tip,x_base Numeric scalars or vectors: image x-coordinate(s) of tip and base of object, with
#'  x increasing right.
#' @param y_tip,y_base Numeric scalars or vectors: image y-coordinate(s) of the tip and base of objects,
#'  with y increasing up.
#' @param plot Logical scalar. `TRUE` draws a diagnostic plot with base-tip segment and calculated 2D
#'  pitch angle. Plotting is only supported for single base-tip pair.
#'
#' @return Numeric vector of 2D pitch angles, in degrees, in the interval (-180°, 180°].
#'  Convention: `0` points right, `90` points up, `-90` points down, `180` points left.
#'  
#' @details
#' The calculation uses the two-landmark vector
#' \deqn{(dx,dy)=(x_{tip}-x_{base}, y_{tip}-y_{base})}
#' and returns
#' \deqn{p_{2} = \operatorname{atan2}(dy, dx).}
#'
#' @examples
#' # scalar examples (plots)
#' pitch2d.from.xy(1, 0, 0, 0, plot = TRUE) # pointed right -> 0°
#' pitch2d.from.xy(-1, 0, 0, 0, plot = TRUE) # pointed left -> 180°
#' pitch2d.from.xy(0, 1, 0, 0, plot = TRUE) # pointed up -> 90°
#' pitch2d.from.xy(0, -1, 0, 0, plot = TRUE) # pointed down -> -90°
#'
#' # vectorised usage (no plot)
#' x_tips  <- c(1, 0, -1)
#' y_tips  <- c(0, 1, 0)
#' x_bases <- c(0, 0, 0)
#' y_bases <- c(0, 0, 0)
#' pitch2d.from.xy(x_tips, y_tips, x_bases, y_bases)
#' 
#' @seealso [pitch2d.w.error()], [pitch2d.from.3d()]
#' @export
pitch2d.from.xy <- function(x_tip,
                            y_tip,
                            x_base,
                            y_base,
                            plot = FALSE){
  
  ## ---- input validation ----
  if (missing(x_tip) || missing(y_tip) || missing(x_base) || missing(y_base)) {
    stop("All coordinate arguments (x_tip, y_tip, x_base, y_base) must be provided.", call. = FALSE)
  }
  if (length(x_tip) == 0 || length(y_tip) == 0 || length(x_base)==0 || length(y_base) == 0) {
    stop("One or more empty coordinate arguments provided.", call. = FALSE)
  }
  if (!is.numeric(x_tip) || !is.numeric(y_tip) || !is.numeric(x_base) || !is.numeric(y_base)) {
    stop("All coordinate arguments must be numeric.", call. = FALSE)
  }
  
  # recycling rules: allow scalar vs vector, but lengths must be compatible
  n <- max(length(x_tip), length(y_tip), length(x_base), length(y_base))
  if (any(c(length(x_tip), length(y_tip), length(x_base), length(y_base)) != n &
          c(length(x_tip), length(y_tip), length(x_base), length(y_base)) != 1)) {
    stop("Coordinate arguments must have the same length or be scalars.", call. = FALSE)
  }
  
  x_tip  <- rep(x_tip, length.out = n)
  y_tip  <- rep(y_tip, length.out = n)
  x_base <- rep(x_base, length.out = n)
  y_base <- rep(y_base, length.out = n)
  
  if (any(is.na(x_tip) | is.na(y_tip) | is.na(x_base) | is.na(y_base))) {
    stop("Coordinate arguments must not contain NA values.", call. = FALSE)
  }
  
  if (!is.logical(plot) || length(plot) != 1) stop("`plot` must be a logical scalar.", call. = FALSE)
  if (plot && n != 1){
    warning("Setting `plot = FALSE`; plotting is only supported when all coordinate arguments are length 1.", call. = FALSE)
    plot <- FALSE
  }
  
  ## compute 2D pitch
  dx <- x_tip - x_base
  dy <- y_tip - y_base
  pitch2d <- rad2deg(atan2(dy, dx))
  
  ## plot (scalar only)
  if(plot){
    half <- max(abs(dx), abs(dy)) + 0.5
    xlim = c(x_base - half, x_base + half)
    ylim = c(y_base - half, y_base + half)
    graphics::plot(x = c(x_tip, x_base),
                   y = c(y_tip, y_base),
                   type = "l",
                   xlab = "x",
                   ylab = "y",
                   xlim = xlim,
                   ylim = ylim,
                   asp = 1)
    graphics::lines(x = c(x_base, max(xlim)),
                    y = c(y_base, y_base),
                    lty = 2)
    graphics::abline(h = y_base,
                     lty = 2)
    angles <- deg2rad(seq(from = min(0, pitch2d),
                          to = max(0, pitch2d),
                          by = 0.01))
    r <- 0.2*half
    graphics::lines(x = c(x_base,
                          x_base + r*cos(angles),
                          x_base),
                    y = c(y_base,
                          y_base + r*sin(angles),
                          y_base),
                    col = "darkblue")
    if(pitch2d < 0){
      ytxt <- min(y_base + 0.1*diff(ylim) * sin(angles))
    } else {
      ytxt <- max(y_base + 0.1*diff(ylim) * sin(angles))
    }
    graphics::text(x = max(x_base + 0.1*diff(xlim) * cos(angles)),
                   y =  ytxt,
                   labels = paste0(round(pitch2d, 2), "\u00B0"),
                   col = "darkblue")
    graphics::points(x = c(x_base, x_tip),
                     y = c(y_base, y_tip),
                     col = c("darkgreen", "darkred"),
                     pch = 16)
  }
  
  if(n == 1){
    attr(pitch2d, "xy") <- list(x_tip = x_tip,
                                y_tip = y_tip,
                                x_base = x_base,
                                y_base = y_base)
  }
  
  return(pitch2d)
}
