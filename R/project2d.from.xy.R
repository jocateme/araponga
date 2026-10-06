#' Calculate a projected 2D vector from two landmarks
#'
#' @description
#' Calculate the projected pitch and length of the 2D directed vector defined by a base landmark and a tip
#' landmark.
#' 
#' @param x_tip,x_base Numeric vectors: image *x*-coordinate(s) of tip and base of object, with
#'  *x* increasing right.
#' @param y_tip,y_base Numeric vectors: image *y*-coordinate(s) of the tip and base of objects,
#'  with *y* increasing up.
#' @param plot Logical scalar. `TRUE` draws a diagnostic plot with base-tip segment and calculated 2D
#'  pitch angle. Plotting is only supported for single base-tip pair.
#'
#' @returns
#' A `data.frame` of class `araponga2d`, with one row per base-tip pair and the following columns:
#' * `x_tip`, `y_tip`, `x_base`, `y_base`: supplied landmark coordinates.
#' * `pitch2d`: projected 2D pitch angle(s), in degrees in the interval (-180, 180].
#' * `length2d`: Euclidean distance(s) between base and tip, in the same units as the supplied coordinates.
#' 
#' When tip and base coincide, `length2d` is zero and `pitch2d` is undefined (`NA`).
#' 
#' The returned object can be supplied as `observed2d` to [find.3d()] and wrappers.
#'  
#' @details
#' The landmarks define the directed vector
#' 
#' \deqn{(dx,dy)=(x_{tip}-x_{base}, y_{tip}-y_{base})}
#' 
#' with projected 2D pitch
#' 
#' \deqn{p_{2} = \operatorname{atan2}(dy, dx)}
#' 
#' and projected 2D length
#' 
#' \deqn{L_2 = \sqrt{dx^2 + dy^2}.}
#' 
#' `length2d` is expressed in the same units as the landmark coordinates. For image coordinates in pixels,
#' for example, `length2d` is returned in pixels.
#' 
#' Coordinate arguments may have length 1 or a common length greater than 1. Length-1 arguments are recycled.
#'
#' @examples
#' # scalar examples (plots)
#' project2d.from.xy(1, 0, 0, 0, plot = TRUE) # pointed right -> 0°
#' project2d.from.xy(-1, 0, 0, 0, plot = TRUE) # pointed left -> 180°
#' project2d.from.xy(0, 1, 0, 0, plot = TRUE) # pointed up -> 90°
#' project2d.from.xy(0, -1, 0, 0, plot = TRUE) # pointed down -> -90°
#'
#' # vectorised usage (no plot)
#' x_tips  <- c(1, 0, -1)
#' y_tips  <- c(0, 1, 0)
#' x_bases <- c(0, 0, 0)
#' y_bases <- c(0, 0, 0)
#' project2d.from.xy(x_tips, y_tips, x_bases, y_bases)
#' 
#' @seealso [pitch2d.w.error()], [project2d.from.3d()]
#' @export
project2d.from.xy <- function(x_tip,
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
  
  if (any(!is.finite(x_tip)) ||
      any(!is.finite(y_tip)) ||
      any(!is.finite(x_base)) ||
      any(!is.finite(y_base))) {
    stop(
      "All coordinate arguments must contain only finite values.",
      call. = FALSE
    )
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
  
  if (!is.logical(plot) || length(plot) != 1 || is.na(plot)) {
    stop("`plot` must be a logical scalar.", call. = FALSE)
  }
  
  if (plot && n != 1) {
    warning(
      "Setting `plot = FALSE`; plotting is only supported when all coordinate arguments are length 1.",
      call. = FALSE
    )
    plot <- FALSE
  }
  
  ## compute 2D pitch and length
  dx <- x_tip - x_base
  dy <- y_tip - y_base
  
  project2d <- .project2d.from.components(dx, dy)
  pitch2d <- project2d$pitch2d
  length2d <- project2d$length2d
  
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
    graphics::abline(h = y_base,
                     col = "gray",
                     lty = 2)
    
    if(!is.na(pitch2d)){
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
    }
    
    graphics::text(x = mean(c(x_base, x_tip)),
                   y =  mean(c(y_base, y_tip)),
                   labels = round(length2d, 2),
                   col = "black")
    
    graphics::points(x = c(x_base, x_tip),
                     y = c(y_base, y_tip),
                     col = c("darkgreen", "darkred"),
                     pch = 16)
  }
  
  total2d <- data.frame(x_tip = x_tip,
                        y_tip = y_tip,
                        x_base = x_base,
                        y_base = y_base,
                        pitch2d = pitch2d,
                        length2d = length2d)
  
  class(total2d) <- c("araponga2d", "data.frame")
  
  return(total2d)
  
}

.project2d.from.components <- function(dx, dy) {
  
  length2d_sq <- dx^2 + dy^2
  
  degenerate <- length2d_sq == 0
  
  pitch2d <- rad2deg(atan2(dy, dx))
  pitch2d[degenerate] <- NA
  pitch2d[pitch2d <= -180] <- 180 # floating point (< -180), package convention (== -180)
  
  length2d <- sqrt(length2d_sq)
  
  return(data.frame(pitch2d = pitch2d,
                    length2d = length2d))
  
}