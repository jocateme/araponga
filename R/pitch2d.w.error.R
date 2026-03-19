#' Simulate error in 2D pitch calculation from labeling error
#'
#' `pitch2d.w.error()` simulates the effect of landmark-labeling uncertainty on a 2D pitch
#' value returned by [pitch2d.from.xy()].
#'
#' @param pitch2d Numeric scalar returned by [pitch2d.from.xy()].
#' @param label_error Positive numeric scalar specifying the error (± pixels) used to perturb each
#'  landmark coordinate.
#' @param label_nsamp Positive integer scalar specifying the approximate number of grid combinations to
#'  evaluate. Default 625 uses a 5-point grid for each of the four coordinates, for 5^4 = 625 total
#'  combinations.
#' @param add_boundaries Logical scalar (default `FALSE`). If `TRUE`, boundary angles `0`, `90`, `-90`,
#'  and `180` are added when the simulated set spans them. This is useful when working with the returned
#'  angles individually, since these boundary values can behave differently from interior values.
#'  
#' @return Numeric vector of unique simulated 2D pitch angles, in degrees, in the interval `(-180, 180]`.
#' 
#' @seealso [pitch2d.from.xy()]
#' @export
pitch2d.w.error <- function(pitch2d,
                            label_error,
                            label_nsamp = 625,
                            add_boundaries = FALSE){
  
  if (!is.numeric(pitch2d) || length(pitch2d) != 1 || !is.finite(pitch2d)) {
    stop("`pitch2d` must be a finite numeric scalar.", call. = FALSE)
  }
  
  xy <- attributes(pitch2d)$xy
  if (is.null(xy) ||
      !is.list(xy) ||
      !all(c("x_tip", "y_tip", "x_base", "y_base") %in% names(xy)) ||
      any(!vapply(xy[c("x_tip", "y_tip", "x_base", "y_base")], is.numeric, logical(1)))) {
    stop("`pitch2d` must carry a valid `xy` attribute as returned by `pitch2d.from.xy()`.", call. = FALSE)
  }
  
  if (!is.numeric(label_error) || length(label_error) != 1 || !is.finite(label_error) || label_error <= 0) {
    stop("`label_error` must be a positive numeric scalar.", call. = FALSE)
  }
  
  if (!is.numeric(label_nsamp) || length(label_nsamp) != 1 || !is.finite(label_nsamp) ||
      label_nsamp < 1 || abs(label_nsamp - round(label_nsamp)) > 1e-8) {
    stop("`label_nsamp` must be a positive integer scalar.", call. = FALSE)
  }
  
  if (!is.logical(add_boundaries) || length(add_boundaries) != 1 || is.na(add_boundaries)) {
    stop("`add_boundaries` must be a logical scalar.", call. = FALSE)
  }
  
  label_nsamp <- as.integer(round(label_nsamp))
  
  xy <- attributes(pitch2d)$xy
  x_tip <- xy$x_tip
  y_tip <- xy$y_tip
  x_base <- xy$x_base
  y_base <- xy$y_base
  
  n <- ceiling(label_nsamp^(1/4))
  t_x <- seq(x_tip - label_error, x_tip + label_error, length.out = n)
  t_y <- seq(y_tip - label_error, y_tip + label_error, length.out = n)
  b_x <- seq(x_base - label_error, x_base + label_error, length.out = n)
  b_y <- seq(y_base - label_error, y_base + label_error, length.out = n)
  
  pitch2d.all <- sort(unique(pitch2d.from.xy(rep(t_x, each = n^3),
                                             rep(t_y, each = n^2, times = n),
                                             rep(b_x, each = n, times = n^2),
                                             rep(b_y, times = n^3))))
  
  if(add_boundaries){
    
    p2d.summ <- summarize.yaws(pitch2d.all)
    
    # do they cross -180/180?
    if(p2d.summ$wrap){
      pitch2d.all <- c(pitch2d.all,
                       180)
      # do they cross 90?
      if(p2d.summ$from < 90 | p2d.summ$to > 90){
        pitch2d.all <- c(pitch2d.all,
                         90)
      }
      # do they cross -90?
      if(p2d.summ$from < -90 | p2d.summ$to > -90){
        pitch2d.all <- c(pitch2d.all,
                         -90)
      }
      # do they cross 0?
      if(p2d.summ$from < 0 | p2d.summ$to > 0){
        pitch2d.all <- c(pitch2d.all,
                         0)
      }
    } else {
      # do they cross 90?
      if(p2d.summ$from < 90 & p2d.summ$to > 90){
        pitch2d.all <- c(pitch2d.all,
                         90)
      }
      # do they cross -90?
      if(p2d.summ$from < -90 & p2d.summ$to > -90){
        pitch2d.all <- c(pitch2d.all,
                         -90)
      }
      # do they cross 0?
      if(p2d.summ$from < 0 & p2d.summ$to > 0){
        pitch2d.all <- c(pitch2d.all,
                         0)
      }
    }
    
  }
  
  return(sort(unique(pitch2d.all)))
  
}

