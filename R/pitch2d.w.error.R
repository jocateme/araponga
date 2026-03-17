#' Simulate error in 2D pitch calculation from labeling error
#'
#' `pitch2d.w.error()` simulates the effect of labeling inaccuracy on `pitch2d`
#' calculation.
#'
#' @param pitch2d Numeric scalar: 2D pitch as returned by [pitch2d.from.xy()], in degrees in
#'  the interval (-180, 180].
#' @param label_error Positive numeric scalar specifying the error (± pixels) in landmark
#'  labeling.
#' @param label_nsim Integer scalar (default 100). Number of Monte Carlo draws passed internally to
#'  [runif()] to simulate error in pitch2d calculation.
#'  
#' @returns
#' (UPDATE)
#' `lookup.error()` returns a named list with two elements:
#' \describe{
#'   \item{`pitch`}{a list of maximum errors in pitch per source of error (named).}
#'   \item{`yaw`}{a list of maximum errors in yaw per source of error (named).}
#'   }
#'
#' `include.error()` returns a numeric vector with the expanded candidate angles (pitches or yaws)
#' after incorporating the maximum error retrieved by `lookup.error()$pitch` or `lookup.error()$yaw`
#' with a 0.01 resolution. The maximum error is stored as vector in the attribute `error.included`.
#' 
#' @seealso [find.pitch()], [find.yaw()], [pitch2d.from.xy()]
#' @export
pitch2d.w.error <- function(pitch2d,
                            label_error,
                            label_nsamp = 625,
                            canonical = FALSE){
  
  xy <- attributes(pitch2d)$xy
  x_tip <- xy$x_tip
  y_tip <- xy$y_tip
  x_base <- xy$x_base
  y_base <- xy$y_base
  
  n <- round(label_nsamp^(1/4))
  t_x <- seq(x_tip - label_error, x_tip + label_error, length.out = n)
  t_y <- seq(y_tip - label_error, y_tip + label_error, length.out = n)
  b_x <- seq(x_base - label_error, x_base + label_error, length.out = n)
  b_y <- seq(y_base - label_error, y_base + label_error, length.out = n)
  
  pitch2d.all <- sort(unique(pitch2d.from.xy(rep(t_x, each = n^3),
                                             rep(t_y, each = n^2, times = n),
                                             rep(b_x, each = n, times = n^2),
                                             rep(b_y, times = n^3))))
  
  if(canonical){
    
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

