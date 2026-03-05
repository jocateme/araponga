#' Trim candidate yaw sets known or expected to differ in yaw
#'
#' @description
#' Given two sets of candidate yaw angles, [trim.yaws()] removes elements from each set that
#' do not have at least one partner in the other set that is at least `min_sep` and up to
#' `max_sep` clockwise (for `ccw_yaws` set) or counterclockwise (for `cw_yaws`) from it. In
#' other words, each retained `ccw_yaws` angle will be ≥ `min_sep` and ≤ `max_sep`
#' counterclockwise of at least one `cw_yaws` angle. Analogously, each retained `cw_yaws` angle
#' will be ≥ `min_sep` and ≤ `max_sep` clockwise of at least one `ccw_yaws` angle.
#'
#' @param ccw_yaws Numeric vector: candidate yaw angles known/expected to be
#'  **counterclockwise** of `cw_yaws` (degrees, in the interval (-180, 180]).
#' @param cw_yaws Numeric vector: candidate yaw angles known/expected to be **clockwise** of
#'  `ccw_yaws` (degrees, in the interval (-180, 180]).
#' @param min_sep Non-negative numeric scalar: the minimum angular separation (degrees) allowed
#'  between a value in one set and a partner in the other set.
#' @param max_sep Non-negative numeric scalar: the maximum angular separation (degrees) allowed
#'  between a value in one set and a partner in the other set.
#' @param plot Logical scalar. `TRUE` draws diagnostic plots with retained (green) and excluded
#'  (red) angles for each set. 
#'
#' @return A \code{list} with elements:
#' \describe{
#'   \item{trimmed_ccw_yaws}{numeric: subset of `ccw_yaws` that have at least one matching `cw_yaws`.}
#'   \item{trimmed_cw_yaws}{numeric: subset of `cw_yaws` that have at least one matching `ccw_yaws`.}
#' }
#'
#' @examples
#' # hypothetical candidate yaw sets:
#' a <- 10:80
#' b <- 30:90
#' 
#' # we know `a` to be CCW of `b` by up to 180°
#' trim.yaws(a, b, 0, 180, plot = TRUE)
#'
#' # we know `a` to be CCW of `b` by 30 to 45°
#' trim.yaws(a, b, 30, 45, plot = TRUE)
#'
#' # if no mutual partners exist, both returned vectors become empty
#' trim.yaws(ccw_yaws = c(-10), cw_yaws = c(130), min_sep = 0, max_sep = 180)
#'
#' @export
trim.yaws <- function(ccw_yaws,
                      cw_yaws,
                      min_sep,
                      max_sep,
                      plot = FALSE
){
  
  # basic argument validation
  if (!is.numeric(max_sep) || !is.numeric(min_sep)) stop("max_sep and min_sep must be numeric", call. = FALSE)
  if (min_sep < 0) stop("min_sep must be >= 0", call. = FALSE)
  if (min_sep > max_sep) stop("min_sep must be <= max_sep", call. = FALSE)
  
  if (missing(ccw_yaws) || missing(cw_yaws)) stop("Both ccw_yaws and cw_yaws must be supplied.", call. = FALSE)
  if (!is.numeric(ccw_yaws) || !is.numeric(cw_yaws)) stop("ccw_yaws and cw_yaws must be numeric.", call. = FALSE)
  if (any(is.na(ccw_yaws)) || any(is.na(cw_yaws))) stop("ccw_yaws and cw_yaws must not contain NA values.", call. = FALSE)
  
  if (any(ccw_yaws <= -180 | ccw_yaws > 180) || any(cw_yaws <= -180 | cw_yaws > 180)) {
    stop("Yaw angles must satisfy -180 < yaw <= 180 degrees.", call. = FALSE)
  }
  
  ccw_yaws  <- sort(unique(as.numeric(ccw_yaws)))
  cw_yaws <- sort(unique(as.numeric(cw_yaws)))
  ccw_original <- ccw_yaws
  cw_original <- cw_yaws
  
  # quick exit: if either set is empty there's nothing to match
  if (length(ccw_yaws) == 0 || length(cw_yaws) == 0) {
    return(list(new_ccw_yaws = numeric(0), new_cw_yaws = numeric(0)))
  }
  
  # iterative mutual trimming
  repeat{
    
    # lower and upper endpoints for allowed cw_yaws for each ccw_yaw
    low_ccw  <- ((ccw_yaws - max_sep + 180) %% 360) - 180
    high_ccw <- ((ccw_yaws - min_sep + 180) %% 360) - 180
    
    # idx_low_ccw = number of cw_yaws < low_ccw
    idx_low_ccw  <- findInterval(low_ccw - 1e-10, cw_yaws) # 1e-10 to make boundary inclusive
    # idx_high_ccw = number of cw_yaws <= high_ccw
    idx_high_ccw <- findInterval(high_ccw, cw_yaws) # top bound naturally inclusive
    
    # for each ccw_yaw, is at least one cw_yaw between low_ccw and high_ccw?
    exists_ccw <- logical(length(ccw_yaws))
    n_cw <- length(cw_yaws)
    for (j in seq_along(ccw_yaws)) {
      if (low_ccw[j] <= high_ccw[j]) {
        exists_ccw[j] <- (idx_high_ccw[j] - idx_low_ccw[j]) > 0
      } else {
        exists_ccw[j] <- ((n_cw - idx_low_ccw[j]) + idx_high_ccw[j]) > 0
      }
    }
    ccw_yaws_new <- ccw_yaws[exists_ccw]
    
    # same process, now for cw_yaws
    low_cw  <- ((cw_yaws + min_sep + 180) %% 360) - 180
    high_cw <- ((cw_yaws + max_sep + 180) %% 360) - 180
    
    if (length(ccw_yaws_new) == 0) {
      cw_yaws_new <- integer(0)
    } else {
      idx_low_cw  <- findInterval(low_cw - 1e-10, ccw_yaws_new) 
      idx_high_cw <- findInterval(high_cw, ccw_yaws_new)
      
      exists_cw <- logical(length(cw_yaws))
      n_ccwnew <- length(ccw_yaws_new)
      for (j in seq_along(cw_yaws)) {
        if (low_cw[j] <= high_cw[j]) {
          exists_cw[j] <- (idx_high_cw[j] - idx_low_cw[j]) > 0
        } else {
          exists_cw[j] <- ((n_ccwnew - idx_low_cw[j]) + idx_high_cw[j]) > 0
        }
      }
      cw_yaws_new <- cw_yaws[exists_cw]
    }
    
    # break iteration if neither side changed
    if (identical(ccw_yaws_new, ccw_yaws) && identical(cw_yaws_new, cw_yaws)) break
    
    # otherwise update and repeat
    ccw_yaws  <- ccw_yaws_new
    cw_yaws <- cw_yaws_new
    
  }
  
  if(plot){
    
    ccw_excl <- ccw_original[!ccw_original %in% ccw_yaws]
    cw_excl <- cw_original[!cw_original %in% cw_yaws]
    
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mfrow = c(1, 2))
    
    visualize.angles(yaws = c(ccw_yaws, ccw_excl),
                     col.yaws = c(rep("green", length(ccw_yaws)),
                                  rep("red", length(ccw_excl))),
                     main.yaws = "ccw_yaws")
    visualize.angles(yaws = c(cw_yaws, cw_excl),
                     col.yaws = c(rep("green", length(cw_yaws)),
                                  rep("red", length(cw_excl))),
                     main.yaws = "cw_yaws")
    
  }
  
  # return trimmed mutually-consistent sets
  return(list(trimmed_ccw_yaws = ccw_yaws,
              trimmed_cw_yaws = cw_yaws))
}
