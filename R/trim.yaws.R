#' Trim candidate yaw sets by directional separation
#'
#' @description
#' Trim two candidate yaw sets to angles that have at least one compatible
#' partner in the other set, given minimum and maximum directional separation
#' constraints.
#'
#' @details
#' Given two sets of candidate yaw angles, `trim.yaws()` retains angles that
#' have at least one compatible partner in the other set. Directional
#' separation is measured counterclockwise from `cw_yaws` to `ccw_yaws`
#' and is represented in the interval \[0, 360).
#'
#' For a pair of angles \eqn{c} from `ccw_yaws` and \eqn{w} from `cw_yaws`,
#' their directional separation is
#'
#' \deqn{(c - w) \bmod 360.}
#'
#' A pair is compatible when this separation is greater than or equal to
#' `min_sep` and less than or equal to `max_sep`. Each returned angle
#' therefore has at least one compatible partner in the other returned set.
#'
#' @param ccw_yaws Numeric vector: candidate yaw angles known/expected to be
#'  **counterclockwise** of `cw_yaws` (degrees, in the interval (-180, 180]).
#' @param cw_yaws Numeric vector: candidate yaw angles known/expected to be **clockwise** of
#'  `ccw_yaws` (degrees, in the interval (-180, 180]).
#' @param min_sep Numeric scalar: minimum directional angular separation in degrees, satisfying
#'  `0 <= min_sep < 360`.
#' @param max_sep Numeric scalar: maximum directional angular separation in degrees, satisfying
#'  `min_sep <= max_sep <= 360`.
#' @param plot Logical scalar. `TRUE` draws diagnostic plots with retained (blue) and excluded
#'  (red) angles for each set. 
#'
#' @returns A \code{list} with elements:
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
  
  if (missing(ccw_yaws) || missing(cw_yaws)) {
    stop("Both 'ccw_yaws' and 'cw_yaws' must be supplied.", call. = FALSE)
  }
  if (!(is.numeric(min_sep) && length(min_sep) == 1 && is.finite(min_sep))) {
    stop("'min_sep' must be a single finite numeric value.", call. = FALSE)
  }
  if (!(is.numeric(max_sep) && length(max_sep) == 1 && is.finite(max_sep))) {
    stop("'max_sep' must be a single finite numeric value.", call. = FALSE)
  }
  if (min_sep < 0) stop("'min_sep' must be >= 0.", call. = FALSE)
  if (max_sep < 0) stop("'max_sep' must be >= 0.", call. = FALSE)
  if (min_sep > max_sep) stop("'min_sep' must be <= 'max_sep'.", call. = FALSE)
  if (max_sep > 360) stop("'max_sep' must be <= 360.", call. = FALSE)
  if (min_sep >= 360) stop("'min_sep' must be < 360.", call. = FALSE)
  
  if (!is.numeric(ccw_yaws) || !is.numeric(cw_yaws) ||
      any(!is.finite(ccw_yaws)) || any(!is.finite(cw_yaws))) {
    stop(
      "'ccw_yaws' and 'cw_yaws' must be finite numeric vectors.",
      call. = FALSE
    )
  }
  
  # angle range check; require -180 < angle <= 180
  if (any(ccw_yaws <= -180 | ccw_yaws > 180) || any(cw_yaws <= -180 | cw_yaws > 180)) {
    stop("Yaw angles must satisfy -180 < yaw <= 180 degrees.", call. = FALSE)
  }
  
  ccw_yaws  <- sort(unique(as.numeric(ccw_yaws)))
  cw_yaws <- sort(unique(as.numeric(cw_yaws)))
  ccw_original <- ccw_yaws
  cw_original <- cw_yaws
  
  # quick exit: if either set is empty there's nothing to match
  if (length(ccw_yaws) == 0 || length(cw_yaws) == 0) {
    return(list(trimmed_ccw_yaws = numeric(0), trimmed_cw_yaws = numeric(0)))
  }
  
  # counterclockwise separation of each ccw yaw from each cw yaw
  separations <- outer(
    ccw_yaws,
    cw_yaws,
    FUN = function(ccw, cw) (ccw - cw) %% 360
  )
  
  # compatible pairs satisfy the requested separation interval
  compatible <- separations >= min_sep &
    separations <= max_sep
  
  # retain angles having at least one compatible partner
  ccw_yaws <- ccw_yaws[rowSums(compatible) > 0]
  cw_yaws <- cw_yaws[colSums(compatible) > 0]
  
  if(isTRUE(plot)){
    
    ccw_excl <- ccw_original[!ccw_original %in% ccw_yaws]
    cw_excl <- cw_original[!cw_original %in% cw_yaws]
    
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(
      mfrow = c(1, 2),
      oma = c(1.2, 0, 0, 0)
    )
    
    plot.angles(0,
                type = "yaw",
                col = "transparent",
                main = "Counterclockwise\nyaw set",
                labels = FALSE)
    if(length(ccw_yaws) > 0){
      plot.angles(ccw_yaws,
                  type = "yaw",
                  col = "#0072B2",
                  labels = FALSE,
                  add = TRUE)
    }
    if(length(ccw_excl) > 0){
      plot.angles(ccw_excl,
                  type = "yaw",
                  col = "#D55E00",
                  labels = FALSE,
                  add = TRUE)
    }
    
    plot.angles(0,
                type = "yaw",
                col = "transparent",
                main = "Clockwise\nyaw set",
                labels = FALSE)
    if(length(cw_yaws) > 0){
      plot.angles(cw_yaws,
                  type = "yaw",
                  col = "#0072B2",
                  labels = FALSE,
                  add = TRUE)
    }
    if(length(cw_excl) > 0){
      plot.angles(cw_excl,
                  type = "yaw",
                  col = "#D55E00",
                  labels = FALSE,
                  add = TRUE)
    }
    
    graphics::par(
      fig = c(0, 1, 0, 1),
      mar = c(0, 0, 0, 0),
      new = TRUE
    )
    graphics::plot.new()
    graphics::legend(
      "bottom",
      legend = c("Retained", "Excluded"),
      col = c("#0072B2", "#D55E00"),
      lwd = 2,
      horiz = TRUE,
      bty = "n",
      x.intersp = 0.8,
      y.intersp = 0.8,
      cex = 0.8
    )
    
  }
  
  # return trimmed mutually-consistent sets
  return(list(trimmed_ccw_yaws = ccw_yaws,
              trimmed_cw_yaws = cw_yaws))
}
