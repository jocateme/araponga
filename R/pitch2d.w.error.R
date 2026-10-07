#' Calculate 2D pitch uncertainty from landmark labeling uncertainty
#'
#' @description
#' Calculate the range of projected 2D pitch angles compatible with uncertainty in the locations of two
#' landmarks.
#'
#' @param observed2d One-row `data.frame` of class `araponga2d`, as returned by [project2d.from.xy()].
#' @param label_error Non-negative numeric scalar specifying the maximum labeling error in each landmark
#'  coordinate, in the same units as the coordinates in `observed2d` (e.g., pixels).
#'  
#' @details
#' `label_error` is applied independently to each landmark coordinate. The function returns the smallest
#' continuous interval of projected 2D pitch angles compatible with that uncertainty.
#'
#' If the uncertainty is large enough that all projected directions are possible, the returned interval
#' spans the full circle.
#'  
#' @returns
#' A named list describing the smallest continuous angular interval compatible with the specified labeling
#' error, with components:
#' * `from`: Starting angle of the interval, in degrees.
#' * `to`: Ending angle of the interval, in degrees.
#' * `width`: Width of the interval, in degrees.
#' * `wrap`: Logical indicating whether the interval crosses the `180`/`-180` boundary.
#' * `all`: An empty list, included for consistency with [summarize.yaws()].
#' 
#' If the landmark error region contains the origin in its interior, all 2D pitch angles are possible and
#' the returned interval has `from = -180`, `to = 180`, and `width = 360`.
#' 
#' @examples
#' # Projected vector from hypothetical pixel coordinates
#' observed <- project2d.from.xy(
#'   x_tip = 10, y_tip = 8,
#'   x_base = 11, y_base = 12
#' )
#'
#' # Pitch interval compatible with ±1 pixel labeling uncertainty
#' pitch2d.w.error(observed, label_error = 1)
#'
#' # Pitch interval compatible with ±5 pixel labeling uncertainty (full circle returned)
#' pitch2d.w.error(observed, label_error = 5)
#'
#' @seealso [project2d.from.xy()], [summarize.yaws()]
#' @export
pitch2d.w.error <- function(observed2d,
                            label_error){
  
  ## --- observed2d ---
  if (missing(observed2d) || !inherits(observed2d, "araponga2d")) {
    stop(
      "`observed2d` must be an `araponga2d` object returned by `project2d.from.xy()`.",
      call. = FALSE
    )
  }
  
  if (nrow(observed2d) != 1) {
    stop(
      "`observed2d` must contain exactly one observation.",
      call. = FALSE
    )
  }
  
  required <- c("x_tip", "y_tip", "x_base", "y_base")
  
  if (!all(required %in% names(observed2d))) {
    stop(
      "`observed2d` must contain `x_tip`, `y_tip`, `x_base`, and `y_base`.",
      call. = FALSE
    )
  }
  
  valid_coordinates <- vapply(
    observed2d[required],
    function(x) is.numeric(x) && length(x) == 1 && is.finite(x),
    logical(1)
  )
  
  if (!all(valid_coordinates)) {
    stop(
      "Landmark coordinates in `observed2d` must be finite numeric values.",
      call. = FALSE
    )
  }
  
  ## --- label_error ---
  if (!is.numeric(label_error) ||
      length(label_error) != 1 ||
      !is.finite(label_error) ||
      label_error < 0) {
    stop(
      "`label_error` must be a non-negative finite numeric scalar.",
      call. = FALSE
    )
  }
  
  dx0 <- observed2d$x_tip - observed2d$x_base
  dy0 <- observed2d$y_tip - observed2d$y_base
  
  dx_lim <- dx0 + c(-2, 2) * label_error
  dy_lim <- dy0 + c(-2, 2) * label_error
  
  # full circle case
  origin_inside <-
    dx_lim[1] < 0 && dx_lim[2] > 0 &&
    dy_lim[1] < 0 && dy_lim[2] > 0
  
  if(origin_inside){
    return(list(
      from = -180,
      to = 180,
      width = 360,
      wrap = FALSE,
      all = list())
    )
  }
  
  # corners of the rectangle
  corners <- expand.grid(
    dx = dx_lim,
    dy = dy_lim
  )
  
  pitch2d.all <- .project2d.from.components(corners$dx, corners$dy)$pitch2d
  
  pitch2d.all <- pitch2d.all[!is.na(pitch2d.all)]
  if (length(pitch2d.all) == 0) {
    stop(
      "All simulated landmark combinations produced undefined 2D pitches.",
      call. = FALSE
    )
  }

  summarize.yaws(pitch2d.all, tie_action = "error")
  
}
