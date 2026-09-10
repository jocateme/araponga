#' Find 3D orientations compatible with an observed 2D pitch
#'
#' Evaluates combinations of candidate 3D pitch, yaw, and view elevation angles using [pitch2d.from.3d()]
#' and returns those whose projected 2D pitch is compatible with an observed 2D pitch. `find.pitch()` and
#' `find.yaw()` are convenience wrappers for finding compatible 3D pitch and yaw orientations,
#' respectively.
#'
#' @param pitch2d Either a numeric scalar returned by [pitch2d.from.xy()] or a numeric vector of
#'  length > 1, in degrees in the interval (-180, 180]. When a scalar is supplied, landmark labeling
#'  uncertainty is incorporated using [pitch2d.w.error()]. When a vector is supplied, the smallest
#'  continuous angular interval containing those values is treated as the range of candidate 2D pitch
#'  angles.
#' @param find Character vector specifying which angle(s) `find.3d()` should return. One or more of
#'  "pitch", "yaw", "view_elevation", and "pitch2d", or "all" to return all four. Default is "all".
#' @param candidate_view_elevations Numeric vector of candidate camera elevation angles relative to the
#'  object, in degrees in the interval \[-90, 90\]. Convention: `-90` = seen from straight below,
#'  `0` = eye level, `90` = seen from straight above. By default, the entire \[-90, 90\] grid is
#'  evaluated at `default_step` resolution.
#' @param candidate_pitches Numeric vector of candidate 3D pitch angles, in degrees in the interval
#'  (-180, 180]. Convention: `90` = pointed up, `0` = horizontally aligned, `-90` =
#'  pointed down. By default, a \[-90, 90\] grid is evaluated at `default_step` resolution. Values
#'  outside \[-90, 90\] may be supplied explicitly when an extended pitch representation is desired.
#' @param candidate_yaws Numeric vector of candidate yaw angles, in degrees in the
#'  interval (-180, 180]. Convention: `0` = pointed right, `90` = pointed straight away, `-90` = pointed
#'  straight toward, `180` = pointed left. By default, the entire (-180, 180] grid is evaluated at
#'  `default_step` resolution.
#' @param label_error Positive numeric scalar specifying the error used to perturb each landmark
#'  coordinate, in the same units as the coordinates supplied to [pitch2d.from.xy()] (e.g., pixels).
#'  Passed internally to [pitch2d.w.error()]. Required when `length(pitch2d) == 1`.
#' @param label_nsamp Positive integer scalar specifying the approximate number of landmark-error
#'  combinations evaluated internally by [pitch2d.w.error()]. Used when `length(pitch2d) == 1`. Default
#'  is `625`.
#' @param default_step Positive numeric scalar specifying the step size, in degrees, used to generate
#'  default candidate-angle vectors. Must evenly divide 180 to ensure full coverage of default candidate
#'  ranges. Default is `1` (integers). Has no effect on candidate vectors supplied explicitly by the
#'  user.
#' @param max_combinations Positive numeric scalar specifying the maximum total number of candidate
#'  pitch-yaw-view elevation combinations to evaluate, in order to prevent excessive memory use and
#'  computation. Default is `1e8`. Searches exceeding this value stop before candidate combinations are
#'  evaluated. Set to `Inf` to disable this limit.
#' @param paired Logical scalar used by `find.pitch()` and `find.yaw()`. If `TRUE`, returns a
#'  `data.frame` of yaws mapped to pitches; if `FALSE` (default), a vector of yaws or pitches.
#'
#' @returns A `data.frame` with all or a subset of the following columns:
#' \describe{
#'   \item{pitch}{numeric: pitch angles, in degrees.}
#'   \item{yaw}{numeric: yaw angles, in degrees.}
#'   \item{view_elevation}{numeric: view elevation angles, in degrees.}
#'   \item{pitch2d}{numeric: projected 2D pitch angles, in degrees.}
#' }
#' Only combinations compatible with the observed 2D pitch and supplied candidate constraints are
#' returned. If no combinations are compatible, the returned `data.frame` has zero rows.
#' 
#' For `find.pitch(..., paired = FALSE)` and `find.yaw(..., paired = FALSE)`, an numeric vector of
#' unique pitch or yaw angles compatible with the provided arguments.
#' 
#' @details
#' For each unique combination of candidate angles, `find.3d()` (and wrappers) uses
#' [`pitch2d.from.3d()`] to calculate the 2D pitch resulting from rotations by pitch, then yaw, then
#' view elevation. It then retains combinations whose projected 2D pitch falls within the smallest
#' continuous angular interval containing the candidate 2D pitch angles.
#' 
#' The default pitch range is \[-90, 90\], which provides a conventional down-to-up representation.
#' Explicit candidate pitches may extend over (-180, 180]. Such extended pitch values should be used
#' intentionally because they necessarily have equivalent representations involving a different yaw
#' (e.g., a pitch of 135° at a yaw of 10° is equivalent to a pitch of 45° at the opposite yaw
#' of -170°).
#' 
#' Candidate angles supplied explicitly are evaluated exactly and are not rounded. For candidate
#' vectors left at their defaults, `default_step` determines the resolution of the search.
#'
#' @examples
#' # pitch2d from hypothetical pixel coordinates
#' p2d <- pitch2d.from.xy(10, 1, -12, 20)
#' 
#' # pitches that project to `p2d` (± 2 pixel error) if seen from 30° (± 1° error) below
#' find.pitch(
#'   p2d,
#'   candidate_view_elevations = -29:-31,
#'   label_error = 2
#'   )
#'
#' # same returned values as from:
#' find.3d(
#'   p2d,
#'   find = "pitch",
#'   candidate_view_elevations = -29:-31,
#'   label_error = 2
#'   )
#'
#' # similar to above, now given yaw between 5° and 15°
#' find.pitch(
#'   pitch2d = p2d,
#'   candidate_view_elevations = -29:-31,
#'   candidate_yaws = 5:15,
#'   label_error = 2
#'   )
#'
#' # yaws that project to `p2d` (± 2 pixel error) if seen from 10° (± 2° error) below
#' find.yaw(
#'   p2d,
#'   candidate_view_elevations = -8:-12,
#'   label_error = 2
#'   )
#'
#' @seealso [pitch2d.from.3d()], [pitch2d.from.xy()], [pitch2d.w.error()], [rotate3d()],
#'  [summarize.yaws()]
#' @rdname find.3d
#' @export
find.3d <- function(pitch2d,
                    find = "all",
                    candidate_view_elevations = seq(-90, 90, default_step),
                    candidate_pitches = seq(-90, 90, default_step),
                    candidate_yaws = seq(-180 + default_step, 180, default_step),
                    label_error,
                    label_nsamp = 625,
                    default_step = 1,
                    max_combinations = 1e8){
  
  ## --- pitch2d ---
  if (missing(pitch2d) || length(pitch2d) == 0) {
    stop("`pitch2d` must be provided.", call. = FALSE)
  }
  if (!is.numeric(pitch2d) || any(!is.finite(pitch2d))) {
    stop("`pitch2d` must be a finite numeric vector.", call. = FALSE)
  }
  if (any(pitch2d <= -180 | pitch2d > 180)) {
    stop("`pitch2d` must satisfy -180 < value <= 180 degrees.", call. = FALSE)
  }
  
  ## --- find ---
  find <- unique(unname(find))
  allowed_find <- c("all", "pitch", "yaw", "view_elevation", "pitch2d")
  if (!is.character(find) || length(find) == 0 || anyNA(find)) {
    stop("`find` must be a character vector.", call. = FALSE)
  }
  bad_find <- setdiff(find, allowed_find)
  if (length(bad_find) > 0) {
    stop(sprintf(
      "`find` contains invalid value(s): %s. Allowed values are: %s.",
      paste(shQuote(bad_find), collapse = ", "),
      paste(shQuote(allowed_find), collapse = ", ")
    ), call. = FALSE)
  }
  if ("all" %in% find && length(find) > 1) {
    stop("`\"all\"` cannot be combined with other values in `find`.",
         call. = FALSE)
  }
  if(identical(find, "all")) find <- c("pitch", "yaw", "view_elevation", "pitch2d")
  
  ## --- default_step ---
  if (!is.numeric(default_step) ||
      length(default_step) != 1 ||
      !is.finite(default_step) ||
      default_step <= 0 ||
      default_step > 180) {
    stop("`default_step` must be a finite numeric scalar > 0 and <= 180.",
         call. = FALSE)
  }
  
  n_steps <- 180 / default_step
  
  if (abs(n_steps - round(n_steps)) > 1e-8) {
    stop(
      "`default_step` must evenly divide 180 degrees.",
      call. = FALSE
    )
  }
  
  ## --- candidate angle sets ---
  if (!is.numeric(candidate_view_elevations) ||
      length(candidate_view_elevations) == 0 ||
      any(!is.finite(candidate_view_elevations))) {
    stop("`candidate_view_elevations` must be a non-empty finite numeric vector.",
         call. = FALSE)
  }
  if (any(candidate_view_elevations < -90 | candidate_view_elevations > 90)) {
    stop("`candidate_view_elevations` must satisfy -90 <= value <= 90 degrees.", call. = FALSE)
  }
  
  if (!is.numeric(candidate_pitches) ||
      length(candidate_pitches) == 0 ||
      any(!is.finite(candidate_pitches))) {
    stop("`candidate_pitches` must be a non-empty finite numeric vector.",
         call. = FALSE)
  }
  if (any(candidate_pitches <= -180 | candidate_pitches > 180)) {
    stop("`candidate_pitches` must satisfy -180 < value <= 180 degrees.", call. = FALSE)
  }
  
  if (!is.numeric(candidate_yaws) ||
      length(candidate_yaws) == 0 ||
      any(!is.finite(candidate_yaws))) {
    stop("`candidate_yaws` must be a non-empty finite numeric vector.",
         call. = FALSE)
  }
  if (any(candidate_yaws <= -180 | candidate_yaws > 180)) {
    stop("`candidate_yaws` must satisfy -180 < value <= 180 degrees.", call. = FALSE)
  }
  
  ## --- label_error ---
  if (length(pitch2d) == 1) {
    if (missing(label_error)) {
      stop("`label_error` is required when `length(pitch2d) == 1`.", call. = FALSE)
    }
    if (!is.numeric(label_error) || length(label_error) != 1 || !is.finite(label_error)) {
      stop("`label_error` must be a finite numeric scalar.", call. = FALSE)
    }
    if (label_error <= 0) {
      stop("`label_error` must be > 0.", call. = FALSE)
    }
  } else {
    if (!missing(label_error) && (!is.numeric(label_error) || length(label_error) != 1 || !is.finite(label_error) || label_error <= 0)) {
      stop("If supplied, `label_error` must be a positive finite numeric scalar.", call. = FALSE)
    }
  }
  
  ## --- label_nsamp ---
  if (!is.numeric(label_nsamp) || length(label_nsamp) != 1 || !is.finite(label_nsamp) ||
      label_nsamp < 1 || abs(label_nsamp - round(label_nsamp)) > 1e-8) {
    stop("`label_nsamp` must be a positive integer scalar.", call. = FALSE)
  }
  label_nsamp <- as.integer(round(label_nsamp))
  
  ## --- max_combinations ---
  if (!is.numeric(max_combinations) ||
      length(max_combinations) != 1 ||
      is.na(max_combinations) ||
      max_combinations <= 0) {
    stop(
      "`max_combinations` must be a positive numeric scalar.",
      call. = FALSE
    )
  }
  
  ## --- pitch2d uncertainty ---
  if(length(pitch2d) > 1){
    pitch2d_w_error <- pitch2d
  } else {
    pitch2d_w_error <- pitch2d.w.error(pitch2d = pitch2d,
                                       label_error = label_error,
                                       label_nsamp = label_nsamp)
  }
  
  ## -- compatible pitch2d interval --
  summ <- summarize.yaws(pitch2d_w_error, tie_action = "error")
  summ$from <- summ$from - 1e-4
  summ$to <- summ$to + 1e-4
  
  ## --- candidate combinations ---
  candidate_pitches <- unique(candidate_pitches)
  candidate_yaws <- unique(candidate_yaws)
  candidate_view_elevations <- unique(candidate_view_elevations)
  
  ny <- length(candidate_yaws)
  np <- length(candidate_pitches)
  ne <- length(candidate_view_elevations)
  
  n_combinations <-
    as.double(ny) *
    as.double(np) *
    as.double(ne)
  
  if (n_combinations > max_combinations) {
    stop(
      sprintf(
        paste0(
          "The requested candidate angles produce %.0f combinations, ",
          "which exceeds `max_combinations = %.0f`. ",
          "Use narrower candidate ranges, a larger `default_step`, ",
          "or increase `max_combinations` intentionally."
        ),
        n_combinations,
        max_combinations
      ),
      call. = FALSE
    )
  }

  ## --- chunk by angle with longest vector ---
  
  candidate_angles <- list(
    yaw = candidate_yaws,
    pitch = candidate_pitches,
    view_elevation = candidate_view_elevations
  )
  
  n_candidates <- lengths(candidate_angles)
  chunk_angle <- names(which.max(n_candidates))
  chunk_values <- candidate_angles[[chunk_angle]]
  
  collected <- vector("list", length(chunk_values))
  
  for(i in seq_along(chunk_values)){
    
    chunk_candidates <- candidate_angles
    chunk_candidates[[chunk_angle]] <- chunk_values[i]
    
    grid <- do.call(
      expand.grid,
      c(
        chunk_candidates,
        list(KEEP.OUT.ATTRS = FALSE)
      )
    )
    grid$pitch2d <- pitch2d.from.3d(grid$pitch,
                                    grid$yaw,
                                    grid$view_elevation)
    
    if(summ$wrap){
      keep <- is.finite(grid$pitch2d) &
        ((grid$pitch2d >= summ$from & grid$pitch2d <= 180) |
        (grid$pitch2d > -180 & grid$pitch2d <= summ$to))
    } else {
      keep <- is.finite(grid$pitch2d) &
        grid$pitch2d >= summ$from &
        grid$pitch2d <= summ$to
    }
    
    collected[[i]] <- unique(grid[keep, find, drop = FALSE])
    
  }
  
  collected <- unique(do.call(rbind, collected))
  rownames(collected) <- NULL
  
  return(as.data.frame(collected))
  
}
#' @rdname find.3d
#' @export
find.yaw <- function(pitch2d,
                     candidate_view_elevations = seq(-90, 90, default_step),
                     candidate_pitches = seq(-90, 90, default_step),
                     candidate_yaws = seq(-180 + default_step, 180, default_step),
                     paired = FALSE,
                     label_error,
                     label_nsamp = 625,
                     default_step = 1,
                     max_combinations = 1e8){
  
  if(!(is.logical(paired) && length(paired) == 1 && !is.na(paired))){
    stop("`paired` must be a logical scalar.", call. = FALSE)
  }
  
  if(paired){
    find = c("pitch", "yaw")
  } else {
    find = "yaw"
  }
  
  df <- find.3d(pitch2d = pitch2d,
                find = find,
                candidate_pitches = candidate_pitches,
                candidate_yaws = candidate_yaws,
                candidate_view_elevations = candidate_view_elevations,
                label_error = label_error,
                label_nsamp = label_nsamp,
                default_step = default_step,
                max_combinations = max_combinations)
  
  if(paired){
    df <- df[order(df$pitch, df$yaw),]
    return(df)
  } else {
    return(sort(unique(df$yaw)))
  }
  
}
#' @rdname find.3d
#' @export
find.pitch <- function(pitch2d,
                       candidate_view_elevations = seq(-90, 90, default_step),
                       candidate_pitches = seq(-90, 90, default_step),
                       candidate_yaws = seq(-180 + default_step, 180, default_step),
                       paired = FALSE,
                       label_error,
                       label_nsamp = 625,
                       default_step = 1,
                       max_combinations = 1e8){
  
  if(!(is.logical(paired) && length(paired) == 1 && !is.na(paired))){
    stop("`paired` must be a logical scalar.", call. = FALSE)
  }
  
  if(paired){
    find = c("yaw", "pitch")
  } else {
    find = "pitch"
  }
  
  df <- find.3d(pitch2d = pitch2d,
                find = find,
                candidate_pitches = candidate_pitches,
                candidate_yaws = candidate_yaws,
                candidate_view_elevations = candidate_view_elevations,
                label_error = label_error,
                label_nsamp = label_nsamp,
                default_step = default_step,
                max_combinations = max_combinations)
  
  if(paired){
    df <- df[order(df$yaw, df$pitch),]
    return(df)
  } else {
    return(sort(unique(df$pitch)))
  }
  
}