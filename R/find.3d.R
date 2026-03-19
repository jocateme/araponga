#' Find combinations of 3D orientations that project to an observed 2D pitch
#'
#' Using precomputed simulation data, recover the combinations of yaw, pitch, and view elevations
#' (at 1° resolution) that produce a given observed 2D pitch. `find.pitch()` and `find.yaw()` are
#' convenient wrappers for two common uses: finding 3D pitch and yaw, respectively.
#'
#' @param pitch2d Either a numeric scalar returned by [pitch2d.from.xy()] or a numeric
#'  vector of >1 candidate 2D pitch angles (e.g., as returned by [pitch2d.w.error()]), in degrees in the
#'  interval (-180, 180].
#' @param find Character vector. Which angle(s) to return between "pitch", "yaw", "view_elevation", and
#'  "pitch2d". Default (`NULL`) returns all.
#' @param candidate_view_elevations Optional numeric vector: known or candidate camera elevation angle(s)
#'  relative to the object, in degrees, in the interval \[-90, 90\]. Convention: `-90` = seen from
#'  straight below, `0` = eye level, `90` = seen from straight above. Default is `NULL` (all view
#'  elevations considered). Provided values are rounded to the nearest integer.
#' @param candidate_pitches Optional numeric vector: known or candidate (3D) pitch angle(s), in degrees,
#'  in the interval \[-90, 90\]. Convention: `90` = pointed up, `0` = horizontally aligned, `-90` =
#'  pointed down. Default is `NULL` (all pitches considered). Provided values are rounded to the nearest
#'  integer.
#' @param candidate_yaws Optional numeric vector: known or candidate yaw angle(s), in degrees, in the
#'  interval (-180, 180]. Convention: `0` = pointed right, `90` = pointed straight away, `-90` = pointed
#'  straight toward, `180` = pointed left. Default is `NULL` (all yaws considered). Provided values are
#'  rounded to the nearest integer.
#' @param label_error Positive numeric scalar specifying the error (± pixels) used to perturb landmark
#'  coordinates. Passed internally to [pitch2d.w.error()]. Optional if `length(pitch2d)` > 1.
#' @param label_nsamp Positive integer scalar specifying the approximate number of grid combinations to
#'  evaluate. Passed internally to [pitch2d.w.error()]. Optional if `length(pitch2d)` > 1.
#' @param sim_download Logical scalar. If `TRUE`, the function will attempt to download the precomputed
#'  simulation dataset automatically if it is not found in the local cache. Default is `FALSE`
#'  (CRAN-friendly).
#' @param paired Logical scalar. If `TRUE`, a `data.frame` of yaws mapped to pitches will be returned; if
#'  `FALSE` (default), a vector of yaws or pitches.
#'
#' @return A `data.frame` with all or a subset of the following columns:
#' \describe{
#'   \item{pitch}{integer: pitch angles (degrees) compatible with the provided arguments.}
#'   \item{yaw}{integer: yaw angles (degrees) compatible with the provided arguments.}
#'   \item{pitch2d}{numeric: projected 2D pitch angles (degrees) compatible with the provided arguments.}
#'   \item{view_elevation}{integer: elevation angles (degrees) compatible with the provided arguments.}
#' }
#' If no matches are found, the returned data.frame has zero rows.
#' 
#' For `find.pitch(..., paired = FALSE)` and `find.yaw(..., paired = FALSE)`, an integer vector of
#' pitch or yaw angles compatible with the provided arguments.
#' 
#' @details
#' The lookup is based on a precomputed Blender simulation grid. A 3D pyramid was generated and rotated
#' by pitch, then yaw, then roll (equivalently to what [rotate3d()] does) for all 11,793,960
#' integer combinations of pitch (`-90:90`), yaw (`-179:180`), and roll (`-90:90`). For each combination
#' the base midpoint and tip were projected to 2D pixel coordinates with an orthographic camera.
#' The cached simulation table stores the resulting projected geometry and derived 2D pitch values;
#' `find.3d()` (and wrappers) then filter that table for combinations compatible with the requested 2D
#' pitch and any optional candidate constraints.
#'
#' @examples
#' # hypothetical pitch2d from coordinates
#' p2d <- pitch2d.from.xy(10, 1, -12, 20)
#' 
#' # pitches that project to `p2d` (± 2 pixel error) if seen from 30° (± 1° error) below
#' find.pitch(p2d, candidate_view_elevations = -29:-31, label_error = 2)
#' # same returned values as from:
#' find.3d(p2d, find = "pitch", candidate_view_elevations = -29:-31, label_error = 2)
#' # similar to above, now given yaw between 5° and 15°
#' find.pitch(pitch2d = p2d, candidate_view_elevations = -29:-31, candidate_yaws = 5:15,
#' label_error = 2)
#'
#' # yaws that project to `p2d` (± 2 pixel error) if seen from 10° (± 2° error) below
#' find.yaw(p2d, candidate_view_elevations = -8:-12, label_error = 2)
#'
#'
#' @importFrom rlang .data
#' @seealso [pitch2d.from.xy()], [pitch2d.w.error()], [rotate3d()], [download.simdata()]
#' @rdname find.3d
#' @export
find.3d <- function(pitch2d,
                    find = NULL,
                    candidate_view_elevations = NULL,
                    candidate_pitches = NULL,
                    candidate_yaws = NULL,
                    label_error,
                    label_nsamp = 625,
                    sim_download = FALSE){
  
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
  allowed_find <- c("pitch", "yaw", "view_elevation", "pitch2d")
  if (!is.null(find)) {
    if (!is.character(find) || length(find) == 0 || anyNA(find)) {
      stop("`find` must be NULL or a character vector.", call. = FALSE)
    }
    bad_find <- setdiff(find, allowed_find)
    if (length(bad_find) > 0) {
      stop(sprintf(
        "`find` contains invalid value(s): %s. Allowed values are: %s.",
        paste(shQuote(bad_find), collapse = ", "),
        paste(shQuote(allowed_find), collapse = ", ")
      ), call. = FALSE)
    }
  }
  
  ## --- candidate angle sets ---
  if (!is.null(candidate_view_elevations)) {
    if (!is.numeric(candidate_view_elevations) || any(!is.finite(candidate_view_elevations))) {
      stop("`candidate_view_elevations` must be a finite numeric vector or NULL.", call. = FALSE)
    }
    if (any(candidate_view_elevations < -90 | candidate_view_elevations > 90)) {
      stop("`candidate_view_elevations` must satisfy -90 <= value <= 90 degrees.", call. = FALSE)
    }
  }
  
  if (!is.null(candidate_pitches)) {
    if (!is.numeric(candidate_pitches) || any(!is.finite(candidate_pitches))) {
      stop("`candidate_pitches` must be a finite numeric vector or NULL.", call. = FALSE)
    }
    if (any(candidate_pitches < -90 | candidate_pitches > 90)) {
      stop("`candidate_pitches` must satisfy -90 <= value <= 90 degrees.", call. = FALSE)
    }
  }
  
  if (!is.null(candidate_yaws)) {
    if (!is.numeric(candidate_yaws) || any(!is.finite(candidate_yaws))) {
      stop("`candidate_yaws` must be a finite numeric vector or NULL.", call. = FALSE)
    }
    if (any(candidate_yaws <= -180 | candidate_yaws > 180)) {
      stop("`candidate_yaws` must satisfy -180 < value <= 180 degrees.", call. = FALSE)
    }
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
  
  ## --- sim_download ---
  if(!(is.logical(sim_download) && length(sim_download) == 1 && !is.na(sim_download))){
    stop("`sim_download` must be a logical scalar.", call. = FALSE)
  }
  
  dest_dir <- tools::R_user_dir("araponga", "cache")
  if(!dir.exists(file.path(dest_dir, "sim_data_parquet"))){
    if(!sim_download){
      stop("Simulation dataset not found locally. Either set `sim_download = TRUE` or run",
      " `download.simdata()` to download the dataset (207MB, one-time).")
    }
    
    download.simdata()
  }
  
  sim_data <- arrow::open_dataset(file.path(dest_dir, "sim_data_parquet"))
  
  if(length(pitch2d) > 1){
    pitch2d_w_error <- pitch2d
  } else {
    pitch2d_w_error <- pitch2d.w.error(pitch2d = pitch2d,
                                       label_error = label_error,
                                       label_nsamp = label_nsamp)
  }
  
  summ <- summarize.yaws(pitch2d_w_error)
  summ$from <- summ$from - 1e-4
  summ$to <- summ$to + 1e-4
  
  if(summ$wrap){
    filters <- list(dplyr::expr((.data$pitch2d >= !!summ$from & .data$pitch2d <= 180) |
                             (.data$pitch2d > -180 & .data$pitch2d <= !!summ$to)))
  } else {
    filters <- list(dplyr::expr(.data$pitch2d >= !!summ$from & .data$pitch2d <= !!summ$to))
  }
  
  if(!is.null(candidate_pitches)){
    filters <- c(filters,
                 dplyr::expr(.data$pitch %in% !!as.integer(round(candidate_pitches))))
  }
  
  if(!is.null(candidate_view_elevations)){
    filters <- c(filters,
                 dplyr::expr(.data$view_elevation %in% !!as.integer(round(candidate_view_elevations))))
  }
  
  if(!is.null(candidate_yaws)){
    filters <- c(filters,
                 dplyr::expr(.data$yaw %in% !!as.integer(round(candidate_yaws))))
  }
  
  sim_data <- sim_data |> dplyr::filter(!!!filters)
  if(!is.null(find)){
    sim_data <- sim_data |> dplyr::select(!!!dplyr::syms(find))
  }
  collected <- dplyr::collect(sim_data)
  collected <- collected[!duplicated(collected),]
  return(as.data.frame(collected))
  
}
#' @rdname find.3d
#' @export
find.yaw <- function(pitch2d,
                     candidate_view_elevations = NULL,
                     candidate_pitches = NULL,
                     candidate_yaws = NULL,
                     paired = FALSE,
                     label_error,
                     label_nsamp = 625,
                     sim_download = FALSE){
  
  if(!(is.logical(paired) && length(paired) == 1 && !is.na(paired))){
    stop("`paired` must be a logical scalar.", call. = FALSE)
  }
  
  if(paired){
    find = c("pitch", "yaw")
  } else {
    find <- "yaw"
  }
  
  df <- find.3d(pitch2d = pitch2d,
                find = find,
                candidate_pitches = candidate_pitches,
                candidate_yaws = candidate_yaws,
                candidate_view_elevations = candidate_view_elevations,
                label_error = label_error,
                label_nsamp = label_nsamp,
                sim_download = sim_download)
  
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
                       candidate_pitches = NULL,
                       candidate_yaws = NULL,
                       candidate_view_elevations = NULL,
                       paired = FALSE,
                       label_error,
                       label_nsamp = 625,
                       sim_download = FALSE){
  
  if(!(is.logical(paired) && length(paired) == 1 && !is.na(paired))){
    stop("`paired` must be a logical scalar.", call. = FALSE)
  }
  
  if(paired){
    find = c("yaw", "pitch")
  } else {
    find <- "pitch"
  }
  
  df <- find.3d(pitch2d = pitch2d,
                find = find,
                candidate_pitches = candidate_pitches,
                candidate_yaws = candidate_yaws,
                candidate_view_elevations = candidate_view_elevations,
                label_error = label_error,
                label_nsamp = label_nsamp,
                sim_download = sim_download)
  
  if(paired){
    df <- df[order(df$yaw, df$pitch),]
    return(df)
  } else {
    return(sort(unique(df$pitch)))
  }
  
}