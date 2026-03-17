#' Find combinations of 3D orientations that project to an observed 2D pitch
#'
#' Using precomputed simulation data, recover the combinations of yaw, pitch, and view elevations
#' (at 1° resolution) that produce a given observed 2D pitch. `find.pitch()` and `find.yaw()` are
#' wrappers for two common uses: finding 3D pitch and yaw, respectively.
#'
#' @param pitch2d Either an observed 2D pitch angle as returned by [pitch2d.from.xy()] or a numeric
#'  vector of >1 candidate 2D pitch angles (e.g., as returned by [pitch2d.w.error()]), in degrees in the
#'  interval (-180, 180].
#' @param find Character vector. Which angle(s) to return between "pitch", "yaw", "view_elevation", and
#'  "pitch2d".
#' @param candidate_view_elevations Optional numeric vector: known or candidate angle(s) between camera
#'  and object, in degrees, in the interval \[-90, 90\]. Convention: `-90` = seen from straight below,
#'  `0` = eye level, `90` = seen from straight above. Default is `NULL` (all view elevations considered).
#' @param candidate_pitches Optional numeric vector: known or candidate (3D) pitch angle(s), in degrees, in
#'  the interval \[-90, 90\]. Convention: `90` = pointed up, `0` = horizontally aligned, `-90` = pointed
#'  down. Default is `NULL` (all pitches considered).
#' @param candidate_yaws Optional numeric vector: known or candidate yaw angle(s), in degrees, in the
#'  interval (-180, 180]. Convention: `0` = pointed right, `90` = pointed straight away, `-90` = pointed
#'  straight toward, `180` = pointed left. Default is `NULL` (all yaws considered).
#' @param label_error Positive numeric scalar specifying the error (± pixels) in landmark
#'  labeling. Passed internally to [pitch2d.w.error()]. Optional if `length(pitch2d) > 1`.
#' @param label_nsamp Integer scalar (default 625). Number of Monte Carlo draws to simulate error in
#'  pitch2d calculation. Passed internally to [pitch2d.w.error()]. Optional if `length(pitch2d) > 1`.
#' @param sim_download Logical scalar. If `TRUE`, the function will attempt to download the precomputed
#'  simulation dataset automatically if it is not found in the local cache. Default is `FALSE`
#'  (CRAN-friendly).
#' @param paired Logical scalar. If `TRUE`, a list of yaws mapped to pitches will be returned; if `FALSE`
#'  (default), a vector of yaws or pitches.
#'
#' @returns (UPDATE) A `data.frame` with two columns:
#' \describe{
#'   \item{pitches}{numeric: the (input) pitch values, in degrees, retained after filtering}
#'   \item{yaws}{numeric: the corresponding computed 3D yaws, in degrees (0 = pointed right,
#'   90 = pointed straight away, -90 = pointed straight toward, 180 = pointed left).}
#' }
#' If no (pitch, yaw) matches are found the returned data.frame has zero rows.
#' 
#' @details
#' (UPDATE) The function [find.pitch()] in practice finds which solutions of [pitch2d.from.3d()] reproduce `pitch2d`.
#' 
#' More specifically, [pitch2d.from.3d()] constructs the full rotation matrix \eqn{R} using
#' [rotate3d()] and computes the projected 2D pitch \eqn{p_{2}} as
#' \deqn{p_{2} = \operatorname{atan2}\!\big(R_{2,1},\,R_{1,1}\big),}
#' 
#' where:
#' \deqn{R_{2,1} = \sin(p_{3})\cos(r_{3}) + \sin(r_{3})\sin(y_{3})\cos(p_{3}),}
#' \deqn{R_{1,1} = \cos(p_{3})\cos(y_{3}),}
#' 
#' \eqn{r_{3}} is `view_elevation`, \eqn{p_{3}} is a candidate/known 3D `pitch`, and \eqn{y_{3}}
#' is the yaw we are interested in.
#'
#' We find \eqn{y_{3}} by inverting this relation:
#' \deqn{\phi=\operatorname{atan2}(-\sin(r_{3}),\,\tan(p_{2}))}
#' \deqn{y_{3} = \phi \pm \arccos\!\left(\dfrac{\cos(r_{3})\;\tan(p_{3})}{\sqrt{\tan(p_{2})^{2}+\sin(r_{3})^{2}}}\right)}
#' 
#' This expression is implemented inside [find.yaw()].
#'
#' @examples
#' # (UPDATE) 
#' # yaws that project to 10° seen from 15° below
#' find.yaw(pitch2d = 10, view_elevation = -15, plot = TRUE)
#' 
#' # similar to above, but now given known pitch of 5°
#' find.yaw(pitch2d = 10, view_elevation = -15, pitches = 5, plot = TRUE)
#'
#' # similar to above, but now given range of candidate pitches instead (0 to 35°)
#' find.yaw(pitch2d = 10, view_elevation = -15, pitches = seq(0, 30, 0.1), plot = TRUE)
#'
#' # similar to above, but now restricting (integer rounding) to range of candidate yaws (0 to 15°)
#' find.yaw(pitch2d = 10, view_elevation = -15, pitches = seq(0, 30, 0.1),
#'            yaws = seq(0, 15, 0.1), plot = TRUE)
#'
#' @seealso [pitch2d.from.xy()], [pitch2d.w.error()]
#' @rdname find.3d
#' @export
find.3d <- function(pitch2d,
                    find,
                    candidate_view_elevations = NULL,
                    candidate_pitches = NULL,
                    candidate_yaws = NULL,
                    label_error,
                    label_nsamp = 625,
                    sim_download = FALSE){
  
  dest_dir <- tools::R_user_dir("araponga", "cache")
  if(!dir.exists(file.path(dest_dir, "sim_data_parquet"))){
    if(!sim_download){
      stop("Simulation dataset not found locally. Either set `sim_download = TRUE` or run \n
           `download.simdata()` to download the dataset (207MB, one-time).")
    }
    
    zip_path <- file.path(dest_dir, "sim_data_parquet.zip")
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
  
  filters <- list()
  
  if(summ$wrap){
    filters <- dplyr::expr((pitch2d > !!summ$from & pitch2d <= 180) |
                             (pitch2d > -180 & pitch2d <= !!summ$to))
  } else {
    filters <- dplyr::expr(pitch2d >= !!summ$from & pitch2d <= !!summ$to)
  }
  
  if(!is.null(candidate_pitches)){
    filters <- c(filters,
                 dplyr::expr(pitch %in% !!as.integer(round(candidate_pitches))))
  }
  
  if(!is.null(candidate_view_elevations)){
    filters <- c(filters,
                 dplyr::expr(view_elevation %in% !!as.integer(round(candidate_view_elevations))))
  }
  
  if(!is.null(candidate_yaws)){
    filters <- c(filters,
                 dplyr::expr(yaw %in% !!as.integer(round(candidate_yaws))))
  }
  
  sim_data <- sim_data %>% dplyr::filter(!!!filters)
  
  sim_data <- sim_data %>% dplyr::select(!!!dplyr::syms(find))
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
  
  if(paired){
    find = c("pitch", "yaw")
  } else {
    find <- "yaw"
  }
  
  s <- Sys.time()
  df <- find.3d(pitch2d = pitch2d,
                find = find,
                candidate_pitches = candidate_pitches,
                candidate_yaws = candidate_yaws,
                candidate_view_elevations = candidate_view_elevations,
                label_error = label_error,
                sim_download = sim_download)
  Sys.time()-s
  
  if(paired){
    df <- df[order(df$pitch, df$yaw),]
    # list <- split(df$yaw,
    #               df$pitch)
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
                sim_download = sim_download)
  
  if(paired){
    df <- df[order(df$yaw, df$pitch),]
    # list <- split(df$pitch,
    #               df$yaw)
    return(df)
  } else {
    return(sort(unique(df$pitch)))
  }
  
}