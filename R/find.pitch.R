#' Find 3D pitches that project to a known 2D pitch (given yaw and view elevation)
#'
#' Given a measured 2D pitch and one or more candidate yaw orientations, compute the
#' corresponding 3D pitch angles that would produce that projected 2D pitch at a given view
#' elevation.
#'
#' @param pitch2d Numeric scalar: observed 2D pitch, in degrees, in the interval (-180, 180];
#'  e.g., from [pitch2d.from.xy()].
#' @param view_elevation Numeric scalar: angle between camera and object, in degrees, in the
#'  interval \[-90, 90\]. Convention: `-90` = seen from straight below, `0` = eye level,
#'  `90` = seen from straight above.
#' @param yaws Optional numeric vector of one known or multiple candidate yaw angles, in degrees, in the
#'  interval (-180, 180]. Convention: `0` = pointed right, `90` = pointed straight away,
#'  `-90` = pointed straight toward, `180` = pointed left. If `NULL` (default), a grid
#'  comprehending all possible yaws (`seq(-179.99, 180, 0.01)`) is used.
#' @param pitches Optional numeric vector of candidate 3D pitches used to filter the results,
#'  in degrees, in the interval \[-90, 90\]. Convention: `90` = pointed up, `0` = horizontally
#'  aligned, `-90` = pointed down. If `NULL` (default) all computed pitches are returned.
#' @param round2keep Integer scalar: number of decimal places to pass to [round()] when
#'  comparing equality between computed and candidate pitches/yaws. Default is `0`, i.e.,
#'  equality is checked after rounding computed and candidate values to integers.
#' @param plot Logical scalar. If `TRUE`, the function plots the full continuous pitch-by-yaw
#'  curve, overlays lines for the selected `yaws`/`pitches`, and points for the returned
#'  combinations.
#'
#' @returns A `data.frame` with two columns:
#' \describe{
#'   \item{yaws}{numeric: the (input) yaw values retained after filtering, in degrees.}
#'   \item{pitches}{numeric: the corresponding computed 3D pitches, in degrees. Convention: `90` = pointed up, `0` = horizontally
#'  aligned, `-90` = pointed down.}
#' }
#' If no (yaw, pitch) matches are found the returned data.frame has zero rows.
#' 
#' @details
#' The function [find.pitch()] in practice finds which solutions of [pitch2d.from.3d()] reproduce `pitch2d`.
#' 
#' More specifically, [pitch2d.from.3d()] constructs the full rotation matrix \eqn{R} using
#' [rotate3d()] and computes the projected 2D pitch \eqn{p_{2}} as
#' \deqn{p_{2} = \operatorname{atan2}\!\big(R_{2,1},\,R_{1,1}\big),}
#' 
#' where:
#' \deqn{R_{2,1} = \sin(p_{3})\cos(r_{3}) + \sin(r_{3})\sin(y_{3})\cos(p_{3}),}
#' \deqn{R_{1,1} = \cos(p_{3})\cos(y_{3}),}
#' 
#' \eqn{r_{3}} is `view_elevation`, \eqn{y_{3}} is a candidate/known `yaw`, and \eqn{p_{3}} is
#' the 3D pitch we are interested in.
#'
#' We find \eqn{p_{3}} by inverting this relation:
#' \deqn{p_{3} = \operatorname{atan2}(\tan(p_{2})\cos(y_{3}) - \sin(r_{3})\sin(y_{3}), \cos(r_{3}))}
#' 
#' This expression is implemented inside [find.pitch()].
#'
#' @examples
#' # pitches that project to 10° seen from 15° below
#' find.pitch(10, view_elevation = -15, plot = TRUE)
#' 
#' # similar to above, but now given known yaw of 30°
#' find.pitch(10, view_elevation = -15, yaws = 30, plot = TRUE)
#'
#' # similar to above, but now given range of candidate yaws instead (20 to 45°)
#' find.pitch(10, view_elevation = -15, yaws = seq(20, 45, 0.1), plot = TRUE)
#'
#' # similar to above, but now restricting (with integer rounding) to range of candidate
#' # pitches 0 to 15°
#' find.pitch(10, view_elevation = -15, yaws = seq(20, 45, 0.1), pitches = seq(0, 15, 0.1),
#'            plot = TRUE)
#'
#' @seealso [pitch2d.from.xy()], [find.yaw()], [pitch2d.from.3d()]
#' @export

find.pitch <- function(pitch2d,
                       view_elevation,
                       yaws = NULL,
                       pitches = NULL,
                       round2keep = 0,
                       plot = FALSE){
  
  ## ---- input checks ----
  if (!is.numeric(pitch2d) || length(pitch2d) != 1) {
    stop("`pitch2d` must be a numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(view_elevation) || length(view_elevation) != 1) {
    stop("`view_elevation` must be a numeric scalar.", call. = FALSE)
  }
  if (!is.null(yaws) && !is.numeric(yaws)) {
    stop("`yaws` must be numeric or NULL.", call. = FALSE)
  }
  if (!is.null(pitches) && !is.numeric(pitches)) {
    stop("`pitches` must be numeric or NULL.", call. = FALSE)
  }
  if (!is.logical(plot) || length(plot) != 1) {
    stop("`plot` must be a logical scalar.", call. = FALSE)
  }
  if (!is.na(round2keep)) {
    if (!is.finite(round2keep) || length(round2keep) != 1) stop("`round2keep` must be a single finite number or NA.", call. = FALSE)
    round2keep <- as.integer(round2keep)
  }
  
  if(pitch2d <= -180 | pitch2d > 180){
    stop("`pitch2d` must satisfy -180 < `pitch` <= 180 degrees.")
  }
  if(view_elevation < -90 | view_elevation > 90){
    stop("`view_elevation` must satisfy -90 <= `view_elevation` <= 90 degrees.")
  }
  if(!is.null(yaws) & any(yaws <= -180 | yaws > 180)){
    stop("`yaws` must satisfy -180 < yaw <= 180 degrees.", call. = FALSE)
  }
  if(!is.null(pitches) & any(pitches < -90 | pitches > 90)){
    stop("`pitches` must satisfy -90 <= pitch <= 90 degrees.", call. = FALSE)
  }
  
  null.yaw <- FALSE
  null.pitch <- FALSE
  
  if(is.null(yaws)){
    null.yaw <- TRUE
    yaws <- seq(-179.99, 180, 0.01)
  }
  yaws <- sort(unique(as.numeric(yaws)))
  if(!is.null(pitches)) pitches <- sort(unique(as.numeric(pitches)))
    
  pitch2d_rad <- deg2rad(pitch2d)
  yaws_rad <- deg2rad(yaws)
  view_elevation_rad <- deg2rad(view_elevation)
  
  ## compute candidate 3D pitch for each yaw
  p3 <- atan2(tan(pitch2d_rad) * cos(yaws_rad) - sin(view_elevation_rad) * sin(yaws_rad),
              cos(view_elevation_rad))
  p3 <- rad2deg(p3)
  p3 <- ((p3 + 180) %% 360) - 180
  
  df <- data.frame(yaws = yaws,
                   pitches = p3)
  
  ## filtering
  if(is.null(pitches)){
    null.pitch <- TRUE
    pitches <- df$pitches
  }
  keep <- (round(df$pitches, round2keep) %in% round(pitches, round2keep)) &
    (round(df$yaws, round2keep) %in% round(yaws, round2keep))
  df <- df[keep, ]
  
  # package convention
  df$yaws[df$yaws == -180] <- 180
  
  ## plotting
  if(plot){
    
    yaws_plot <- deg2rad(seq(-179.9, 180, 0.1))
    graphics::plot(y = rad2deg(atan2(tan(pitch2d_rad) * cos(yaws_plot) -
                                       sin(view_elevation_rad) * sin(yaws_plot),
                                     cos(view_elevation_rad))),
         x = rad2deg(yaws_plot),
         type = "l",
         xlab = "yaw (\u00B0)",
         ylab = "pitch (\u00B0)")
    if(!null.yaw){
      graphics::abline(v = yaws,
                       col = "#5F9EA066")
    }
    if(!null.pitch){
      graphics::abline(h = pitches,
                       col = "#5F9EA066")
    }
    graphics::points(df,
                     col = "darkblue",
                     pch = 16)
    
  }
  
  return(df)
  
}