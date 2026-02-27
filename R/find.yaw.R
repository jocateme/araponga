#' Find 3D yaws that project to a known 2D pitch (given 3D pitch and view elevation)
#'
#' Given a measured 2D pitch and one or more candidate pitch orientations, compute the
#' corresponding yaw angles that would produce that projected 2D pitch at a given view elevation.
#'
#' @param pitch2d Numeric scalar: observed 2D pitch, in degrees, in the interval (-180, 180];
#'  e.g., from [pitch2d.from.xy()].
#' @param view_elevation Numeric scalar: angle between camera and object, in degrees, in the
#'  interval \[-90, 90\]. Convention: `-90` = seen from straight below, `0` = eye level,
#'  `90` = seen from straight above.
#' @param pitches Optional numeric vector of one known or multiple candidate pitch angles, in
#'  degrees, in the interval \[-90, 90\]. Convention: `90` = pointed up, `0` = horizontally
#'  aligned, `-90` = pointed down. If `NULL` (default), a grid comprehending all possible
#'  pitches (`seq(-90, 90, 0.1)`) is used.
#' @param yaws Optional numeric vector of candidate 3D yaws used to filter the
#'  results, in degrees, in the interval (-180, 180]. Convention: `0` = pointed right,
#'  `90` = pointed straight away, `-90` = pointed straight toward, `180` = pointed left. If
#'  `NULL` (default) all computed yaws are returned.
#' @param round2keep Integer scalar: number of decimal places to pass to [round()] when comparing
#'  equality between computed and candidate pitches/yaws. Default is `0`, i.e., equality is
#'  checked after rounding computed and candidate values to integers.
#' @param plot Logical scalar. If `TRUE`, the function draws a yaw-by-pitch plot with the
#'  returned combinations.
#'
#' @returns A `data.frame` with two columns:
#' \describe{
#'   \item{pitches}{numeric: the (input) pitch values, in degrees, retained after filtering}
#'   \item{yaws}{numeric: the corresponding computed 3D yaws, in degrees (0 = pointed right,
#'   90 = pointed straight away, -90 = pointed straight toward, 180 = pointed left).}
#' }
#' If no (pitch, yaw) matches are found the returned data.frame has zero rows.
#' 
#' @details
#' In practice [find.yaw()] finds which solutions of [pitch2d.from.3d()] reproduce `pitch2d`.
#' 
#' Computationally, [pitch2d.from.3d()] constructs the full rotation matrix \eqn{R} using
#' [rotate3d()] and computes the projected 2D pitch \eqn{p_{2}} as
#' \deqn{p_{2} = \operatorname{atan2}\!\big(R_{2,1},\,R_{1,1}\big)}
#'
#' By inversing this relation, we find both yaws (\eqn{y_{3}}) that project `pitch2d`
#' (\eqn{p_{2}}) from `view_elevation` (\eqn{r_{3}}) given a candidate/known `pitch`
#' (\eqn{p_{3}}):
#' \deqn{\phi=\operatorname{atan2}(-\sin(r_{3}),\,\tan(p_{2}))}
#' \deqn{y_{3} = \phi \pm \arccos\!\left(\dfrac{\cos(r_{3})\;\tan(p_{3})}{\sqrt{\tan(p_{2})^{2}+\sin(r_{3})^{2}}}\right)}
#' 
#' This expression is equivalent to the code inside [find.yaw()].
#'
#' @examples
#' # yaws that projects to 10° seen from 15° below
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
#' @seealso [pitch2d.from.xy()], [find.pitch()], [pitch2d.from.3d()]
#' @export
find.yaw <- function(pitch2d,
                     view_elevation,
                     pitches = NULL,
                     yaws = NULL,
                     round2keep = 0,
                     plot = FALSE){
  
  ## ---- input checks ----
  if (!is.numeric(pitch2d) || length(pitch2d) != 1) stop("`pitch2d` must be a numeric scalar.", call. = FALSE)
  if (!is.numeric(view_elevation) || length(view_elevation) != 1) stop("`view_elevation` must be a numeric scalar.", call. = FALSE)
  if (!is.null(pitches) && !is.numeric(pitches)) stop("`pitches` must be numeric or NULL.", call. = FALSE)
  if (!is.null(yaws) && !is.numeric(yaws)) stop("`yaws` must be numeric or NULL.", call. = FALSE)
  if (!is.numeric(round2keep) || length(round2keep) != 1) stop("`round2keep` must be a single numeric value.", call. = FALSE)
  if (!is.logical(plot) || length(plot) != 1) stop("`plot` must be a logical scalar.", call. = FALSE)
  
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
  
  if(is.null(pitches)){
    null.pitch <- TRUE
    pitches <- seq(-90, 90, 0.1)
  }
  pitches <- sort(unique(pitches))
  if(!is.null(yaws)) yaws <- sort(unique(as.numeric(yaws)))
  
  pitch2d_rad <- deg2rad(pitch2d)
  pitches_rad <- deg2rad(pitches)
  view_elevation_rad <- deg2rad(view_elevation)
  
  ## precompute helpers
  K <- tan(pitch2d_rad)
  sr <- sin(view_elevation_rad)
  cr <- cos(view_elevation_rad)
  R <- sqrt(K^2 + sr^2)
  phi <- atan2(-sr, K)
  d_vec <- cr * tan(pitches_rad)
  ratio <- d_vec / R
  # ratio must be in [-1, 1] to get real acos - flagging for exclusion
  valid <- abs(ratio) <= 1
  d_vec <- rep(NA_real_, length(ratio))
  d_vec[valid] <- acos(ratio[valid])
  
  y_plus <- rad2deg(phi + d_vec)
  y_minus <- rad2deg(phi - d_vec)
  
  y_plus <- ((y_plus  + 180) %% 360) - 180
  y_minus <- ((y_minus  + 180) %% 360) - 180
  
  df <- data.frame(
    pitches = c(pitches, pitches),
    yaws = c(y_minus, y_plus)
  )
  
  df <- df[!is.na(df$yaws),]
  
  ## filtering
  if(is.null(yaws)){
    null.yaw <- TRUE
    yaws <- df$yaws
  }
  keep <- (round(df$pitches, round2keep) %in% round(pitches, round2keep)) &
    (round(df$yaws, round2keep) %in% round(yaws, round2keep))
  df <- df[keep, ]
  
  ## plotting
  if(plot){
    
    yaws_plot <- deg2rad(seq(-180, 180, 0.1))
    graphics::plot(df,
         type = "p",
         xlab = "pitch (\u00B0)",
         ylab = "yaw (\u00B0)",
         pch = 16,
         col = "darkred")
    
  }
  
  return(df)
  
}