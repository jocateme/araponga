#' Simulated error of pitch and yaw estimation
#' 
#' This dataset contains absolute distances (degrees) between actual and estimated pitch and yaw
#' orientations (from [find.pitch()] and [find.yaw()]), considering three sources of error: inherent
#' method error, mismeasurement of view elevation, and landmark labeling inaccuracy.
#'
#' @format ## `sim_data`
#' A data frame with 98,568 rows and 18 columns:
#' \describe{
#'   \item{pitch.real, yaw.real, roll.real}{Known pitch, yaw, and roll (view elevation) orientations, in degrees.}
#'   \item{pitch2d}{2D pitch calculated from known landmarks.}
#'   \item{yaw.error, pitch.error}{Absolute difference between known yaw (or pitch) and closest estimated candidate yaw (or pitch), in degrees.}
#'   \item{yaw.error.roll2.5, pitch.error.roll2.5}{Same as `yaw.error`/`pitch.error`, but after simulating a ±2.5 degree error in roll (view elevation).}
#'   \item{yaw.error.roll5, pitch.error.roll5}{Same as `yaw.error`/`pitch.error`, but after simulating a ±5 degree error in roll (view elevation).}
#'   \item{yaw.error.roll7.5, pitch.error.roll7.5}{Same as `yaw.error`/`pitch.error`, but after simulating a ±7.5 degree error in roll (view elevation).}
#'   \item{yaw.error.label2, pitch.error.label2}{Same as `yaw.error`/`pitch.error`, but after simulating a ±2 pixel error in landmarks.}
#'   \item{yaw.error.label5, pitch.error.label5}{Same as `yaw.error`/`pitch.error`, but after simulating a ±5 pixel error in landmarks.}
#'   \item{yaw.error.label8, pitch.error.label8}{Same as `yaw.error`/`pitch.error`, but after simulating a ±8 pixel error in landmarks.}
#' }
"sim_data"