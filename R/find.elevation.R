#' Find the pitch that projects a known 2D pitch, given known yaw and and view elevation
#'
#' @param pitch2d
#' @param yaw
#' @param view_elevation
#'
#' @returns pitch pitch angle, in degrees
#' @export
#'
#' @examples
find.pitch <- function(pitch2d,
                           yaw,
                           view_elevation){

  pitch2d_rad <- deg2rad(pitch2d)
  yaw_rad <- deg2rad(yaw)
  view_elevation_rad <- deg2rad(view_elevation)
  
  K  <- tan(pitch2d_rad)
  cy <- cos(yaw_rad)
  sy <- sin(yaw_rad)
  sr <- sin(view_elevation_rad)
  cr <- cos(view_elevation_rad)
  
  p3_rad <- atan2(K*cy-sr*sy, cr)
  
  p3_deg <- rad2deg(p3_rad)
  p3_adj <- ((p3_deg + 180) %% 360) - 180
  
  return(p3_adj)
}