#' Find the elevation that projects a known 2D elevation, given known azimuth and and view inclination
#'
#' @param elevation2d
#' @param azimuth
#' @param view_inclination
#'
#' @returns elevation elevation angle, in degrees
#' @export
#'
#' @examples
find.elevation <- function(elevation2d,
                           azimuth,
                           view_inclination){

  elevation2d_rad <- deg2rad(elevation2d)
  azimuth_rad <- deg2rad(azimuth)
  view_inclination_rad <- deg2rad(view_inclination)
  
  K  <- tan(elevation2d_rad)
  cy <- cos(azimuth_rad)
  sy <- sin(azimuth_rad)
  sr <- sin(view_inclination_rad)
  cr <- cos(view_inclination_rad)
  
  p3_rad <- atan2(K*cy-sr*sy, cr)
  
  p3_deg <- rad2deg(p3_rad)
  p3_adj <- ((p3_deg + 180) %% 360) - 180
  
  return(p3_adj)
}