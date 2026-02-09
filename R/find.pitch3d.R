#' Find the 3D pitch that projects a known 2D pitch, given known 3D yaw and roll
#'
#' @param pitch2d numeric; known 2D pitch angle, in degrees; e.g., from pitch2d_from_xy
#' @param yaw3d numeric; known 3D yaw angle, in degrees
#' @param roll3d numeric; known 3D roll angle, in degrees
#'
#' @returns 3D pitch angle, in degrees
#' @export
#'
#' @examples
find.pitch3d <- function(pitch2d,
                         yaw3d,
                         roll3d){

  pitch2d_rad <- deg2rad(pitch2d)
  yaw3d_rad <- deg2rad(yaw3d)
  roll3d_rad <- deg2rad(roll3d)
  
  K  <- tan(pitch2d_rad)
  cy <- cos(yaw3d_rad)
  sy <- sin(yaw3d_rad)
  sr <- sin(roll3d_rad)
  cr <- cos(roll3d_rad)
  
  p3_rad <- atan2(K*cy-sr*sy, cr)
  
  p3_deg <- rad2deg(p3_rad)
  p3_adj <- ((p3_deg + 180) %% 360) - 180
  
  return(p3_adj)
}