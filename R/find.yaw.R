#' Find the yaw that projects a known 2D pitch, given known pitch and view elevation
#'
#' @param pitch2d
#' @param pitch
#' @param view_elevation
#'
#' @returns
#' @export
#'
#' @examples
find.yaw <- function(pitch2d,
                     pitch,
                     view_elevation){
  # Step 1: convert all angles exactly once to radians
  deg2rad     <- pi / 180
  pitch2d_rad <- pitch2d * deg2rad
  pitch_rad <- pitch * deg2rad
  view_elevation_rad  <- view_elevation  * deg2rad
  
  # Step 2: compute K, sr, cr (all scalar or vectorized)
  K  <- tan(pitch2d_rad)
  sr <- sin(view_elevation_rad)
  cr <- cos(view_elevation_rad)
  
  # Step 3: compute D, R, φ
  D <- cr * tan(pitch_rad)          # = cr * tan(pitch)
  R <- sqrt(K^2 + sr^2)               # always ≥ 0
  φ <- atan2(-sr, K)                  # the “base angle”
  
  # Step 4: compute Δ = arccos( D/R )
  ratio <- D / R
  # mark anything outside [-1,1] as impossible → NA
  valid <- abs(ratio) <= 1
  Δ_rad <- rep(NA_real_, length(ratio))
  Δ_rad[valid] <- acos(ratio[valid])
  
  # Step 5: two possible yaw solutions
  y_plus_rad  <- φ + Δ_rad
  y_minus_rad <- φ - Δ_rad
  
  # Step 6: convert back to degrees and wrap into [–180, +180]
  rad2deg    <- 180 / pi
  y1_deg     <- y_plus_rad  * rad2deg
  y2_deg     <- y_minus_rad * rad2deg
  
  yaw1_adj <- ((y1_deg + 180) %% 360) - 180
  yaw2_adj <- ((y2_deg + 180) %% 360) - 180
  
  return(c(yaw1_adj, yaw2_adj))
}