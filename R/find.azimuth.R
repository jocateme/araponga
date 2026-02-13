#' Find the azimuth that projects a known 2D elevation, given known elevation and view inclination
#'
#' @param elevation2d
#' @param elevation
#' @param view_inclination
#'
#' @returns
#' @export
#'
#' @examples
find.azimuth <- function(elevation2d,
                         elevation,
                         view_inclination){
  # Step 1: convert all angles exactly once to radians
  deg2rad     <- pi / 180
  elevation2d_rad <- elevation2d * deg2rad
  elevation_rad <- elevation * deg2rad
  view_inclination_rad  <- view_inclination  * deg2rad
  
  # Step 2: compute K, sr, cr (all scalar or vectorized)
  K  <- tan(elevation2d_rad)
  sr <- sin(view_inclination_rad)
  cr <- cos(view_inclination_rad)
  
  # Step 3: compute D, R, φ
  D <- cr * tan(elevation_rad)          # = cr * tan(elevation)
  R <- sqrt(K^2 + sr^2)               # always ≥ 0
  φ <- atan2(-sr, K)                  # the “base angle”
  
  # Step 4: compute Δ = arccos( D/R )
  ratio <- D / R
  # mark anything outside [-1,1] as impossible → NA
  valid <- abs(ratio) <= 1
  Δ_rad <- rep(NA_real_, length(ratio))
  Δ_rad[valid] <- acos(ratio[valid])
  
  # Step 5: two possible azimuth solutions
  y_plus_rad  <- φ + Δ_rad
  y_minus_rad <- φ - Δ_rad
  
  # Step 6: convert back to degrees and wrap into [–180, +180]
  rad2deg    <- 180 / pi
  y1_deg     <- y_plus_rad  * rad2deg
  y2_deg     <- y_minus_rad * rad2deg
  
  azimuth1_adj <- ((y1_deg + 180) %% 360) - 180
  azimuth2_adj <- ((y2_deg + 180) %% 360) - 180
  
  return(c(azimuth1_adj, azimuth2_adj))
}