#' Find azimuths that can project a known 2D elevation, given known view inclination and candidate elevations
#'
#' @param candidate.azimuths 
#' @param candidate.elevations 
#' @param elevation2d 
#' @param view_inclination 
#' @param plot 
#' @param branch 
#'
#' @returns
#' @export
#'
#' @examples
find.candidate.azimuths <- function(candidate.azimuths,
                           candidate.elevations,
                           elevation2d,
                           view_inclination,
                           plot = TRUE,
                           branch = "both"){
  # 1) Sort & unique the ranges
  candidate.azimuths   <- sort(unique(candidate.azimuths))
  candidate.elevations <- sort(unique(candidate.elevations))
  
  # 2) Convert candidate.elevations → radians (vectorized)
  deg2rad       <- pi / 180
  elevation_rad   <- candidate.elevations * deg2rad
  elevation2d_rad   <- elevation2d * deg2rad
  view_inclination_rad    <- view_inclination  * deg2rad
  
  # 3) Precompute K, sr, cr, R, φ (all constants w.r.t elevation loop)
  K   <- tan(elevation2d_rad)
  sr  <- sin(view_inclination_rad)
  cr  <- cos(view_inclination_rad)
  R   <- sqrt(K^2 + sr^2)             # constant
  φ   <- atan2(-sr, K)                # constant
  
  # 4) For each elevation_rad, compute D = cr * tan(elevation_rad) and Δ = acos(D / R)
  D_vec <- cr * tan(elevation_rad)
  ratio <- D_vec / R
  # mark anything outside [-1,1] as impossible → NA
  valid <- abs(ratio) <= 1
  Δ_vec <- rep(NA_real_, length(ratio))
  Δ_vec[valid] <- acos(ratio[valid])
  
  # 5) Compute both possible azimuth angles in radians, then convert to deg
  y_plus_rad  <- φ + Δ_vec
  y_minus_rad <- φ - Δ_vec
  rad2deg     <- 180 / pi
  y_plus_deg  <- y_plus_rad  * rad2deg
  y_minus_deg <- y_minus_rad * rad2deg
  
  # 6) Wrap each into [–180, +180]
  y_plus_adj  <- ((y_plus_deg  + 180) %% 360) - 180
  y_minus_adj <- ((y_minus_deg + 180) %% 360) - 180
  
  # 7) Build a data.frame of (elevation, both azimuth branches)
  df_pos <- data.frame(
    elevation = candidate.elevations,
    azimuth   = y_plus_adj
  )
  df_neg <- data.frame(
    elevation = candidate.elevations,
    azimuth   = y_minus_adj
  )
  
  # 8) Depending on “branch”, stack
  if (branch == "pos") {
    df_full <- df_pos
  } else if (branch == "neg") {
    df_full <- df_neg
  } else {  # "both"
    df_full <- rbind(df_pos, df_neg)
  }
  
  # 9) Filter out NAs and then keep only those rows whose
  #    rounded(elevation) ∈ rounded(candidate.elevations) AND 
  #    rounded(azimuth)   ∈ rounded(candidate.azimuths).
  df_full <- stats::na.omit(df_full)
  keep <- (round(df_full$elevation) %in% round(candidate.elevations)) &
    (round(df_full$azimuth)   %in% round(candidate.azimuths))
  df2 <- df_full[keep, ]
  
  # 10) Optionally plot “azimuth vs elevation” curves + the accepted points
  if (isTRUE(plot)) {
    # Create a dense grid of elevation angles
    p_min <- max(-90, min(candidate.elevations))
    p_max <- min( 90, max(candidate.elevations))
    p_seq <- seq(from = p_min, to = p_max, length.out = 1e3)
    p_seq_rad <- p_seq * deg2rad
    
    # For each p_seq, compute the two azimuth branches (vectorized):
    D_seq    <- cr * tan(p_seq_rad)
    ratio2   <- D_seq / R
    valid2   <- abs(ratio2) <= 1
    Δ_seq    <- rep(NA_real_, length(ratio2))
    Δ_seq[valid2] <- acos(ratio2[valid2])
    y1_seq   <- (φ + Δ_seq) * rad2deg
    y2_seq   <- (φ - Δ_seq) * rad2deg
    
    y1_adj   <- ((y1_seq + 180) %% 360) - 180
    y2_adj   <- ((y2_seq + 180) %% 360) - 180
    
    # Begin an empty plot
    plot(NULL,
         xlim = range(candidate.elevations),
         ylim = range(candidate.azimuths),
         xlab = " elevation",
         ylab = " azimuth",
         main = "All possible azimuth vs elevation curves",
         type = "n"
    )
    
    # Draw “+” branch if requested
    if (branch == "pos" || branch == "both") {
      lines(p_seq, y1_adj)
    }
    # Draw “–” branch if requested
    if (branch == "neg" || branch == "both") {
      lines(p_seq, y2_adj)
    }
    # Overlay all valid points from df2
    points(df2$elevation, df2$azimuth, col = "blue", pch = 16)
  }
  
  # 11) Return the unique set of azimuth that survived filtering
  return(sort(unique(df2$azimuth)))
}