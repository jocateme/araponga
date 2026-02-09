#' Find possible 3D yaws that can project a known 2D pitch, given known 3D roll and range of possible 3D pitches
#'
#' @param yaw.range 
#' @param pitch.range 
#' @param pitch2d 
#' @param roll3d 
#' @param plot 
#' @param branch 
#'
#' @returns
#' @export
#'
#' @examples
find.yaw.range <- function(yaw.range,
                           pitch.range,
                           pitch2d,
                           roll3d,
                           plot = TRUE,
                           branch = "both"){
  # 1) Sort & unique the ranges
  yaw.range   <- sort(unique(yaw.range))
  pitch.range <- sort(unique(pitch.range))
  
  # 2) Convert pitch.range → radians (vectorized)
  deg2rad       <- pi / 180
  pitch3d_rad   <- pitch.range * deg2rad
  pitch2d_rad   <- pitch2d * deg2rad
  roll3d_rad    <- roll3d  * deg2rad
  
  # 3) Precompute K, sr, cr, R, φ (all constants w.r.t pitch3d loop)
  K   <- tan(pitch2d_rad)
  sr  <- sin(roll3d_rad)
  cr  <- cos(roll3d_rad)
  R   <- sqrt(K^2 + sr^2)             # constant
  φ   <- atan2(-sr, K)                # constant
  
  # 4) For each pitch3d_rad, compute D = cr * tan(pitch3d_rad) and Δ = acos(D / R)
  D_vec <- cr * tan(pitch3d_rad)
  ratio <- D_vec / R
  # mark anything outside [-1,1] as impossible → NA
  valid <- abs(ratio) <= 1
  Δ_vec <- rep(NA_real_, length(ratio))
  Δ_vec[valid] <- acos(ratio[valid])
  
  # 5) Compute both possible yaw angles in radians, then convert to deg
  y_plus_rad  <- φ + Δ_vec
  y_minus_rad <- φ - Δ_vec
  rad2deg     <- 180 / pi
  y_plus_deg  <- y_plus_rad  * rad2deg
  y_minus_deg <- y_minus_rad * rad2deg
  
  # 6) Wrap each into [–180, +180]
  y_plus_adj  <- ((y_plus_deg  + 180) %% 360) - 180
  y_minus_adj <- ((y_minus_deg + 180) %% 360) - 180
  
  # 7) Build a data.frame of (pitch3d, both yaw branches)
  df_pos <- data.frame(
    pitch3d = pitch.range,
    yaw3d   = y_plus_adj
  )
  df_neg <- data.frame(
    pitch3d = pitch.range,
    yaw3d   = y_minus_adj
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
  #    rounded(pitch3d) ∈ rounded(pitch.range) AND 
  #    rounded(yaw3d)   ∈ rounded(yaw.range).
  df_full <- stats::na.omit(df_full)
  keep <- (round(df_full$pitch3d) %in% round(pitch.range)) &
    (round(df_full$yaw3d)   %in% round(yaw.range))
  df2 <- df_full[keep, ]
  
  # 10) Optionally plot “yaw vs pitch” curves + the accepted points
  if (isTRUE(plot)) {
    # Create a dense grid of pitch angles
    p_min <- max(-90, min(pitch.range))
    p_max <- min( 90, max(pitch.range))
    p_seq <- seq(from = p_min, to = p_max, length.out = 1e3)
    p_seq_rad <- p_seq * deg2rad
    
    # For each p_seq, compute the two yaw branches (vectorized):
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
         xlim = range(pitch.range),
         ylim = range(yaw.range),
         xlab = "3D pitch",
         ylab = "3D yaw",
         main = "All possible yaw vs pitch curves",
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
    points(df2$pitch3d, df2$yaw3d, col = "blue", pch = 16)
  }
  
  # 11) Return the unique set of yaw3d that survived filtering
  return(sort(unique(df2$yaw3d)))
}