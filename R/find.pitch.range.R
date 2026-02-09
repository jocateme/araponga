#' Find possible 3D pitches that can project a known 2D pitch, given known 3D roll and range of possible 3D yaws
#'
#' @param pitch.range 
#' @param yaw.range 
#' @param pitch2d 
#' @param roll3d 
#' @param plot 
#' @param return 
#'
#' @returns
#' @export
#'
#' @examples
find.pitch.range <- function(pitch.range,
                             yaw.range,
                             pitch2d,
                             roll3d,
                             plot = TRUE, 
                             return = "pitch"){
  
  pitch.range <- sort(unique(pitch.range))
  yaw.range   <- sort(unique(yaw.range))
  
  pitch3d_all <- find.pitch3d(pitch2d = pitch2d,
                              yaw3d = yaw.range,
                              roll3d = roll3d)
  
  df <- data.frame(
    yaw3d   = yaw.range,
    pitch3d = pitch3d_all
  )
  
  keep <- (round(df$pitch3d) %in% round(pitch.range)) &
    (round(df$yaw3d)   %in% round(yaw.range))
  df2 <- df[keep, ]
  
  # 5) Optionally plot: the curve pitch3d = f(yaw3d) plus the valid points in blue
  if (isTRUE(plot)) {
    yr_min <- max(-180, min(yaw.range))
    yr_max <- min( 180, max(yaw.range))
    pr_min <- min(pitch.range)
    pr_max <- max(pitch.range)
    
    # Because find.pitch3d is vectorized, we can just do a dense seq of y_s:
    y_seq   <- seq(from = yr_min, to = yr_max, length.out = 1e3)
    p_curve <- find.pitch3d(pitch2d = pitch2d, yaw3d = y_seq, roll3d = roll3d)
    
    # Plot the continuous curve
    plot(x = y_seq, y = p_curve,
         type = "l",
         xlim = range(yaw.range),
         ylim = c(pr_min, pr_max),
         xlab = "yaw", ylab = "pitch",
         main = "3D pitch vs 3D yaw (fixed pitch2d & roll3d)")
    # Overlay all valid points in blue
    points(df2$yaw3d, df2$pitch3d, col = "blue", pch = 16)
  }
  
  # 6) Return the unique sorted pitch3d values that survived
  if(return == "pitch"){
    return(sort(unique(df2$pitch3d)))
  }
  if(return == "both"){
    return(unique(df2))
  }
}