#' Find pitches that can project a known 2D pitch, given known view elevation and candidate yaws
#'
#' @param candidate.pitches
#' @param candidate.yaws
#' @param pitch2d 
#' @param view_elevation
#' @param plot 
#' @param return 
#'
#' @returns
#' @export
#'
#' @examples
find.candidate.pitches <- function(candidate.pitches,
                                      candidate.yaws,
                                      pitch2d,
                                      view_elevation,
                                      plot = TRUE, 
                                      return = "pitch"){
  
  candidate.pitches <- sort(unique(candidate.pitches))
  candidate.yaws <- sort(unique(candidate.yaws))
  
  pitch_all <- find.pitch(pitch2d = pitch2d,
                                  yaw = candidate.yaws,
                                  view_elevation = view_elevation)
  
  
  df <- data.frame(
    yaw   = candidate.yaws,
    pitch = pitch_all
  )
  
  keep <- (round(df$pitch) %in% round(candidate.pitches)) &
    (round(df$yaw)   %in% round(candidate.yaws))
  df2 <- df[keep, ]
  
  # 5) Optionally plot: the curve pitch = f(yaw) plus the valid points in blue
  if (isTRUE(plot)) {
    yr_min <- max(-180, min(candidate.yaws))
    yr_max <- min( 180, max(candidate.yaws))
    pr_min <- min(candidate.pitches)
    pr_max <- max(candidate.pitches)
    
    # Because find.pitch is vectorized, we can just do a dense seq of y_s:
    y_seq   <- seq(from = yr_min, to = yr_max, length.out = 1e3)
    p_curve <- find.pitch(pitch2d = pitch2d, yaw = y_seq, view_elevation = view_elevation)
    
    # Plot the continuous curve
    plot(x = y_seq, y = p_curve,
         type = "l",
         xlim = range(candidate.yaws),
         ylim = c(pr_min, pr_max),
         xlab = "yaw", ylab = "pitch",
         main = " pitch vs  yaw (fixed pitch2d & view_elevation)")
    # Overlay all valid points in blue
    points(df2$yaw, df2$pitch, col = "blue", pch = 16)
  }
  
  # 6) Return the unique sorted pitch values that survived
  if(return == "pitch"){
    return(sort(unique(df2$pitch)))
  }
  if(return == "both"){
    return(unique(df2))
  }
}