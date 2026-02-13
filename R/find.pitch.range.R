#' Find possible elevations that can project a known 2D elevation, given known view inclination and candidate azimuths
#'
#' @param candidate.elevations
#' @param candidate.azimuths
#' @param elevation2d 
#' @param view_inclination
#' @param plot 
#' @param return 
#'
#' @returns
#' @export
#'
#' @examples
find.candidate.elevations <- function(candidate.elevations,
                             candidate.azimuths,
                             elevation2d,
                             view_inclination,
                             plot = TRUE, 
                             return = "elevation"){
  
  candidate.elevations <- sort(unique(candidate.elevations))
  candidate.azimuths <- sort(unique(candidate.azimuths))
    
  elevation_all <- find.elevation(elevation2d = elevation2d,
                                azimuth = candidate.azimuths,
                                view_inclination = view_inclination.range)
  
  
  df <- data.frame(
    azimuth   = candidate.azimuths,
    elevation = elevation_all
  )
  
  keep <- (round(df$elevation) %in% round(candidate.elevations)) &
    (round(df$azimuth)   %in% round(candidate.azimuths))
  df2 <- df[keep, ]
  
  # 5) Optionally plot: the curve elevation = f(azimuth) plus the valid points in blue
  if (isTRUE(plot)) {
    yr_min <- max(-180, min(candidate.azimuths))
    yr_max <- min( 180, max(candidate.azimuths))
    pr_min <- min(candidate.elevations)
    pr_max <- max(candidate.elevations)
    
    # Because find.elevation is vectorized, we can just do a dense seq of y_s:
    y_seq   <- seq(from = yr_min, to = yr_max, length.out = 1e3)
    p_curve <- find.elevation(elevation2d = elevation2d, azimuth = y_seq, view_inclination = view_inclination)
    
    # Plot the continuous curve
    plot(x = y_seq, y = p_curve,
         type = "l",
         xlim = range(candidate.azimuths),
         ylim = c(pr_min, pr_max),
         xlab = "azimuth", ylab = "elevation",
         main = " elevation vs  azimuth (fixed elevation2d & view_inclination)")
    # Overlay all valid points in blue
    points(df2$azimuth, df2$elevation, col = "blue", pch = 16)
  }
  
  # 6) Return the unique sorted elevation values that survived
  if(return == "elevation"){
    return(sort(unique(df2$elevation)))
  }
  if(return == "both"){
    return(unique(df2))
  }
}