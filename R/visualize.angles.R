#' Visualize pitch and yaw orientations
#'
#' Plotting helper for quick visual inspection of yaw (overhead view) and pitch (side view) orientations.
#'
#' @param angles Optional `data.frame` with numeric columns `pitches` and `yaws`; e.g., the output
#'  of [find.yaw()] or [find.pitch()]. If provided, overrides arguments `pitches` and `yaws`.
#' @param pitches Optional numeric vector of pitch angles, in degrees, in the interval \[-90, 90\].
#' @param yaws Optional numeric vector of yaw angles, in degrees, in the interval (-180, 180].
#' @param facing Character scalar: which direction the object should be facing for the pitch
#'  plot. One of `"right"` (default) or `"left"`.
#' @param col.pitches Color (character) or vector of colors passed to [plot()] to draw pitch
#'  segments. Recycled to match `nrow(angles)` or `length(pitches)`.
#' @param col.yaws Color (character) or vector of colors passed to [plot()] to draw yaw
#'  segments. Recycled to match `nrow(angles)` or `length(yaws)`.
#' @param main.pitches Character scalar: optional title for the pitch plot.
#' @param main.yaws Character scalar: optional title for the yaw plot.
#'  
#' @return Invisibly, a `data.frame` with columns:
#' \describe{
#'   \item{which}{factor: "pitch" or "yaw".}
#'   \item{angle}{numeric: original angle, in degrees.}
#'   \item{x}{numeric: x coordinate plotted.}
#'   \item{y}{numeric: y coordinate plotted.}
#'  }
#'  
#' @examples
#' # pitches that project to 10° seen from 15° below
#' angles <- find.pitch(10, view_elevation = -15)
#' visualize.angles(angles)
#' 
#' # yaws that project to 15° seen from 45° below, given range of candidate pitches 10 to 20°
#' angles <- find.yaw(pitch2d = 15, view_elevation = -45, pitches = seq(10, 20, 0.1))
#' visualize.angles(angles)
#' 
#' # call with explicit vectors
#' visualize.angles(pitches = -30:-20)
#' visualize.angles(yaws = 70:80)
#' 
#' @export
visualize.angles <- function(angles = NULL,
                             pitches = NULL,
                             yaws = NULL,
                             facing = c("right", "left"),
                             col.pitches = "cadetblue",
                             col.yaws = "salmon",
                             main.pitches = NULL,
                             main.yaws = NULL){
  
  if(!is.null(angles)){
    if(!is.data.frame(angles) | !"yaws" %in% colnames(angles) | !"pitches" %in% colnames(angles)){
      stop("`angles` must be a dataframe with columns `yaws` and `pitches`", call. = FALSE)
    }
    yaws <- angles$yaws
    pitches <- angles$pitches
  }
  
  facing <- match.arg(facing)
  
  if (!is.null(pitches) && !is.numeric(pitches)) stop("`pitches` must be numeric.", call. = FALSE)
  if (!is.null(yaws)    && !is.numeric(yaws))    stop("`yaws` must be numeric.",    call. = FALSE)
  
  # remove NAs with a warning
  if (!is.null(pitches) && anyNA(pitches)) {
    warning("`pitches` contains NA; NA entries will be ignored in the plot.", call. = FALSE)
    pitches <- pitches[!is.na(pitches)]
  }
  if (!is.null(yaws) && anyNA(yaws)) {
    warning("`yaws` contains NA; NA entries will be ignored in the plot.", call. = FALSE)
    yaws <- yaws[!is.na(yaws)]
  }
  
  if(any(yaws <= -180 | yaws > 180)){
    stop("`yaws` must satisfy -180 < yaw <= 180 degrees.", call. = FALSE)
  }
  if(any(pitches < -90 | pitches > 90)){
    stop("`pitches` must satisfy -90 <= pitch <= 90 degrees.", call. = FALSE)
  }
  
  if(!is.null(yaws) & !is.null(pitches)){
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mfrow = c(1, 2))
  }
  
  df <- NULL
  
  if(!is.null(pitches)){
    
    df.p <- data.frame(which = "pitch", angle = pitches)
    pitches <- deg2rad(pitches)
    
    if(is.null(main.pitches)) main.pitches <- "pitch\nside view"
    
    if(facing == "right"){
      theta <- seq(-90, 90, length.out = 200) * pi/180
      sides <- c(1, 4, 3)
      adj <- 1
    } else {
      theta <- seq(90, 270, length.out = 200) * pi/180
      pitches <- pi - pitches
      sides <- 1:3
      adj <- -1
    }
    
    df.p$x <- cos(pitches)
    df.p$y <- sin(pitches)
    df <- rbind(df, df.p)
    
    graphics::plot(x = 0 + 1 * cos(theta),
                   y = 0 + 1 * sin(theta),
                   type = "l",
                   asp = 1,
                   xlab = "",
                   ylab = "",
                   bty = "n",
                   xaxt = "n",
                   yaxt = "n",
                   col = "gray50",
                   main = main.pitches)
    graphics::segments(0, 0,
                       0 + 1 * cos(pitches),
                       0 + 1 * sin(pitches),
                       col = col.pitches,
                       lwd = 0.5)
    graphics::segments(0, 0,
                       1*adj, 0,
                       lty = 2,
                       col = "#BEBEBE80")
    graphics::segments(0, -1,
                       0, 1,
                       lty = 2,
                       col = "#BEBEBE80")
    graphics::mtext(text = c("-90\u00B0", "0\u00B0 (horizon)", "90\u00B0"),
                    side = sides,
                    at = 0)
  }
  
  if(!is.null(yaws)){
    
    df.y <- data.frame(which = "yaw",
                       angle = yaws)
    yaws <- deg2rad(yaws)
    df.y$x = cos(yaws)
    df.y$y = sin(yaws)
    df <- rbind(df, df.y)
    
    if(is.null(main.yaws)) main.yaws <- "yaw\noverhead view"
    
    theta <- seq(0, 2*pi, length.out = 400)
    graphics::plot(x = 0 + 1 * cos(theta),
                   y = 0 + 1 * sin(theta),
                   type = "l",
                   asp = 1,
                   xlab = "",
                   ylab = "",
                   bty = "n",
                   xaxt = "n",
                   yaxt = "n",
                   col = "gray50",
                   main = main.yaws)
    graphics::segments(0, 0,
                       0 + 1 * cos(yaws),
                       0 + 1 * sin(yaws),
                       col = col.yaws,
                       lwd = 0.5)
    graphics::segments(-1, 0,
                       1, 0,
                       lty = 2,
                       col = "gray")
    graphics::segments(0, -1,
                       0, 1,
                       lty = 2,
                       col = "gray")
    graphics::mtext(text = c("-90\u00B0 (camera)", "180\u00B0", "90\u00B0", "0\u00B0"),
                    side = 1:4)
    
  }
  
  df$which <- factor(df$which, levels = c("pitch", "yaw"))
  invisible(df)
  
}