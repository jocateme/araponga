#' Visualize angles
#'
#' @description
#' Plotting helper for quick visual inspection of angles, in particular pitches, yaws, and view
#' elevations.
#'
#' @param angles Set(s) of angles to be plotted, in degrees in the range (-180, 180]. If a numeric vector,
#'  one plot is generated. If a `list` or a `data.frame`, `length(angles)` plots are generated,
#'  customized when `names(angles)` match `"pitch"`, `"yaw"`, or `"view_elevation"`. Compatible with
#'  outputs from [find.3d()], [find.pitch()], and [find.yaw()].
#' @param type When `angles` is a vector, optional character choice between `"pitch"`, `"yaw"`, and
#'  `"view_elevation"` that determines the kind of visualization to be generated. If `NULL` (default),
#'  a generic plot is generated.
#' @param facing Character choice between `"right"` (default) or `"left"`: which direction the object
#'  should be facing for `pitch` and `view_elevation`.
#' @param col Optional vector of colors to draw angle segments in each plot. Recycled to match
#'  `length(angles)` when `angles` is not a vector.
#' @param main Optional character vector: titles for each plot. Recycled to match `length(angles)` when
#'  `angles` is not a vector.
#' @param labels Logical scalar (default `TRUE`): should inset labels be plotted?
#' @param add Logical scalar (default `FALSE`): should angle segments be added to the current plotting
#'  device?
#' 
#' @examples
#' # calls with a vector
#' ## pitch between 60 and 70
#' plot.angles(60:70, "pitch")
#' ## view_elevation between -30 and -40
#' plot.angles(-30:-40, "view_elevation")
#' 
#' if(interactive()){
#' # call with output from find.3d
#' ## pitches and yaws that project to 10° (± 1° error) if seen from 15° below
#' df <- find.3d(9:11, find = c("pitch", "yaw"), candidate_view_elevations = -15)
#' plot.angles(df)
#' }
#' 
#' # call with named list
#' list <- list(pitch = 10:30, yaw = -100:-45, view_elevation = -10:-15)
#' plot.angles(list)
#' 
#' @seealso [find.3d()]
#' @export plot.angles
plot.angles <- function(angles = NULL,
                        type = NULL,
                        facing = c("right", "left"),
                        col = NULL,
                        main = NULL,
                        labels = TRUE,
                        add = FALSE){
  
  if (is.null(angles) || length(angles) == 0) {
    stop("`angles` must be provided (numeric vector, list, or data.frame).", call. = FALSE)
  }
  
  facing <- match.arg(facing)
  
  if(is.numeric(angles)){
    angles <- list(angles)
    if(!is.null(type)){
      type <- match.arg(type, c("pitch", "yaw", "view_elevation"))
      names(angles) <- type
    }
  } else {
    angles <- as.list(angles)
  }
  
  tmp <- unlist(angles)
  if(any(is.na(tmp))){
    angles <- lapply(angles, stats::na.omit)
    tmp <- stats::na.omit(tmp)
    warning("NA `angles` ignored.", call. = FALSE)
  }
  if(any(tmp <= -180 | tmp > 180)){
    stop("All `angles` must satisfy -180 < angle <= 180 degrees.", call. = FALSE)
  }
  if(any(!is.numeric(tmp))){
    stop("`angles` must be numeric.", call. = FALSE)
  }
  
  if(is.null(names(angles))){
    names(angles) <- rep("", length(angles))
  }
  
  
  n <- length(angles)
  if(!is.null(col)) col <- rep(col, length.out = n)
  if(!is.null(main)) main <- rep(main, length.out = n)
  
  plot.pitch <- function(pitches,
                         main = NULL,
                         col = NULL,
                         facing,
                         labels,
                         add){

    pitches <- deg2rad(pitches)
    
    if(is.null(main)) main <- "pitch\nside view"
    if(is.null(col)) col <- "cadetblue"
    
    if(!add){
      if(any(abs(pitches) > pi/2)){
        
        theta <- seq(0, 2*pi, length.out = 400)
        
      } else {
        
        if(facing == "right"){
          theta <- seq(-90, 90, length.out = 200) * pi/180
        } else {
          theta <- seq(90, 270, length.out = 200) * pi/180
          pitches <- pi - pitches
        }
        
      }
      
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
                     main = main)
    }
    
    if(!add){
      graphics::segments(0, 0,
                         0 + 1 * cos(pitches),
                         0 + 1 * sin(pitches),
                         col = col,
                         lwd = 0.5)
      graphics::segments(1-2*(facing == "left"), 0,
                         0, 0,
                         lty = 2,
                         col = "#BEBEBE80")
      graphics::segments(0, -1,
                         0, 1,
                         lty = 2,
                         col = "#BEBEBE80")
      if(any(abs(pitches) > pi/2)){
        
        graphics::segments(0, 0,
                           -1+2*(facing == "left"), 0,
                           lty = 2,
                           col = "#BEBEBE80")
        graphics::text(x = cos(pi-(facing=="left")*pi),
                       y = sin(pi-(facing=="left")*pi),
                       labels = "180\u00B0",
                       pos = 2+(facing=="left")*2)
        
      }
      graphics::text(x = c(cos(-pi/2), cos(0+(facing=="left")*pi), cos(pi/2)),
                     y = c(sin(-pi/2), sin(0+(facing=="left")*pi), sin(pi/2)),
                     labels = c("-90\u00B0", "0\u00B0 (horizon)", "90\u00B0"),
                     pos = c(1, 4-(facing=="left")*2, 3))
      if(labels){
        graphics::text(0, 0, "base", pos = 2+(facing=="left")*2)
        graphics::text(mean(cos(pitches)),
                       mean(sin(pitches)),
                       "tip",
                       pos = 4-(facing=="left")*2)
      }
    }
    
  }
  
  plot.yaw <- function(yaws,
                       main = NULL,
                       col = NULL,
                       labels,
                       add){
    
    yaws <- deg2rad(yaws)
    
    if(is.null(main)) main <- "yaw\noverhead view"
    if(is.null(col)) col <- "salmon"
    
    if(!add){
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
                     main = main)
    }
    
    graphics::segments(0, 0,
                       0 + 1 * cos(yaws),
                       0 + 1 * sin(yaws),
                       col = col,
                       lwd = 0.5)
    if(!add){
      graphics::segments(-1, 0,
                         1, 0,
                         lty = 2,
                         col = "#BEBEBE80")
      graphics::segments(0, -1,
                         0, 1,
                         lty = 2,
                         col = "#BEBEBE80")
      graphics::text(x = c(cos(-pi/2), cos(pi), cos(pi/2), cos(0)),
                     y = c(sin(-pi/2), sin(pi), sin(pi/2), sin(0)),
                     labels = c("-90\u00B0 (camera)", "180\u00B0", "90\u00B0", "0\u00B0"),
                     pos = c(1, 2, 3, 4))
      if(labels){
        graphics::text(0, 0, "base", pos = 2)
        graphics::text(mean(cos(yaws)),
                       mean(sin(yaws)),
                       "tip",
                       pos = 4)
      }
    }
    
  }
  
  plot.elevation <- function(elevations,
                             main = NULL,
                             col = NULL,
                             facing,
                             labels,
                             add){
    
    elevations <- deg2rad(elevations)
    
    if(is.null(main)) main <- "view elevation\nside view"
    if(is.null(col)) col <- "darkolivegreen"
    
    if(!add){
      
      if(facing == "right"){
        theta <- seq(-90, 90, length.out = 200) * pi/180
      } else {
        theta <- seq(90, 270, length.out = 200) * pi/180
        elevations <- pi - elevations
      }
      
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
                     main = main)
      
    }
    
    graphics::segments(0, 0,
                       0 + 1 * cos(elevations),
                       0 + 1 * sin(elevations),
                       col = col,
                       lwd = 0.5)
    if(!add){
      graphics::segments(1-2*(facing == "left"), 0,
                         0, 0,
                         lty = 2,
                         col = "#BEBEBE80")
      graphics::segments(0, -1,
                         0, 1,
                         lty = 2,
                         col = "#BEBEBE80")
      graphics::text(x = c(cos(-pi/2), cos(0+(facing=="left")*pi), cos(pi/2)),
                     y = c(sin(-pi/2), sin(0+(facing=="left")*pi), sin(pi/2)),
                     labels = c("-90\u00B0", "0\u00B0 (eye level)", "90\u00B0"),
                     pos = c(1, 4-(facing=="left")*2, 3))
      if(labels){
        graphics::text(0, 0, "object", pos = 2+(facing=="left")*2)
        graphics::text(mean(cos(elevations)),
                       mean(sin(elevations)),
                       "observer",
                       pos = 4-(facing=="left")*2)
      }
    }
    
  }
  
  plot.generic <- function(angles,
                           main = NULL,
                           col = NULL,
                           add){
    
    
    if(is.null(col)) col <- "gray50"
    
    angles <- deg2rad(angles)
    
    if(!add){
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
                     main = main)
    }

    graphics::segments(0, 0,
                       0 + 1 * cos(angles),
                       0 + 1 * sin(angles),
                       col = col,
                       lwd = 0.5)
    
    if(!add){
      graphics::segments(-1, 0,
                         1, 0,
                         lty = 2,
                         col = "#BEBEBE80")
      graphics::segments(0, -1,
                         0, 1,
                         lty = 2,
                         col = "#BEBEBE80")
      graphics::text(x = c(cos(-pi/2), cos(pi), cos(pi/2), cos(0)),
                     y = c(sin(-pi/2), sin(pi), sin(pi/2), sin(0)),
                     labels = c("-90\u00B0", "180\u00B0", "90\u00B0", "0\u00B0"),
                     pos = c(1, 2, 3, 4))
    }
    
  }
  
  old_xpd <- graphics::par()$xpd
  old_ask <- graphics::par()$ask
  graphics::par(xpd = TRUE)
  on.exit(graphics::par(xpd = old_xpd,
                        ask = old_ask), add = TRUE)
  
  for(i in seq_len(n)){
    
    if(i == 2 && interactive()){
      graphics::par(ask = TRUE)
    }
    
    if(names(angles)[i] == "pitch"){
      plot.pitch(pitches = angles[[i]],
                 main = main[i],
                 col = col[i],
                 facing = facing,
                 labels = labels,
                 add = add)
      next
    }
    
    if(names(angles)[i] == "yaw"){
      plot.yaw(yaws = angles[[i]],
               main = main[i],
               col = col[i],
               labels = labels,
               add = add)
      next
    }
    
    if(names(angles)[i] == "view_elevation"){
      plot.elevation(elevations = angles[[i]],
                     main = main[i],
                     col = col[i],
                     facing = facing,
                     labels = labels,
                     add = add)
      next
    }
    
    plot.generic(angles = angles[[i]],
                 main = main[i],
                 col = col[i],
                 add = add)
    
  }
  
}