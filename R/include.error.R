#' Lookup and incorporate expected error in pitch or yaw estimation
#'
#' @description
#' Estimate expected pitch and yaw errors from simulation results with `lookup.error()` and expand
#' candidate angle sets to incorporate those errors with `include.error()`.
#'
#' These functions use a simulation dataset (`sim_data`) included with the package that quantifies
#' the discrepancy between true 3D angles and the angles recovered by the projection methods
#' implemented in [find.pitch()] and [find.yaw()]. The simulations can account for three sources of
#' error: inherent method error, mismeasurement of view elevation, and landmark labeling inaccuracy.
#'
#' `lookup.error()` retrieves the expected maximum error from any of these error sources, while
#' `include.error()` both retrieves the maximum error and expands a set of candidate pitch or yaw
#' angles returned by [find.pitch()]/[find.yaw()] so that all values within the expected error
#' range are included. `pitch2d.w.error()` simulates the effect of labeling inaccuracy on `pitch2d`
#' calculation.
#'
#' @param raw Optional. The object returned by [`find.pitch()`] or [`find.yaw()`]. If supplied,
#' overrides the arguments below.
#' @param pitch2d Numeric scalar: observed 2D pitch as returned by [pitch2d.from.xy()], in degrees in
#'  the interval (-180, 180]. Ignored if `raw` is provided.
#' @param view_elevation Numeric scalar: angle between camera and object, in degrees, in the
#'  interval \[-90, 90\]. Convention: `-90` = seen from straight below, `0` = eye level,
#'  `90` = seen from straight above. Ignored if `raw` is provided.
#' @param yaws Optional numeric vector of candidate yaw angles, in degrees, in the
#'  interval (-180, 180]. Convention: `0` = pointed right, `90` = pointed straight away,
#'  `-90` = pointed straight toward, `180` = pointed left. If `NULL` (default), a grid
#'  comprehending all simulated yaws is used. Ignored if `raw` is provided.
#' @param pitches Optional numeric vector of candidate pitch angles, in degrees, in the interval
#'  \[-90, 90\]. Convention: `90` = pointed up, `0` = horizontally aligned, `-90` = pointed down.
#'  If `NULL` (default), a grid comprehending all simulated pitches is used. Ignored if `raw` is
#'  provided.
#' @param view_elevation_error Character choice specifying the error (± degrees) in view elevation
#'  measurement. One of `"0"`, `"2.5"`, `"5"`, or `"7.5"`.
#' @param label_error Character choice specifying the error (± pixels) in landmark
#'  labeling. One of `"0"`, `"2"`, `"5"`, or `"8"`.
#' @param label_nsim Integer scalar (default 100). Number of Monte Carlo draws passed internally to
#'  `pitch2d.w.error()` to simulate error in pitch2d calculation when `label_error != "0"`.
#' @param plot Logical scalar. `TRUE` plots original and expanded sets of angles side by side.
#'  
#' @returns
#' `lookup.error()` returns a named list with two elements:
#' \describe{
#'   \item{`pitch`}{a list of maximum errors in pitch per named source of error.}
#'   \item{`yaw`}{a list of maximum errors in yaw per named source of error.}
#'   }
#'
#' `include.error()` returns a numeric vector with the expanded candidate angles (pitches or yaws)
#' after incorporating the maximum error retrieved by `lookup.error()$pitch` or `lookup.error()$yaw`
#' with a 0.01 resolution. The maximum error is stored as vector in the attribute `error.included`.
#' 
#' @seealso [find.pitch()], [find.yaw()], [pitch2d.from.xy()]
#' @rdname include.error
#' @export
lookup.error <- function(raw = NULL,
                         pitch2d = NULL,
                         view_elevation = NULL,
                         yaws = NULL,
                         pitches = NULL,
                         view_elevation_error = c("0", "2.5", "5", "7.5"),
                         label_error = c("0", "2", "5", "8"),
                         label_nsim = 100){
  
  view_elevation_error <- as.numeric(match.arg(view_elevation_error))
  label_error <- as.numeric(match.arg(label_error))
  
  if(!is.null(raw)){
    attr <- attributes(raw)$meta
    
    pitch2d = attr$pitch2d
    view_elevation = attr$view_elevation
    yaws = unique(attr$yaws)
    pitches = unique(attr$pitches)
    
  } else {
    
    if(is.null(yaws)) view_elevation <- unique(sim_data$yaw.real)
    if(is.null(pitches)) view_elevation <- unique(sim_data$pitch.real)
    
  }
  
  pitch.error.list <- NULL
  yaw.error.list <- NULL
  
  if(label_error == 0 & view_elevation_error == 0){
    
    diff <- 1
    which1 <- sapply(sim_data$pitch2d, function(xi) any(abs(xi - pitch2d) <= diff))
    while(sum(which1) == 0){
      diff <- diff + 1
      which1 <- sapply(sim_data$pitch2d, function(xi) any(abs(xi - pitch2d) <= diff))
    }
    diff1 <- diff
    sim_data_trim <- sim_data[which1,]
    
    diff <- 1
    which2 <- sapply(sim_data_trim$roll.real, function(xi) any(abs(xi - view_elevation) <= diff))
    while(sum(which2) == 0){
      diff <- diff + 1
      which2 <- sapply(sim_data_trim$roll.real, function(xi) any(abs(xi - view_elevation) <= diff))
    }
    diff2 <- diff
    sim_data_trim <- sim_data_trim[which2,]
    
    diff <- 1
    which3 <- sapply(sim_data_trim$yaw.real, function(xi) any(abs(xi - yaws) <= diff))
    while(sum(which3) == 0){
      diff <- diff + 1
      which3 <- sapply(sim_data_trim$yaw.real, function(xi) any(abs(xi - yaws) <= diff))
    }
    diff3 <- diff
    sim_data_trim <- sim_data_trim[which3,]
    
    diff <- 1
    which4 <- sapply(sim_data_trim$pitch.real, function(xi) any(abs(xi - pitches) <= diff))
    while(sum(which4) == 0){
      diff <- diff + 1
      which4 <- sapply(sim_data_trim$pitch.real, function(xi) any(abs(xi - pitches) <= diff))
    }
    diff4 <- diff
    sim_data_trim <- sim_data_trim[which4,]
    
    which.col <- which(colnames(sim_data_trim) %in% "pitch.error")
    pitch.error.list <- append(pitch.error.list,
                               list(error = max(sim_data_trim[,which.col])))
    which.col <- which(colnames(sim_data_trim) %in% "yaw.error")
    yaw.error.list <- append(yaw.error.list,
                             list(error = max(sim_data_trim[,which.col])))
    
  }
  
  if(view_elevation_error != 0){
    
    view_elevation_w_error <- seq(view_elevation - view_elevation_error,
                                  view_elevation + view_elevation_error,
                                  1)
    
    diff <- 1
    which1 <- sapply(sim_data$pitch2d, function(xi) any(abs(xi - pitch2d) <= diff))
    while(sum(which1) == 0){
      diff <- diff + 1
      which1 <- sapply(sim_data$pitch2d, function(xi) any(abs(xi - pitch2d) <= diff))
    }
    diff1 <- diff
    sim_data_trim <- sim_data[which1,]
    
    diff <- 1
    which2 <- sapply(sim_data_trim$roll.real, function(xi) any(abs(xi - view_elevation_w_error) <= diff))
    while(sum(which2) == 0){
      diff <- diff + 1
      which2 <- sapply(sim_data_trim$roll.real, function(xi) any(abs(xi - view_elevation_w_error) <= diff))
    }
    diff2 <- diff
    sim_data_trim <- sim_data_trim[which2,]
    
    diff <- 1
    which3 <- sapply(sim_data_trim$yaw.real, function(xi) any(abs(xi - yaws) <= diff))
    while(sum(which3) == 0){
      diff <- diff + 1
      which3 <- sapply(sim_data_trim$yaw.real, function(xi) any(abs(xi - yaws) <= diff))
    }
    diff3 <- diff
    sim_data_trim <- sim_data_trim[which3,]
    
    diff <- 1
    which4 <- sapply(sim_data_trim$pitch.real, function(xi) any(abs(xi - pitches) <= diff))
    while(sum(which4) == 0){
      diff <- diff + 1
      which4 <- sapply(sim_data_trim$pitch.real, function(xi) any(abs(xi - pitches) <= diff))
    }
    diff4 <- diff
    sim_data_trim <- sim_data_trim[which4,]
    
    which.col <- which(colnames(sim_data_trim) %in% paste0("pitch.error.roll", view_elevation_error))
    pitch.error.list <- append(pitch.error.list,
                               list(error.roll = max(sim_data_trim[,which.col])))
    which.col <- which(colnames(sim_data_trim) %in% paste0("yaw.error.roll", view_elevation_error))
    yaw.error.list <- append(yaw.error.list,
                             list(error.roll = max(sim_data_trim[,which.col])))
    
  }
  
  if(label_error > 0){
    
    pitch2d_w_error <- pitch2d.w.error(pitch2d = pitch2d,
                                       label_error = as.character(label_error),
                                       label_nsim = label_nsim)
    
    diff <- 1
    which1 <- sapply(sim_data$pitch2d, function(xi) any(abs(xi - pitch2d_w_error) <= diff))
    while(sum(which1) == 0){
      diff <- diff + 1
      which1 <- sapply(sim_data$pitch2d, function(xi) any(abs(xi - pitch2d_w_error) <= diff))
    }
    diff1 <- diff
    sim_data_trim <- sim_data[which1,]
    
    diff <- 1
    which2 <- sapply(sim_data_trim$roll.real, function(xi) any(abs(xi - view_elevation) <= diff))
    while(sum(which2) == 0){
      diff <- diff + 1
      which2 <- sapply(sim_data_trim$roll.real, function(xi) any(abs(xi - view_elevation) <= diff))
    }
    diff2 <- diff
    sim_data_trim <- sim_data_trim[which2,]
    
    diff <- 1
    which3 <- sapply(sim_data_trim$yaw.real, function(xi) any(abs(xi - yaws) <= diff))
    while(sum(which3) == 0){
      diff <- diff + 1
      which3 <- sapply(sim_data_trim$yaw.real, function(xi) any(abs(xi - yaws) <= diff))
    }
    diff3 <- diff
    sim_data_trim <- sim_data_trim[which3,]
    
    diff <- 1
    which4 <- sapply(sim_data_trim$pitch.real, function(xi) any(abs(xi - pitches) <= diff))
    while(sum(which4) == 0){
      diff <- diff + 1
      which4 <- sapply(sim_data_trim$pitch.real, function(xi) any(abs(xi - pitches) <= diff))
    }
    diff4 <- diff
    sim_data_trim <- sim_data_trim[which4,]
    
    which.col <- which(colnames(sim_data_trim) %in% paste0("pitch.error.label", label_error))
    pitch.error.list <- append(pitch.error.list,
                               list(error.label = max(sim_data_trim[,which.col])))
    which.col <- which(colnames(sim_data_trim) %in% paste0("yaw.error.label", label_error))
    yaw.error.list <- append(yaw.error.list,
                             list(error.label = max(sim_data_trim[,which.col])))
    
  }
  
  return(list(pitch = pitch.error.list, yaw = yaw.error.list))
  
}
#' @rdname include.error
#' @export
include.error <- function(raw,
                          view_elevation_error = c("0", "2.5", "5", "7.5"),
                          label_error = c("0", "2", "5", "8"),
                          label_nsim = 100,
                          plot = FALSE){
  
  view_elevation_error <- match.arg(view_elevation_error)
  label_error <- match.arg(label_error)
  
  attr <- attributes(raw)$meta
  output <- unique(raw[,colnames(raw) %in% attr$find])
  
  max.err <- lookup.error(pitch2d = attr$pitch2d,
                          view_elevation = attr$view_elevation,
                          yaws = unique(attr$yaws),
                          pitches = unique(attr$pitches),
                          view_elevation_error = view_elevation_error,
                          label_error = label_error,
                          label_nsim = label_nsim)
  
  if(attr$find == "pitches"){
    
    max.error <- max(unlist(max.err$pitch))
    
    res <- 0.01
    scale <- round(1/res)
    
    # full grid of possible pitches with 0.01 res
    fullgrid <- seq(-90, 90, res)
    n <- length(fullgrid)
    
    # converting to integer
    output_int <- round(output * scale)
    error_int <- floor(max.error * scale)
    
    mask <- logical(n)
    
    output_unique <- unique(output_int)
    for (o in output_unique) {
      cidx <- o + 90*scale + 1 # e.g. -90 becomes 1
      low <- cidx - error_int
      high <- cidx + error_int
      low <- if (low < 1) 1 else low # <-90 not allowed
      high <- if (high > n) n else high # > 90 not allowed
      if (low <= high) mask[low:high] <- TRUE
    }
    
    # all possible pitches (considering res of 0.01) within max.error of any output
    output.w.error <- fullgrid[mask]
    # which ones were included as a priori pitches, considering round2keep?
    output.w.error <- output.w.error[round(output.w.error, attr$round2keep) %in% unique(round(attr$pitches, attr$round2keep))]
    
    if(plot){
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      graphics::par(mfrow = c(1, 2))
      # new_output <- output.w.error[!output.w.error %in% output]
      # visualize.angles(yaws = c(output, new.output), col.yaws = c(rep("salmon", length(output)),
      #                                                             rep("darkred", length(new_output))), main.yaws = "raw")
      visualize.angles(pitches = output, main.pitches = "raw")
      visualize.angles(pitches = output.w.error, main.pitches = "w/ error")
    }
    
  }
  
  if(attr$find == "yaws"){
    max.error <- max(unlist(max.err$yaw))
    
    res <- 0.01
    scale <- round(1/res)
    
    # full grid of possible pitches with 0.01 res
    fullgrid <- seq(-180 + res, 180, res)
    n <- length(fullgrid)
    
    # converting to integer
    output_int <- round(output * scale)
    error_int <- floor(max.error * scale)
    
    mask <- logical(n)
    
    output_unique <- unique(output_int)
    for (o in output_unique) {
      cidx <- (o + 180*scale) %% (360*scale) # e.g. -179.99 becomes 1
      low <- cidx - error_int
      high <- cidx + error_int
      # reduce to modulo 0...(n-1) for comparisons
      low <- ((low - 1) %% n) + 1
      high <- ((high - 1) %% n) + 1
      
      if (low <= high) {
        # contiguous range in index space
        mask[low:high] <- TRUE
      } else {
        # wrapped, mark lo...180 and -179.99...hi
        mask[low:n] <- TRUE
        mask[1:high]   <- TRUE
      }
      
    }
    
    # all possible yaws (considering res of 0.01) within max.error of any output
    output.w.error <- fullgrid[mask]
    # which ones were included as a priori pitches, considering round2keep?
    output.w.error <- output.w.error[round(output.w.error, attr$round2keep) %in% unique(round(attr$yaws, attr$round2keep))]
    
    if(plot){
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      graphics::par(mfrow = c(1, 2))
      # new_output <- output.w.error[!output.w.error %in% output]
      # visualize.angles(yaws = c(output, new.output), col.yaws = c(rep("salmon", length(output)),
      #                                                             rep("darkred", length(new_output))), main.yaws = "raw")
      visualize.angles(yaws = output, main.yaws = "raw")
      visualize.angles(yaws = output.w.error, main.yaws = "w/ error")
    }
    
  }
  
  attr(output.w.error, "error.included") <- max.error
  return(output.w.error)
  
}
#' @rdname include.error
#' @export
pitch2d.w.error <- function(pitch2d,
                            label_error = c("0", "2", "5", "8"),
                            label_nsim = 100){
  
  label_error <- as.numeric(match.arg(label_error))
    
  xy <- attributes(pitch2d)$xy
  x_tip <- xy$x_tip
  y_tip <- xy$y_tip
  x_base <- xy$x_base
  y_base <- xy$y_base
  
  t_x <- stats::runif(label_nsim, x_tip - label_error, x_tip + label_error)
  t_y <- stats::runif(label_nsim, y_tip - label_error, y_tip + label_error)
  b_x <- stats::runif(label_nsim, x_base - label_error, x_base + label_error)
  b_y <- stats::runif(label_nsim, y_base - label_error, y_base + label_error)
  
  pitch2d.all <- sort(unique(pitch2d.from.xy(t_x, t_y, b_x, b_y)))
  p2d.summ <- summarize.yaws(pitch2d.all)
  
  # do they cross -180/180?
  if(p2d.summ$wrap){
    pitch2d.all <- c(pitch2d.all,
                     180)
    # do they cross 90?
    if(p2d.summ$from < 90 | p2d.summ$to > 90){
      pitch2d.all <- c(pitch2d.all,
                       90)
    }
    # do they cross -90?
    if(p2d.summ$from < -90 | p2d.summ$to > -90){
      pitch2d.all <- c(pitch2d.all,
                       -90)
    }
    # do they cross 0?
    if(p2d.summ$from < 0 | p2d.summ$to > 0){
      pitch2d.all <- c(pitch2d.all,
                       0)
    }
  } else {
    # do they cross 90?
    if(p2d.summ$from < 90 & p2d.summ$to > 90){
      pitch2d.all <- c(pitch2d.all,
                       90)
    }
    # do they cross -90?
    if(p2d.summ$from < -90 & p2d.summ$to > -90){
      pitch2d.all <- c(pitch2d.all,
                       -90)
    }
    # do they cross 0?
    if(p2d.summ$from < 0 & p2d.summ$to > 0){
      pitch2d.all <- c(pitch2d.all,
                       0)
    }
  }
  
  return(sort(unique(pitch2d.all)))
  
}

