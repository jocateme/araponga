#' Summarize yaw angles into the smallest continuous interval
#'
#' @description
#' Given a set of yaw angles (or any set of angles that wrap around ± 180), `summarize.yaws()` returns
#' the endpoints and width of the smallest continuous angular interval that contains all supplied angles.
#' 
#' @param yaws Numeric vector of yaw angles, in degrees, in the interval (-180, 180].
#' @param plot Logical scalar. `TRUE` draws a diagnostic plot with individual candidate angles and
#'  the returned smallest continuous interval that contains them. Works only when `length(yaws) > 1`.
#' @param tie_action Choice between `"all"` and "`error`" on how to proceed when multiple smallest
#'  continuous intervals exist. `"all"` (default) issues a warning and returns all possible intervals
#'  (see Value), while "`error`" issues an error.
#'
#' @return A named `list` with components:
#' \describe{
#'   \item{from}{numeric scalar: interval start (clockwise-most endpoint), in degrees.}
#'   \item{to}{numeric scalar: interval end (counterclockwise-most endpoint), in degrees.}
#'   \item{width}{numeric scalar: angular width of the shortest enclosing arc, in degrees.}
#'   \item{wrap}{logical: `TRUE` if the interval crosses the ±180 boundary.}
#'   \item{alternatives}{list containing the output (from, to, width, wrap) for all alternative intervals,
#'   if any}
#'   }
#'   
#' @examples
#' # simple non-wrapping interval
#' summarize.yaws(-60:60, plot = TRUE)
#'
#' # a wrapping interval (points around ±180)
#' summarize.yaws(c(-175:-170, 171:179), plot = TRUE)
#' 
#' # output from find.yaw()
#' yaws <- find.yaw(pitch2d = 14:16, candidate_view_elevations = -45, candidate_pitches = 10:20)
#' summarize.yaws(yaws, plot = TRUE)
#'
#' # singleton (no plotting)
#' summarize.yaws(30)
#' 
#' # more than one interval possible (returns warning)
#' summarize.yaws(c(-90, 90))
#' 
#' @export
summarize.yaws <- function(yaws,
                           plot = FALSE,
                           tie_action = c("all",
                                         "error")){
  
  ## input checks
  if (missing(yaws) || length(yaws) == 0) {
    stop("`yaws` must be a non-empty numeric vector.", call. = FALSE)
  }
  if (!is.numeric(yaws)){
    stop("`yaws` must be numeric.", call. = FALSE)
  }
  if (any(is.na(yaws))){
    warning(sum(is.na(yaws)), "NA `yaws` excluded.", call. = FALSE)
    yaws <- yaws[!is.na(yaws)]
    if (length(yaws) == 0) {
      stop("All `yaws` are NA; nothing to summarize.", call. = FALSE)
    }
  }
  if (any(yaws <= -180 | yaws > 180)) {
    stop("`yaws` must satisfy -180 < yaw <= 180 degrees.", call. = FALSE)
  }
  tie_action <- match.arg(tie_action)
  
  yaws <- unique(yaws)
  
  if(length(yaws)==1){
    return(list(from = yaws,
                to = yaws,
                width = 0,
                wrap = FALSE))
  }
  
  # normalize to 0->360
  yaws360 <- ((yaws + 360) %% 360)
  yaws360 <- sort(unique(yaws360))
  n <- length(yaws360)
  # consecutive gaps
  gaps <- diff(yaws360)
  # include wrap gap
  gaps <- c(gaps, yaws360[1] + 360 - yaws360[n])
  # index of largest gap
  idx <- which(gaps == max(gaps))
  
  if(length(idx) > 1){
    if(tie_action == "stop"){
      stop('More than one smallest continuous interval found. Set `tie_action = "alternatives"` to get',
           'all possible intervals.', call. = FALSE)
    }
    if(tie_action == "alternatives"){
      warning('More than one smallest continuous interval found. Returned an arbitrary possibility; see',
      ' $alternatives for all possibilities.', call. = FALSE)
    }
  }
  
  alternatives <- list()
  
  for(k in idx){
    # values before and after gap
    start0 <- yaws360[(k %% n) + 1]
    end0 <- yaws360[k]
    # make end >= start (on 0...360 line)
    if(end0 < start0) end0 <- end0 + 360
    width <- end0 - start0
    # map back to -180...180 representation for endpoints
    conv <- function(v){ifelse(v > 180,
                               v - 360,
                               v)}
    from <- conv(start0)
    to   <- conv(end0 %% 360)  # keep in 0..360 then map to -180..180
    # wrap TRUE when to (in -180..180) is numerically < from
    wrap <- (to < from)
    
    alternatives <- append(alternatives,
                           list(list(from = from, to = to, width = width, wrap = wrap)))
  }
  
  
  chosen <- alternatives[[1]]
  alternatives <- alternatives[-1]
  
  if(plot){
    
    plot.angles(yaws, "yaw",
                labels = FALSE)
    if(wrap){
      yaws_plot <- deg2rad(c(seq(chosen$from, 180, 0.1),
                             seq(-180, chosen$to, 0.1)))
      
    } else {
      yaws_plot <- deg2rad(seq(chosen$from, chosen$to, 0.1))
    }
    
    
    graphics::lines(x = 0 + 1.05 * cos(yaws_plot),
                    y = 0 + 1.05 * sin(yaws_plot),
                    type = "l",
                    col = "darkred",
                    lwd = 1.5)
    
  }
  
  return(list(from = chosen$from,
              to = chosen$to,
              width = chosen$width,
              wrap = chosen$wrap,
              alternatives = alternatives))
  
}
