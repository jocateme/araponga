#' Summarize yaw angles into the smallest continuous interval
#'
#' @description
#' Given a set of yaw angles, `summarize.yaws()` returns the endpoints and width of the smallest
#' continuous angular interval that contains all supplied yaws.
#' 
#' @param yaws Numeric vector of yaw angles, in degrees, in the interval (-180, 180].
#'
#' @returns A named `list` with components:
#' \describe{
#'   \item{from}{numeric scalar: interval start (more counterclockwise endpoint), in degrees.}
#'   \item{to}{numeric scalar: inverval end (more clockwise endpoint), in degrees.}
#'   \item{width}{numeric scalar: angular width of the shortest enclosing arc, in degrees.}
#'   \item{wrap}{logical: `TRUE` if the interval crosses the -180/180 boundary.}
#'   }
#'   
#' @examples
#' # simple non-wrapping interval
#' summarize.yaws(-60:60)
#'
#' # a wrapping interval (points around ±180)
#' summarize.yaws(c(-175:-170, 171:179))
#' 
#' # output from find.yaw()
#' yaws <- find.yaw(pitch2d = 15, view_elevation = -45, pitches = seq(10, 20, 0.1))$yaws
#' summarize.yaws(yaws)
#'
#' # singleton
#' summarize.yaws(30)
#' 
#' @export
summarize.yaws <- function(yaws){
  
  ## input checks
  if (missing(yaws) || length(yaws) == 0) {
    stop("`yaws` must be a non-empty numeric vector.", call. = FALSE)
  }
  if (!is.numeric(yaws)){
    stop("`yaws` must be numeric.", call. = FALSE)
  }
  if (any(is.na(yaws))){
    warning(sum(is.na(yaws)), " NA `yaws` excluded.", call. = FALSE)
    yaws <- yaws[!is.na(yaws)]
  }
  if (any(yaws <= -180 | yaws > 180)) {
    stop("`yaws` must satisfy -180 < yaw <= 180 degrees.", call. = FALSE)
  }
  
  yaws <- unique(yaws)
  
  if(length(yaws)==1){
    return(list(from = yaws,
                to = yaws,
                width = 0,
                wrap = FALSE))
  }
  
  # normalize to 0->360
  yaws360 <- ((yaws %% 360) + 360) %% 360
  yaws360 <- sort(unique(yaws360))
  n <- length(yaws360)
  # consecutive gaps
  gaps <- diff(yaws360)
  # include wrap gap
  gaps <- c(gaps, yaws360[1] + 360 - yaws360[n])
  # index of largest gap
  k <- which.max(gaps)

  # values before and after gap
  bound1 <- yaws360[(k %% n) + 1]
  bound2 <- yaws360[k]
  # start is the more CCW value
  start0 <- min(bound1, bound2)
  end0 <- max(bound1, bound2)
  width <- end0 - start0
  # map back to -180->180 for endpoints
  conv <- function(v){ifelse(v > 180,
                             v - 360,
                             v)}
  from <- conv(start0)
  to <- conv(end0)
  wrap <- (to < from)
  list(from = from, to = to, width = width, wrap = wrap)
  
}