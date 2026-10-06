#' Find 3D orientations compatible with observed 2D constraints
#'
#' Evaluate combinations of candidate 3D pitch, yaw, and view elevation and return those whose predicted
#' 2D projection is compatible with supplied observations. `find.pitch()` and `find.yaw()` are convenience
#' wrappers for finding compatible 3D pitch and yaw orientations, respectively.
#'
#' @param observed2d Optional one-row `data.frame` of class `araponga2d` returned by [project2d.from.xy()].
#'  Must be supplied with `label_error` and cannot be combined with `pitch2d` or `length2d`.
#' @param find Character vector specifying which 3D angle(s) to return. One or more of "pitch", "yaw", and
#'  "view_elevation", or "all" to return all three. Default is "all".
#' @param label_error Non-negative numeric scalar specifying the maximum labeling error in each landmark
#'  coordinate, in the same units as the coordinates in `observed2d` (e.g., pixels). Required when
#'  `observed2d` is supplied and not allowed with `pitch2d` or `length2d`.
#' @param full_length Optional positive numeric vector giving the possible unprojected (no foreshortening)
#'  length of the directed vector, in the same units as `length2d` or the coordinates in `observed2d`. Its
#'  minimum and maximum define a continuous allowed range.
#' @param pitch2d Optional numeric vector of projected 2D pitch angles, in degrees in the interval (-180,
#'  180]. The smallest continuous angular interval containing the supplied values is treated as the allowed
#'  `pitch2d` interval. Cannot be combined with `observed2d`.
#' @param length2d Optional non-negative numeric vector of projected 2D lengths. Its minimum and maximum
#'  define a continuous allowed range. Requires `full_length` and cannot be combined with `observed2d`.
#' @param candidate_pitches Numeric vector of candidate 3D pitch angles, in degrees in the interval
#'  (-180, 180]. Convention: `90` = pointed up, `0` = horizontally aligned, `-90` =
#'  pointed down. By default, a \[-90, 90\] grid is evaluated at `default_step` resolution. Values
#'  outside \[-90, 90\] may be supplied explicitly when an extended pitch representation is desired.
#' @param candidate_yaws Numeric vector of candidate yaw angles, in degrees in the
#'  interval (-180, 180]. Convention: `0` = pointed right, `90` = pointed straight away, `-90` = pointed
#'  straight toward, `180` = pointed left. By default, the entire (-180, 180] grid is evaluated at
#'  `default_step` resolution.
#' @param candidate_view_elevations Numeric vector of candidate camera elevation angles relative to the
#'  object, in degrees in the interval \[-90, 90\]. Convention: `-90` = seen from straight below,
#'  `0` = eye level, `90` = seen from straight above. By default, the entire \[-90, 90\] grid is
#'  evaluated at `default_step` resolution.
#' @param default_step Positive numeric scalar specifying the step size, in degrees, used to generate
#'  default candidate-angle vectors. Must evenly divide 180 to ensure full coverage of default candidate
#'  ranges. Default is `1` (integers). Has no effect on candidate vectors supplied explicitly by the
#'  user.
#' @param max_combinations Positive numeric scalar specifying the maximum total number of candidate
#'  pitch-yaw-view elevation combinations to evaluate, in order to prevent excessive memory use and
#'  computation. Default is `1e8`. Searches exceeding this value stop before candidate combinations are
#'  evaluated. Set to `Inf` to disable this limit.
#' @param paired Logical scalar used by `find.pitch()` and `find.yaw()`. If `TRUE`, returns a
#'  `data.frame` of yaws mapped to pitches; if `FALSE` (default), a vector of yaws or pitches.
#'
#' @returns
#' A `data.frame` containing the requested subset of:
#' \describe{
#'   \item{pitch}{numeric: 3D pitch angles, in degrees.}
#'   \item{yaw}{numeric: yaw angles, in degrees.}
#'   \item{view_elevation}{numeric: view elevation angles, in degrees.}
#' }
#'
#' Each row represents a candidate 3D orientation compatible with all supplied observational constraints.
#' If no combinations are compatible, the returned `data.frame` has zero rows.
#'
#' For `find.pitch(..., paired = FALSE)` and `find.yaw(..., paired = FALSE)`, a numeric vector of unique
#' compatible pitch or yaw angles is returned.
#' 
#' @details
#' For each unique combination of candidate pitch, yaw, and view elevation, `find.3d()` and wrappers use
#' [project2d.from.3d()] to predict the corresponding 2D projection. Candidate orientations are retained
#' when that projection is compatible with all supplied observational constraints.
#'
#' The function supports two main ways of specifying the observed 2D projection.
#'
#' The preferred route is to supply `observed2d` together with `label_error`. If `full_length` is not known,
#' the landmarks constrain projected pitch only. If `full_length` is also supplied, both projected
#' pitch and length are used jointly, preserving the relationship between them under landmark labeling
#' uncertainty.
#'
#' Alternatively, projected measurements may be supplied directly through `pitch2d` and/or `length2d`. This
#' is useful when landmark coordinates are unavailable or when the user already has suitable 2D constraints.
#' `length2d` requires `full_length`. When both `pitch2d` and `length2d` are supplied directly, they are
#' treated as independent constraints.
#'
#' Candidate angles supplied explicitly are evaluated exactly and are not rounded. For candidate
#' vectors left at their defaults, `default_step` determines the resolution of the search.
#'
#' The default pitch range is \[-90, 90\], which provides a conventional down-to-up representation.
#' Explicit candidate pitches may extend over (-180, 180]. Such extended pitch values should be used
#' intentionally because they necessarily have equivalent representations involving a different yaw
#' (e.g., a pitch of 135° at a yaw of 10° is equivalent to a pitch of 45° at the opposite yaw
#' of -170°).
#'
#' @examples
#' # construct a hypothetical observation from a known 3D orientation
#' predicted <- project2d.from.3d(
#'   pitch = 30,
#'   yaw = 20,
#'   view_elevation = 10,
#'   full_length = 100
#' )
#'
#' observed <- project2d.from.xy(
#'   x_tip = predicted$dx,
#'   y_tip = predicted$dy,
#'   x_base = 0,
#'   y_base = 0
#' )
#'
#' # landmark route: constrain projected direction only
#' find.3d(
#'   observed2d = observed,
#'   label_error = 1,
#'   candidate_pitches = 25:35,
#'   candidate_yaws = 15:25,
#'   candidate_view_elevations = 5:15
#' )
#'
#' # landmark route with full-length information: constrain projected direction and length jointly
#' find.3d(
#'   observed2d = observed,
#'   label_error = 1,
#'   full_length = c(98, 102),
#'   candidate_pitches = 25:35,
#'   candidate_yaws = 15:25,
#'   candidate_view_elevations = 5:15
#' )
#'
#' # direct constraints
#' find.3d(
#'   pitch2d = predicted$pitch2d + c(-1, 1),
#'   length2d = predicted$length2d + c(-2, 2),
#'   full_length = c(98, 102),
#'   candidate_pitches = 25:35,
#'   candidate_yaws = 15:25,
#'   candidate_view_elevations = 5:15
#' )
#' 
#' # convenience wrappers
#' find.pitch(
#'   observed2d = observed,
#'   label_error = 1,
#'   candidate_yaws = 15:25,
#'   candidate_view_elevations = 5:15
#' )
#'
#' find.yaw(
#'   observed2d = observed,
#'   label_error = 1,
#'   candidate_pitches = 25:35,
#'   candidate_view_elevations = 5:15
#' )
#'
#' @seealso [project2d.from.3d()], [project2d.from.xy()], [pitch2d.w.error()]
#' @rdname find.3d
#' @export
find.3d <- function(observed2d = NULL,
                    find = "all",
                    label_error = NULL,
                    full_length = NULL,
                    pitch2d = NULL,
                    length2d = NULL,
                    candidate_pitches = seq(-90, 90, default_step),
                    candidate_yaws = seq(-180 + default_step, 180, default_step),
                    candidate_view_elevations = seq(-90, 90, default_step),
                    default_step = 1,
                    max_combinations = 1e8){
  
  ## --- find ---
  find <- unique(unname(find))
  allowed_find <- c("all", "pitch", "yaw", "view_elevation")
  if(!is.character(find) || length(find) == 0 || anyNA(find)){
    stop("`find` must be a character vector.", call. = FALSE)
  }
  bad_find <- setdiff(find, allowed_find)
  if(length(bad_find) > 0){
    stop(sprintf(
      "`find` contains invalid value(s): %s. Allowed values are: %s.",
      paste(shQuote(bad_find), collapse = ", "),
      paste(shQuote(allowed_find), collapse = ", ")
    ), call. = FALSE)
  }
  if("all" %in% find && length(find) > 1){
    stop("`\"all\"` cannot be combined with other values in `find`.",
         call. = FALSE)
  }
  if(identical(find, "all")) find <- c("pitch", "yaw", "view_elevation")
  
  ## --- default_step ---
  if(!is.numeric(default_step) ||
     length(default_step) != 1 ||
     !is.finite(default_step) ||
     default_step <= 0 ||
     default_step > 180){
    stop("`default_step` must be a finite numeric scalar > 0 and <= 180.",
         call. = FALSE)
  }
  
  n_steps <- 180 / default_step
  
  if(abs(n_steps - round(n_steps)) > 1e-8){
    stop(
      "`default_step` must evenly divide 180 degrees.",
      call. = FALSE
    )
  }
  
  ## --- candidate angle sets ---
  if(!is.numeric(candidate_view_elevations) ||
     length(candidate_view_elevations) == 0 ||
     any(!is.finite(candidate_view_elevations))){
    stop("`candidate_view_elevations` must be a non-empty finite numeric vector.",
         call. = FALSE)
  }
  if(any(candidate_view_elevations < -90 | candidate_view_elevations > 90)){
    stop("`candidate_view_elevations` must satisfy -90 <= value <= 90 degrees.", call. = FALSE)
  }
  
  if(!is.numeric(candidate_pitches) ||
     length(candidate_pitches) == 0 ||
     any(!is.finite(candidate_pitches))){
    stop("`candidate_pitches` must be a non-empty finite numeric vector.",
         call. = FALSE)
  }
  if(any(candidate_pitches <= -180 | candidate_pitches > 180)){
    stop("`candidate_pitches` must satisfy -180 < value <= 180 degrees.", call. = FALSE)
  }
  
  if(!is.numeric(candidate_yaws) ||
     length(candidate_yaws) == 0 ||
     any(!is.finite(candidate_yaws))){
    stop("`candidate_yaws` must be a non-empty finite numeric vector.",
         call. = FALSE)
  }
  if(any(candidate_yaws <= -180 | candidate_yaws > 180)){
    stop("`candidate_yaws` must satisfy -180 < value <= 180 degrees.", call. = FALSE)
  }
  
  ## --- max_combinations ---
  if(!is.numeric(max_combinations) ||
     length(max_combinations) != 1 ||
     is.na(max_combinations) ||
     max_combinations <= 0){
    stop(
      "`max_combinations` must be a positive numeric scalar.",
      call. = FALSE
    )
  }
  
  observed <- .prepare.observed(
    observed2d = observed2d,
    label_error = label_error,
    pitch2d = pitch2d,
    length2d = length2d,
    full_length = full_length
  )
  
  ## --- candidate combinations ---
  candidate_pitches <- unique(candidate_pitches)
  candidate_yaws <- unique(candidate_yaws)
  candidate_view_elevations <- unique(candidate_view_elevations)
  
  ny <- length(candidate_yaws)
  np <- length(candidate_pitches)
  ne <- length(candidate_view_elevations)
  
  n_combinations <-
    as.double(ny) *
    as.double(np) *
    as.double(ne)
  
  if(n_combinations > max_combinations){
    stop(
      sprintf(
        paste0(
          "The requested candidate angles produce %.0f combinations, ",
          "which exceeds `max_combinations = %.0f`. ",
          "Use narrower candidate ranges, a larger `default_step`, ",
          "or increase `max_combinations` intentionally."
        ),
        n_combinations,
        max_combinations
      ),
      call. = FALSE
    )
  }
  
  ## --- chunk by angle with longest vector ---
  
  candidate_angles <- list(
    yaw = candidate_yaws,
    pitch = candidate_pitches,
    view_elevation = candidate_view_elevations
  )
  
  n_candidates <- lengths(candidate_angles)
  chunk_angle <- names(which.max(n_candidates))
  chunk_values <- candidate_angles[[chunk_angle]]
  
  collected <- vector("list", length(chunk_values))
  
  for(i in seq_along(chunk_values)){
    
    chunk_candidates <- candidate_angles
    chunk_candidates[[chunk_angle]] <- chunk_values[i]
    
    grid <- do.call(
      expand.grid,
      c(
        chunk_candidates,
        list(KEEP.OUT.ATTRS = FALSE)
      )
    )
    
    ## --- project candidate orientations ---
    
    predicted <- project2d.from.3d(pitch = grid$pitch,
                                   yaw = grid$yaw,
                                   view_elevation = grid$view_elevation)
    
    keep <- rep(TRUE, nrow(grid))
    
    ## --- pitch2d criterion ---
    
    if(!is.null(observed$pitch2d)){
      
      tol <- sqrt(.Machine$double.eps)
      
      if(observed$pitch2d$wrap){
        
        keep <- keep &
          is.finite(predicted$pitch2d) &
          (
            (predicted$pitch2d >= observed$pitch2d$from - tol &
               predicted$pitch2d <= 180) |
              (predicted$pitch2d > -180 &
                 predicted$pitch2d <= observed$pitch2d$to + tol)
          )
        
      } else {
        
        keep <- keep &
          is.finite(predicted$pitch2d) &
          predicted$pitch2d >= observed$pitch2d$from - tol &
          predicted$pitch2d <= observed$pitch2d$to + tol
        
      }
      
    }
    
    ## --- length2d criterion --- ##
    
    if(!is.null(observed$length2d)){
      
      predicted_length2d.min <- predicted$projection_factor * min(observed$full_length)
      predicted_length2d.max <- predicted$projection_factor * max(observed$full_length)
      
      tol <- sqrt(.Machine$double.eps) * pmax(
        1,
        abs(predicted_length2d.min),
        abs(predicted_length2d.max),
        max(abs(observed$length2d))
      )
      
      keep <- keep &
        is.finite(predicted$projection_factor) &
        predicted_length2d.min <= max(observed$length2d) + tol &
        predicted_length2d.max >= min(observed$length2d) - tol
      
    }
    
    ## --- dx/dy criterion ---
    
    if(!is.null(observed$dx)){
      
      keep <- keep &
        .pred.segment.intersects.obs.rectangle(
          pred_dx_factor = predicted$dx_factor,
          pred_dy_factor = predicted$dy_factor,
          obs_full_length_range = observed$full_length,
          obs_dx_range = observed$dx,
          obs_dy_range = observed$dy
        )
      
    }
    
    collected[[i]] <- unique(grid[keep, find, drop = FALSE])
    
  }
  
  collected <- unique(do.call(rbind, collected))
  rownames(collected) <- NULL
  
  return(as.data.frame(collected))
  
}
#' @rdname find.3d
#' @export
find.yaw <- function(observed2d = NULL,
                     label_error = NULL,
                     full_length = NULL,
                     pitch2d = NULL,
                     length2d = NULL,
                     candidate_pitches = seq(-90, 90, default_step),
                     candidate_yaws = seq(-180 + default_step, 180, default_step),
                     candidate_view_elevations = seq(-90, 90, default_step),
                     paired = FALSE,
                     default_step = 1,
                     max_combinations = 1e8){
  
  if(!(is.logical(paired) && length(paired) == 1 && !is.na(paired))){
    stop("`paired` must be a logical scalar.", call. = FALSE)
  }
  
  if(paired){
    find <- c("pitch", "yaw")
  } else {
    find <- "yaw"
  }
  
  df <- find.3d(
    observed2d = observed2d,
    find = find,
    candidate_view_elevations = candidate_view_elevations,
    candidate_pitches = candidate_pitches,
    candidate_yaws = candidate_yaws,
    full_length = full_length,
    label_error = label_error,
    pitch2d = pitch2d,
    length2d = length2d,
    default_step = default_step,
    max_combinations = max_combinations
  )
  
  if(paired){
    df <- df[order(df$pitch, df$yaw), ]
    return(df)
  } else {
    return(sort(unique(df$yaw)))
  }
}
#' @rdname find.3d
#' @export
find.pitch <- function(observed2d = NULL,
                       label_error = NULL,
                       full_length = NULL,
                       pitch2d = NULL,
                       length2d = NULL,
                       candidate_pitches = seq(-90, 90, default_step),
                       candidate_yaws = seq(-180 + default_step, 180, default_step),
                       candidate_view_elevations = seq(-90, 90, default_step),
                       paired = FALSE,
                       default_step = 1,
                       max_combinations = 1e8){
  
  if(!(is.logical(paired) && length(paired) == 1 && !is.na(paired))){
    stop("`paired` must be a logical scalar.", call. = FALSE)
  }
  
  if(paired){
    find <- c("yaw", "pitch")
  } else {
    find <- "pitch"
  }
  
  df <- find.3d(
    observed2d = observed2d,
    find = find,
    candidate_view_elevations = candidate_view_elevations,
    candidate_pitches = candidate_pitches,
    candidate_yaws = candidate_yaws,
    full_length = full_length,
    label_error = label_error,
    pitch2d = pitch2d,
    length2d = length2d,
    default_step = default_step,
    max_combinations = max_combinations
  )
  
  if(paired){
    df <- df[order(df$yaw, df$pitch), ]
    return(df)
  } else {
    return(sort(unique(df$pitch)))
  }
}

.prepare.observed <-  function(
    observed2d = NULL,
    label_error = NULL,
    pitch2d = NULL,
    length2d = NULL,
    full_length = NULL
){
  
  ## --- observed2d ---
  
  if(!is.null(observed2d)){
    
    if(!inherits(observed2d, "araponga2d")){
      stop(
        "`observed2d` must be an `araponga2d` object returned by `project2d.from.xy()`.",
        call. = FALSE
      )
    }
    
    if(nrow(observed2d) != 1){
      stop(
        "`observed2d` must contain exactly one observation.",
        call. = FALSE
      )
    }
    
    required <- c("x_tip", "y_tip", "x_base", "y_base")
    
    if(!all(required %in% names(observed2d))){
      stop(
        "`observed2d` must contain `x_tip`, `y_tip`, `x_base`, and `y_base`.",
        call. = FALSE
      )
    }
    
    if(any(!vapply(
      observed2d[required],
      function(x) is.numeric(x) && length(x) == 1 && is.finite(x),
      logical(1)
    ))){
      stop(
        "Landmark coordinates in `observed2d` must be finite numeric values.",
        call. = FALSE
      )
    }
    
  }
  
  
  ## --- label_error ---
  
  if(!is.null(label_error)){
    
    if(!is.numeric(label_error) ||
       length(label_error) != 1 ||
       !is.finite(label_error) ||
       label_error < 0){
      stop(
        "`label_error` must be a non-negative finite numeric scalar.",
        call. = FALSE
      )
    }
    
  }
  
  
  ## --- pitch2d ---
  
  if(!is.null(pitch2d)){
    
    if(!is.numeric(pitch2d) ||
       length(pitch2d) == 0 ||
       any(!is.finite(pitch2d))){
      stop(
        "`pitch2d` must be a non-empty finite numeric vector.",
        call. = FALSE
      )
    }
    
    if(any(pitch2d <= -180 | pitch2d > 180)){
      stop(
        "`pitch2d` must satisfy -180 < value <= 180 degrees.",
        call. = FALSE
      )
    }
    
  }
  
  
  ## --- length2d ---
  
  if(!is.null(length2d)){
    
    if(!is.numeric(length2d) ||
       length(length2d) == 0 ||
       any(!is.finite(length2d)) ||
       any(length2d < 0)){
      stop(
        "`length2d` must contain non-negative finite numeric values.",
        call. = FALSE
      )
    }
    
  }
  
  
  ## --- full_length ---
  
  if(!is.null(full_length)){
    
    if(!is.numeric(full_length) ||
       length(full_length) == 0 ||
       any(!is.finite(full_length)) ||
       any(full_length <= 0)){
      stop(
        "`full_length` must contain positive finite numeric values.",
        call. = FALSE
      )
    }
    
    full_length <- range(full_length)
  }
  
  observed_mode <- !is.null(observed2d)
  direct_mode <- !is.null(pitch2d) || !is.null(length2d)
  
  ## Exactly one input route
  if(observed_mode && direct_mode){
    stop(
      "Supply either `observed2d` or direct `pitch2d`/`length2d` constraints, not both.",
      call. = FALSE
    )
  }
  
  if(!observed_mode && !direct_mode){
    stop(
      "Supply either `observed2d` or at least one of `pitch2d` and `length2d`.",
      call. = FALSE
    )
  }
  
  out <- list(
    pitch2d = NULL,
    length2d = NULL,
    full_length = full_length,
    dx = NULL,
    dy = NULL
  )
  
  ## --- observed landmark route ---
  if(observed_mode){
    
    if(is.null(label_error)){
      stop(
        "`label_error` is required when `observed2d` is supplied.",
        call. = FALSE
      )
    }
    
    if(is.null(full_length)){
      
      # Without projected-length information, landmarks constrain
      # only projected direction.
      out$pitch2d <- pitch2d.w.error(
        observed2d = observed2d,
        label_error = label_error
      )
      
    } else {
      
      # With length information, preserve the joint pitch-length
      # uncertainty by working directly in component space.
      dx0 <- observed2d$x_tip - observed2d$x_base
      dy0 <- observed2d$y_tip - observed2d$y_base
      
      out$dx <- dx0 + c(-2, 2) * label_error
      out$dy <- dy0 + c(-2, 2) * label_error
    }
    
    return(out)
  }
  
  ## --- direct constraint route ---
  if(!is.null(label_error)){
    stop(
      "`label_error` can only be used with `observed2d`.",
      call. = FALSE
    )
  }
  
  if(!is.null(length2d) && is.null(full_length)){
    stop(
      "`full_length` is required when `length2d` is supplied.",
      call. = FALSE
    )
  }
  
  if(!is.null(full_length) && is.null(length2d)){
    stop(
      "`full_length` can only constrain direct inputs when `length2d` is supplied.",
      call. = FALSE
    )
  }
  
  if(!is.null(pitch2d)){
    out$pitch2d <- summarize.yaws(
      pitch2d,
      tie_action = "error"
    )
  }
  
  if(!is.null(length2d)){
    out$length2d <- range(length2d)
  }
  
  out
}

.pred.segment.intersects.obs.rectangle <- function(pred_dx_factor,
                                                   pred_dy_factor,
                                                   obs_full_length_range,
                                                   obs_dx_range,
                                                   obs_dy_range,
                                                   plot = FALSE){
  
  if(length(pred_dx_factor) == 1 && plot){
    pred_dx_range <- pred_dx_factor * obs_full_length_range
    pred_dy_range <- pred_dy_factor * obs_full_length_range
    
    xlim <- range(obs_dx_range, pred_dx_range)
    ylim <- range(obs_dy_range, pred_dy_range)
    
    xpad <- 0.1 * diff(xlim)
    ypad <- 0.1 * diff(ylim)
    
    if(xpad == 0) xpad <- 1
    if(ypad == 0) ypad <- 1
    
    graphics::plot(
      NULL,
      xlim = xlim + c(-xpad, xpad),
      ylim = ylim + c(-ypad, ypad),
      xlab = "dx",
      ylab = "dy",
      asp = 1
    )
    
    graphics::rect(
      xleft = obs_dx_range[1],
      xright = obs_dx_range[2],
      ybottom = obs_dy_range[1],
      ytop = obs_dy_range[2],
      border = NA,
      col = "gray80"
    )
    
    graphics::lines(
      x = pred_dx_range,
      y = pred_dy_range
    )
  }
  
  keep <- rep(TRUE, length(pred_dx_factor))
  comp_full_length.min <- rep(min(obs_full_length_range), length(pred_dx_factor))
  comp_full_length.max <- rep(max(obs_full_length_range), length(pred_dx_factor))
  
  ## dx
  
  pred_dx.nonzero <- pred_dx_factor != 0
  
  if(any(pred_dx.nonzero)){
    
    # values of full_length compatible with obs_dx are
    # min_obs_dx  ≤  pred_dx_factor*full_length  ≤  max_obs_dx
    # min_obs_dx/pred_dx_factor  ≤  full_length  ≤  max_obs_dx/pred_dx_factor
    # (for positive dx_factor)
    
    full_length.dx.1 <- min(obs_dx_range)/pred_dx_factor[pred_dx.nonzero]
    full_length.dx.2 <- max(obs_dx_range)/pred_dx_factor[pred_dx.nonzero]
    
    # endpoint order may reverse when dx_factor is negative
    full_length.dx.min <- pmin(full_length.dx.1,
                               full_length.dx.2)
    full_length.dx.max <- pmax(full_length.dx.1,
                               full_length.dx.2)
    
    # intersect with obs_full_length constraint
    comp_full_length.min[pred_dx.nonzero] <- pmax(
      comp_full_length.min[pred_dx.nonzero],
      full_length.dx.min
    )
    
    comp_full_length.max[pred_dx.nonzero] <- pmin(
      comp_full_length.max[pred_dx.nonzero],
      full_length.dx.max
    )
    
  }
  
  # if 0 is not a possible observed_dx, retain only candidates whose predicted_dx_factor != 0
  if(!(min(obs_dx_range) <= 0 && max(obs_dx_range) >= 0)){
    keep <- keep & pred_dx.nonzero
  }
  
  
  ## dy
  
  pred_dy.nonzero <- pred_dy_factor != 0
  
  if(any(pred_dy.nonzero)){
    
    full_length.dy.1 <- min(obs_dy_range)/pred_dy_factor[pred_dy.nonzero]
    full_length.dy.2 <- max(obs_dy_range)/pred_dy_factor[pred_dy.nonzero]
    
    full_length.dy.min <- pmin(full_length.dy.1,
                               full_length.dy.2)
    full_length.dy.max <- pmax(full_length.dy.1,
                               full_length.dy.2)
    
    comp_full_length.min[pred_dy.nonzero] <- pmax(
      comp_full_length.min[pred_dy.nonzero],
      full_length.dy.min
    )
    
    comp_full_length.max[pred_dy.nonzero] <- pmin(
      comp_full_length.max[pred_dy.nonzero],
      full_length.dy.max
    )
    
  }
  
  if(!(min(obs_dy_range) <= 0 && max(obs_dy_range) >= 0)){
    keep <- keep & pred_dy.nonzero
  }
  
  
  ## common full_length
  
  tol <- sqrt(.Machine$double.eps) * pmax(
    1,
    abs(comp_full_length.min),
    abs(comp_full_length.max)
  )
  
  keep <- keep &
    comp_full_length.min <= comp_full_length.max + tol
  
  return(keep)
  
}
