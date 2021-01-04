#' Set alpha level if missing
#'
#' @noRd

alpha_setter <- function(alpha) {
  if (missing(alpha))
  {
    alpha <- 0.05
    message("Confidence intervals set to 95%.")
  }
  return(alpha)
}

#' Set delta_list for various user supplied delta arguments
#'
#' @noRd

delta_setter <- function(delta, min_delta, max_delta) {
  if (missing(min_delta) & missing(max_delta) & !missing(delta))
  {
    delta_list <- delta
  }
  else if(!missing(min_delta) & !missing(max_delta) & !missing(delta)) {
    delta_list <- delta
    warning("delta, min_delta, and max_delta all provided. Ignoring min_delta and max_delta.")
  }
  else if(!missing(min_delta) & missing(max_delta) & missing(delta)) {
    min_delta <- -0.1
    max_delta <- 0.1
    delta_list <- seq(from = max_delta, to = min_delta, length.out = 11)
    message("delta (and min_delta/max_delta) missing. Setting delta to range from -0.1 to 0.1.")
  }
  else if(missing(min_delta) & !missing(max_delta) & missing(delta)) {
    min_delta <- -0.1
    max_delta <- 0.1
    delta_list <- seq(from = max_delta, to = min_delta, length.out = 11)
    message("delta (and min_delta/max_delta) missing. Setting delta to range from -0.1 to 0.1.")
  }
  else if (missing(min_delta) & missing(max_delta) & missing(delta))
  {
    min_delta <- -0.1
    max_delta <- 0.1
    delta_list <- seq(from = max_delta, to = min_delta, length.out = 11)
    message("delta (and min_delta/max_delta) missing. Setting delta to range from -0.1 to 0.1.")
  }
  else if (missing(delta) & !missing(min_delta) & !missing(max_delta))
  {
    if (min_delta == max_delta)
    {
      # spread out the delta range to +/- 0.2 from the single entered delta, ensuring
      # the range doesn't go beyond the possible bounds
      min_delta <- max(-0.99, min_delta - 0.2)
      max_delta <- min(0.99, min_delta + 0.4)
      min_delta <- max_delta - 0.4
    }
    delta_list <- seq(from = max_delta, to = min_delta, length.out = 11)
  }
  return(delta_list)
}

#' Check if all arguments are either numeric or integer values
#' @param list of call arguments
#' @param integer number of parent generations to go back on evaluation
#'
#' @noRd

arguments_type_checker <- function(x, parent.eval.n = 2) {
  for (i in seq_along(x)[-1]) {
    argument_value <- tryCatch(eval.parent(x[i][[1]], n = parent.eval.n),
                               error = function(e) e)
    if (!inherits(argument_value, "error")) {
      if (!inherits(argument_value, "pairlist")){
        if (!any(c(inherits(argument_value, "integer"),
                   inherits(argument_value, "numeric"),
                   # Used for negative integer values. There is probably a better way to do this
                   is.call(argument_value)))) {
          stop("One or more function arguments are of an invalid class. All arguments must be numeric.",
               call. = FALSE)
        }
      }
    }
  }
}

