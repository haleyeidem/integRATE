#' Combine desirability scores
#'
#' This function combines any number of individual desirability scores into an
#' overall desirability score.
#'
#' @details Using a weighted geometric mean, this function combines individual
#' desirability scores to calculate overall desirability scores.
#' @param ... Any number of vectors of desirability scores (as calculated using
#' desire()).
#' @param weights Optional, relative weights reflecting the quality, importance,
#' or applicability of the variable for which desirability scores have been
#' calculated.
#' @return Returns a numeric vector of overall desirability scores.
#' @export

desire_overall <- function(..., weights = NULL){

  # get number of variables
  n <- length(list(...))

  # check whether any variables passed
  if(n == 0) stop("Some desirabilities must be included\n")

  # merge into matrix
  desire_all <- cbind(...)

  if(min(desire_all, na.rm=TRUE) < 0 | max(desire_all, na.rm=TRUE) > 1) {
    stop("Desirabilities must be between 0 and 1\n")
  }

  # check lengths match
  if(!is.null(weights)) {
    if(ncol(desire_all) != length(weights) ) {
      stop("Number of weights does not match number of desirabilities\n")
    }
  }

  # equal weights if none provided
  if(is.null(weights) == TRUE) {
    w <- rep(1,n)/n
  } else {
    w <- weights
  }

  # weighted geometric mean by row (omit missing)
  y <- apply(desire_all, 1, function(x) exp(sum(w[!is.na(x)] * log(x[!is.na(x)])/
      sum(w[!is.na(x)])) )
  )

  return(y)
}
