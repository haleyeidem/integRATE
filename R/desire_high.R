#' High values are best
#'
#' This function maps a numeric variable to a [0, 1] scale where high values are
#' most desirable.
#'
#' @details Values in the top n percent will have the highest possible
#' desirability and values in the botton n percent will have the lowest possible
#' desirability. Values between will be transformed to intermediate values
#' according to the function.
#'
#' @param x Vector of numeric values.
#' @param cut1,cut2 Percentiles where the desirability function changes.
#' @param min,max Minimum (default = 0) and maximum (default = 1) desirability
#' scores.
#' @param scale Controls shape of the desirability function. Larger values
#' correspond to more steep and strict curves whereas smaller values correspond
#' to more gradual and inclusive curves.
#' @return Returns a numeric vector of desirability scores.
#' @export

desire_high <- function(x, cut1, cut2, min = 0, max = 1, scale = 1){

  if(cut1 >= cut2) stop("cut1 must be less than cut2\n")
  if(min < 0 | min > 1) stop("min must be between zero and one\n")
  if(max < 0 | max > 1) stop("max must be between zero and one\n")
  if(scale <= 0) stop("scale must be greater than zero\n")

  y <- rep(NA,length(x))
  y <- ((x - cut1)/(cut2 - cut1))^scale
  y[x < cut1] <- 0
  y[x > cut2] <- 1

  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
