desire_low <- function(x, cut1, cut2,  min = 0, max = 1, scale = 1){

  if(cut1 >= cut2) stop("cut1 must be less than cut2\n")
  if(min < 0 | min > 1) stop("min must be between zero and one\n")
  if(max < 0 | max > 1) stop("max must be between zero and one\n")
  if(scale <= 0) stop("scale must be greater than zero\n")

  y <- rep(NA,length(x))
  y <- ((x - cut2)/(cut1 - cut2))^scale
  y[x < cut1] <- 1
  y[x > cut2] <- 0

  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
