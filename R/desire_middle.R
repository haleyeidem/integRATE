desire_middle <- function(x, cut1, cut2, cut3, cut4, min = 0, max = 1,
  scale = 1){

  if(cut1 >= cut2) stop("cut1 must be less than cut2\n")
  if(cut2 >= cut3) stop("cut2 must be less than cut3\n")
  if(cut3 >= cut4) stop("cut3 must be less than cut4\n")
  if(min < 0 | min > 1) stop("min must be between zero and one\n")
  if(max < 0 | max > 1) stop("max must be between zero and one\n")
  if(scale <= 0) stop("scale must be greater than zero\n")

  y <- rep(NA, length(x))
  for (i in 1:length(x)){
    if (is.na(x[i])) next
    if (x[i] <= cut1 | x[i] >= cut4)  y[i] <- 0
    if (x[i] >= cut2 & x[i] <= cut3) y[i] <- 1
    if (x[i] > cut1 & x[i] < cut2) y[i] <- ((x[i] - cut1)/(cut2 - cut1))^scale
    if (x[i] > cut3 & x[i] < cut4) y[i] <- ((x[i] - cut4)/(cut3 - cut4))^scale
  }

  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
