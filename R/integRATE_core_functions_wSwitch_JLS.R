##testing

## make toy data
#df <- data.frame(rnorm(1000, 0, 0.75))
#df[,2] <- data.frame(c(runif(150, 0, 0.001), runif(400, 0.001, 0.2), runif(250, 0.2, 0.5), runif(200, 0.5, 1)))
#colnames(df) <- c("Log Fold Change", "Adjusted P-Value")

#######################################################################
## desire #############################################################
#######################################################################

# tiny change

# cut types
cut.type <-
  c("numerical", "num", "percentile", "per", "none", "no")

# desire types
desire.type <-
  c("low", "l", "high", "h", "extremes", "e")

# initialize function
desire <- function(x, cut1, cut2, cut3, cut4, min = 0, max = 1, scale = 1,
                           cut_type = cut.type, desire_type = desire.type){

  # save arguments
  cut_type <- match.arg(cut.type)
  desire_type <- match.arg(desire.type)

  # back compatabilities for cut type
  if(method == "num") method <- "numerical"
  if(method == "per") method <- "percentile"
  if(method == "no") method <- "none"
  # back compatabilities for desire type
  if(method == "l") method <- "low"
  if(method == "h") method <- "high"
  if(method == "e") method <- "extremes"

  # save non-nas to nna
  if(all(nna <- !is.na(x))) nna <- TRUE

  # use x[nna] and calculate desirability of those values

  # checks
  if(min < 0 | min > 1) stop("min must be between zero and one\n")
  if(max < 0 | max > 1) stop("max must be between zero and one\n")
  if(scale <= 0) stop("scale must be greater than zero\n")

  # create vector of NAs
  y <- rep(NA,length(x))

  # mod to handle NAs!!!!!!!

  # initiate switch for desire types
  switch(desire_type,
         low = {
           # transform data
           y <- ((x - cut2)/(cut1 - cut2))^scale
           switch(cut_type,
                  numerical = {
                    # check
                    if(cut1 >= cut2) stop("cut1 must be less than cut2\n")
                    # if less than cut1 replace with 1
                    y[x < cut1] <- 1
                    # if greater than cut2 replace with 0
                    y[x > cut2] <- 0
                  },
                  percentile = {
                    # check
                    if(cut1 >= cut2) stop("cut1 must be less than cut2\n")
                    # if x is less than cut1 replace with 1
                    y[x < cut1] <- 1
                    # if x is less than cut2 replace with 0
                    y[x > cut2] <- 0
                  },
                  none = {

                    # create articifical cuts
                    cut1 <- min(x)
                    cut2 <- max(x)
                    # if x is less than or equal to min(x) replace with 1
                    y[x = cut1] <- 1
                    # if x is greater than or equal to max(x) replace with 0
                    y[x = cut2] <- 0
                  }

         # start here!!!!!!!!

  )

  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
### desire low numerical end ###


### desire low percentile begin ###
# initialize function
desire_low_per <- function(x, per1, per2,  min = 0, max = 1, scale = 1){

  # checks
  if(per1 >= per2) stop("per1 must be less than per2\n")
  if(min < 0 | min > 1) stop("min must be between zero and one\n")
  if(max < 0 | max > 1) stop("max must be between zero and one\n")
  if(scale <= 0) stop("scale must be greater than zero\n")

  # create cuts
  cut1 <- quantile(x,per1)
  cut2 <- quantile(x,per2)

  # create vector of NAs
  y <- rep(NA,length(x))


  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
### desire low percentile end ###

### desire low no cuts begin ###
# initialize function
desire_low_no_cuts <- function(x, min = 0, max = 1, scale = 1){

  # checks
  if(min < 0 | min > 1) stop("min must be between zero and one\n")
  if(max < 0 | max > 1) stop("max must be between zero and one\n")
  if(scale <= 0) stop("scale must be greater than zero\n")



  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
### desire low no cuts end ###

#######################################################################
## desire high ########################################################
#######################################################################

### desire high numerical begin ###
desire_high_num <- function(x, cut1, cut2, min = 0, max = 1, scale = 1){

  # checks
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
### desire high numerical end ###

### desire high percentile begin ###
desire_high_per <- function(x, per1, per2, min = 0, max = 1, scale = 1){

  # checks
  if(per1 >= per2) stop("per1 must be less than per2\n")
  if(min < 0 | min > 1) stop("min must be between zero and one\n")
  if(max < 0 | max > 1) stop("max must be between zero and one\n")
  if(scale <= 0) stop("scale must be greater than zero\n")

  # create cuts
  cut1 <- quantile(x,per1)
  cut2 <- quantile(x,per2)

  y <- rep(NA,length(x))
  y <- ((x - cut1)/(cut2 - cut1))^scale
  y[x < cut1] <- 0
  y[x > cut2] <- 1

  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
### desire high percentile end ###

### desire high no cuts begin ###
desire_high_per <- function(x, min = 0, max = 1, scale = 1){

  # checks
  if(min < 0 | min > 1) stop("min must be between zero and one\n")
  if(max < 0 | max > 1) stop("max must be between zero and one\n")
  if(scale <= 0) stop("scale must be greater than zero\n")

  # create articifical cuts
  # where cut1 is the lowest value
  # and cut2 is the highest value
  cut1 <- min(x)
  cut2 <- max(x)

  y <- rep(NA,length(x))
  y <- ((x - cut1)/(cut2 - cut1))^scale
  y[x = cut1] <- 0
  y[x = cut2] <- 1

  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
### desire high no cuts end ###

#######################################################################
## desire extremes ####################################################
#######################################################################

### desire extremes numerical start ###
desire_extremes_num <- function(x, cut1, cut2, cut3, cut4, min = 0, max = 1,
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
    if (x[i] <= cut1 | x[i] >= cut4)  y[i] <- 1
    if (x[i] >= cut2 & x[i] <= cut3) y[i] <- 0
    if (x[i] > cut1 & x[i] < cut2) y[i] <- ((x[i] - cut2)/(cut1 - cut2))^scale
    if (x[i] > cut3 & x[i] < cut4) y[i] <- ((x[i] - cut3)/(cut4 - cut3))^scale
  }

  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
### desire extremes numerical end ###

### desire extremes percentile start ###
desire_extremes_per <- function(x, per1, per2, per3, per4, min = 0, max = 1,
                                scale = 1){

  if(per1 >= per2) stop("per1 must be less than per2\n")
  if(per2 >= per3) stop("per2 must be less than per3\n")
  if(per3 >= per4) stop("per3 must be less than per4\n")
  if(min < 0 | min > 1) stop("min must be between zero and one\n")
  if(max < 0 | max > 1) stop("max must be between zero and one\n")
  if(scale <= 0) stop("scale must be greater than zero\n")

  # create cuts
  cut1 <- quantile(x,per1)
  cut2 <- quantile(x,per2)
  cut3 <- quantile(x,per3)
  cut4 <- quantile(x,per4)

  y <- rep(NA, length(x))
  for (i in 1:length(x)){
    if (is.na(x[i])) next
    if (x[i] <= cut1 | x[i] >= cut4)  y[i] <- 1
    if (x[i] >= cut2 & x[i] <= cut3) y[i] <- 0
    if (x[i] > cut1 & x[i] < cut2) y[i] <- ((x[i] - cut2)/(cut1 - cut2))^scale
    if (x[i] > cut3 & x[i] < cut4) y[i] <- ((x[i] - cut3)/(cut4 - cut3))^scale
  }

  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
### desire extremes percentile end ###

### desire extremes no cuts start ###
desire_extremes_no_cuts <- function(x, min = 0, max = 1,
                                    scale = 1){

  # create articifical cuts
  # where cut1 is the lowest value
  # and cut2 is the highest value
  cut1 <- min(x)
  cut4 <- max(x)
  cut2 <- 0
  cut3 <- 0

  if(cut1 >= cut2) stop("cut1 must be less than cut2\n")
  if(cut2 >= cut3) stop("cut2 must be less than cut3\n")
  if(cut3 >= cut4) stop("cut3 must be less than cut4\n")
  if(min < 0 | min > 1) stop("min must be between zero and one\n")
  if(max < 0 | max > 1) stop("max must be between zero and one\n")
  if(scale <= 0) stop("scale must be greater than zero\n")

  y <- rep(NA, length(x))
  for (i in 1:length(x)){
    if (is.na(x[i])) next
    if (x[i] <= cut1 | x[i] >= cut4)  y[i] <- 1
    if (x[i] >= cut2 & x[i] <= cut3) y[i] <- 0
    if (x[i] > cut1 & x[i] < cut2) y[i] <- ((x[i] - cut2)/(cut1 - cut2))^scale
    if (x[i] > cut3 & x[i] < cut4) y[i] <- ((x[i] - cut3)/(cut4 - cut3))^scale
  }

  # rescale:  min to max
  y <- (y * (max - min))  + min

  return(y)
}
### desire extremes no cuts end ###
