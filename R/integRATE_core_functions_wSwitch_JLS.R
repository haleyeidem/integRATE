## make toy data
#df <- data.frame(rnorm(1000, 0, 0.75))
#df[,2] <- data.frame(c(runif(150, 0, 0.001), runif(400, 0.001, 0.2), runif(250, 0.2, 0.5), runif(200, 0.5, 1)))
#colnames(df) <- c("Log Fold Change", "Adjusted P-Value")

## integRATE core desirability function ##
desire <- function(x, cut1, cut2, cut3, cut4, min = 0, max = 1, scale = 1, cut_type = cut.type, desire_type = desire.type){ # open function
  cut.type <- c("numerical", "num", "percentile", "per", "none", "no") # possible cut types
  desire.type <- c("low", "l", "high", "h", "extremes", "e") # possible desire types

  if(!hasArg(cut_type)) cut_type <- "none" # checks
  if(!hasArg(desire_type)) stop("\ndesire_type should be one of the following: 'low', 'high' or 'extremes'\n\nfor more details see help page ?desire()")
  if(!is.element(desire_type, desire.type)) stop("\ndesire_type should be one of the following: 'low', 'l', 'high', 'h', 'extremes', 'e'")
  if(!is.element(cut_type, cut.type)) stop("\ncut_type should be one of the following: 'numerical', 'num', 'percentile', 'per', 'none', 'no'")
  if(min < 0 | min > 1) stop("\nmin must be between zero and one\n")
  if(max < 0 | max > 1) stop("\nmax must be between zero and one\n")
  if(scale <= 0) stop("\nscale must be greater than zero\n")

  if(missing(cut_type)) cut_type <- "none" # back compatabilities
  if(cut_type == "none") cut_type <- "no"
  if(cut_type == "numerical") cut_type <- "num"
  if(cut_type == "percentile") cut_type <- "per"
  if(desire_type == "low") desire_type <- "l"
  if(desire_type == "high") desire_type <- "h"
  if(desire_type == "extremes") desire_type <- "e"

  y <- rep(NA,length(x)) # vector of NAs
  if(all(nna <- !is.na(x))) nna <- TRUE # True if !NA

  switch(desire_type, # open desire type switch
         l = { # open low
           switch(cut_type, # switch for cut types within low
                  num = { # open numerical
                    if(cut1 >= cut2) stop("\ncut1 must be less than cut2\n") # check
                    y <- ((x - cut2)/(cut1 - cut2))^scale # transform data
                    y[x[nna] < cut1] <- 1
                    y[x[nna] > cut2] <- 0
                  }, # close numerical
                  per = { # open percentile
                    if(cut1 >= cut2) stop("\ncut1 must be less than cut2\n") # check
                    per1 <- quantile(x[nna],cut1); per2 <- quantile(x[nna],cut2) # create percentile cuts
                    y <- ((x - per2)/(per1 - per2))^scale # transform data
                    y[x[nna] < per1] <- 1
                    y[x[nna] > per2] <- 0
                  }, # close percentile
                  no = { # open none
                    y <- ((x - cut2)/(cut1 - cut2))^scale # transform data
                    y[x[nna] == min(x[nna])] <- 1
                    y[x[nna] == max(x[nna])] <- 0
                  } # close none
                  ) # close nested cut_type switch
         }, # close low
         h = { # open high
           switch(cut_type, # switch for cut types within high
                  num = { # open numerical
                    if(cut1 >= cut2) stop("\ncut1 must be less than cut2\n") # check
                    y <- ((x - cut1)/(cut2 - cut1))^scale # transform data
                    y[x[nna] < cut1] <- 0
                    y[x[nna] > cut2] <- 1
                  }, # close numerical
                  per = { # open percentile
                    per1 <- quantile(x[nna],cut1); per2 <- quantile(x[nna],cut2) # create percentile cuts
                    y <- ((x - cut1)/(cut2 - cut1))^scale # transform data
                    y[x[nna] < per1] <- 0
                    y[x[nna] > per2] <- 1
                  }, # close percentile
                  no = { # open none
                    y <- ((x - cut1)/(cut2 - cut1))^scale # transform data
                    y[x[nna] == min(x[nna])] <- 0
                    y[x[nna] == max(x[nna])] <- 1
                  } # close none
                  ) # end nested cut_type switch
         }, # close high
         e = { # open extremes
           switch(cut_type, # switch for cut types within extremes
                  num = { # open numerical
                    if(cut2 >= cut3) stop("\ncut2 must be less than cut3\n") # check
                    if(cut3 >= cut4) stop("\ncut3 must be less than cut4\n")
                    for (i in 1:length(x)){ # open for loop
                      if (is.na(x[i])) next # skip NA
                      if (x[i] <= cut1 | x[i] >= cut4)  y[i] <- 1
                      if (x[i] >= cut2 & x[i] <= cut3) y[i] <- 0
                      if (x[i] > cut1 & x[i] < cut2) y[i] <- ((x[i] - cut2)/(cut1 - cut2))^scale
                      if (x[i] > cut3 & x[i] < cut4) y[i] <- ((x[i] - cut3)/(cut4 - cut3))^scale
                    } # close for loop
                  }, # close numerical
                  per = { # open percentile
                    if(cut2 >= cut3) stop("\ncut2 must be less than cut3\n") # check
                    if(cut3 >= cut4) stop("\ncut3 must be less than cut4\n")
                    # create cuts
                    per1 <- quantile(x[nna],cut1); per2 <- quantile(x[nna],cut2); per3 <- quantile(x[nna],cut3); per4 <- quantile(x[nna],cut4) # create cuts
                    for (i in 1:length(x)){ # open for loop
                      if (is.na(x[i])) next # skip NA
                      if (x[i] <= per1 | x[i] >= per4)  y[i] <- 1
                      if (x[i] >= per2 & x[i] <= per3) y[i] <- 0
                      if (x[i] > per1 & x[i] < per2) y[i] <- ((x[i] - per2)/(per1 - per2))^scale
                      if (x[i] > per3 & x[i] < per4) y[i] <- ((x[i] - per3)/(per4 - per3))^scale
                    } # close for loop
                  }, # close percentile
                  no = { # open none
                    cut1 <- min(x[nna]); cut4 <- max(x[nna]); cut2 <- 0; cut3 <- 0 # create cuts
                    for (i in 1:length(x)){ # open for loop
                      if (is.na(x[i])) next # skip NA
                      if (x[i] <= cut1 | x[i] >= cut4)  y[i] <- 1
                      if (x[i] >= cut2 & x[i] <= cut3) y[i] <- 0
                      if (x[i] > cut1 & x[i] < cut2) y[i] <- ((x[i] - cut2)/(cut1 - cut2))^scale
                      if (x[i] > cut3 & x[i] < cut4) y[i] <- ((x[i] - cut3)/(cut4 - cut3))^scale
                    } # close for loop
                  } # close none
                  ) # end nested cut_type switch
         } # close extremes
    ) # close desire_type switch
  y <- (y * (max - min)) + min; return(y)# rescale according min to max and return
} # close function
