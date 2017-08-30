#' Low, high, and extreme desirability functions
#'
#' These functions map numeric variables to a [0, 1] scale where low, high, or
#' extreme values are most desirable.
#'
#' @details
#'
#' @param x Vector of numeric values.
#' @param desire_type Class of desirability function to apply (low, high, or
#' extreme).
#' @param cut_type Class of cuts assigned to desirability function (numerical,
#' percentile, or none).
#' @param cut1,cut2,cut3,cut4 Cut points where the desirability function
#' changes.
#' @param min,max Minimum (default = 0) and maximum (default = 1) desirability
#' scores.
#' @param scale Controls shape of the desirability function. Larger values
#' correspond to more steep and strict curves whereas smaller values correspond
#' to more gradual and inclusive curves.
#' @return Returns a numeric vector of desirability scores.
#' @export

desire_individual <- function(x, desire_type = desire.type, cut_type = cut.type, cut1, cut2, cut3, cut4, min = 0, max = 1, scale = 1){

  # Set desirability function
  desire.type <- c("low", "l", "high", "h", "extremes", "e")
  if(!hasArg(desire_type)) stop("\ndesire_type should be one of the following: 'low', 'high' or 'extremes'\n\nfor more details see help page ?desire()")
  if(!is.element(desire_type, desire.type)) stop("\ndesire_type should be one of the following: 'low', 'l', 'high', 'h', 'extremes', 'e'")
  if(desire_type == "low") desire_type <- "l"
  if(desire_type == "high") desire_type <- "h"
  if(desire_type == "extremes") desire_type <- "e"

  # Set cut types
  cut.type <- c("numerical", "num", "percentile", "per", "none", "no")
  if(!hasArg(cut_type)) cut_type <- "none"
  if(!is.element(cut_type, cut.type)) stop("\ncut_type should be one of the following: 'numerical', 'num', 'percentile', 'per', 'none', 'no'")
  if(cut_type == "none") cut_type <- "no"
  if(cut_type == "numerical") cut_type <- "num"
  if(cut_type == "percentile") cut_type <- "per"

  # Check for appropriate min, max, and scale
  if(min < 0 | min > 1) stop("\nmin must be between zero and one\n")
  if(max < 0 | max > 1) stop("\nmax must be between zero and one\n")
  if(scale <= 0) stop("\nscale must be greater than zero\n")

  # Initialize vector of NAs
  y <- rep(NA,length(x))
  if(all(nna <- !is.na(x))) nna <- TRUE # True if !NA

  switch(desire_type,
         # Low values are most desirable
         l = {
           switch(cut_type,
                  # Numerical cuts
                  num = {
                    if(cut1 >= cut2) stop("\ncut1 must be less than cut2\n")
                    # Apply desirability function
                    y <- ((x - cut2)/(cut1 - cut2))^scale
                    # Override desirability score at cuts
                    y[x[nna] < cut1] <- 1
                    y[x[nna] > cut2] <- 0
                  },
                  # Percentile cuts
                  per = {
                    if(cut1 >= cut2) stop("\ncut1 must be less than cut2\n")
                    # Calculate percentile cuts
                    per1 <- quantile(x[nna],cut1)
                    per2 <- quantile(x[nna],cut2)
                    # Apply desirability function
                    y <- ((x - per2)/(per1 - per2))^scale
                    # Override desirability score at cuts
                    y[x[nna] < per1] <- 1
                    y[x[nna] > per2] <- 0
                  },
                  # No cuts
                  no = {
                    cut1 <- min(x[nna])
                    cut2 <- max(x[nna])
                    # Apply desirability function
                    y <- ((x - cut2)/(cut1 - cut2))^scale
                    # Override desirability score at cuts (min and max)
                    y[x[nna] == cut1] <- 1
                    y[x[nna] == cut2] <- 0
                  }
           )
         },
         # High values are most desirable
         h = {
           switch(cut_type,
                  # Numerical cuts
                  num = {
                    if(cut1 >= cut2) stop("\ncut1 must be less than cut2\n")
                    # Apply desirability function
                    y <- ((x - cut1)/(cut2 - cut1))^scale
                    # Override desirability score at cuts
                    y[x[nna] < cut1] <- 0
                    y[x[nna] > cut2] <- 1
                  },
                  # Percentile cuts
                  per = {
                    if(cut1 >= cut2) stop("\ncut1 must be less than cut2\n")
                    # Calculate percentile cuts
                    per1 <- quantile(x[nna],cut1)
                    per2 <- quantile(x[nna],cut2)
                    # Apply desirability function
                    y <- ((x - per1)/(per2 - per1))^scale
                    # Override desirability score at cuts
                    y[x[nna] < per1] <- 0
                    y[x[nna] > per2] <- 1
                  },
                  # No cuts
                  no = {
                    cut1 <- min(x[nna])
                    cut2 <- max(x[nna])
                    # Apply desirability function
                    y <- ((x - cut1)/(cut2 - cut1))^scale
                    # Override desirability score at cuts (min and max)
                    y[x[nna] == cut1] <- 0
                    y[x[nna] == cut2] <- 1
                  }
           )
         },
         # Extreme values are most desirable
         e = {
           switch(cut_type,
                  # Numerical cuts
                  num = {
                    if(cut2 >= cut3) stop("\ncut2 must be less than cut3\n")
                    if(cut3 >= cut4) stop("\ncut3 must be less than cut4\n")
                    for (i in 1:length(x)){
                      if (is.na(x[i])) next
                      # Apply desirability function
                      if (x[i] > cut1 & x[i] < cut2) y[i] <- ((x[i] - cut2)/(cut1 - cut2))^scale
                      if (x[i] > cut3 & x[i] < cut4) y[i] <- ((x[i] - cut3)/(cut4 - cut3))^scale
                      # Override desirability score between and outside cuts
                      if (x[i] <= cut1 | x[i] >= cut4)  y[i] <- 1
                      if (x[i] >= cut2 & x[i] <= cut3) y[i] <- 0
                    }
                  },
                  # Percentile cuts
                  per = {
                    if(cut2 >= cut3) stop("\ncut2 must be less than cut3\n")
                    if(cut3 >= cut4) stop("\ncut3 must be less than cut4\n")
                    # Calculate percentile cuts
                    per1 <- quantile(x[nna],cut1)
                    per2 <- quantile(x[nna],cut2)
                    per3 <- quantile(x[nna],cut3)
                    per4 <- quantile(x[nna],cut4)
                    for (i in 1:length(x)){
                      if (is.na(x[i])) next
                      # Apply desirability function
                      if (x[i] > per1 & x[i] < per2) y[i] <- ((x[i] - per2)/(per1 - per2))^scale
                      if (x[i] > per3 & x[i] < per4) y[i] <- ((x[i] - per3)/(per4 - per3))^scale
                      # Override desirability score between and outside cuts
                      if (x[i] <= per1 | x[i] >= per4)  y[i] <- 1
                      if (x[i] >= per2 & x[i] <= per3) y[i] <- 0
                    }
                  },
                  # No cuts
                  no = {
                    cut1 <- min(x[nna])
                    cut4 <- max(x[nna])
                    cut2 <- 0
                    cut3 <- 0
                    for (i in 1:length(x)){
                      if (is.na(x[i])) next
                      # Apply desirability function
                      if (x[i] > cut1 & x[i] < cut2) y[i] <- ((x[i] - cut2)/(cut1 - cut2))^scale
                      if (x[i] > cut3 & x[i] < cut4) y[i] <- ((x[i] - cut3)/(cut4 - cut3))^scale
                      # Override desirability score between and outside cuts
                      if (x[i] <= cut1 | x[i] >= cut4)  y[i] <- 1
                      if (x[i] >= cut2 & x[i] <= cut3) y[i] <- 0
                    }
                  }
           )
         }
  )

  # Rescale according min to max and return desirability score
  y <- (y * (max - min)) + min; return(y)

}
