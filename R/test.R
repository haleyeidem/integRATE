# Make test data
df <- data.frame(rnorm(1000, 0, 0.75))
df[,2] <- data.frame(c(runif(150, 0, 0.001), runif(400, 0.001, 0.2), runif(250, 0.2, 0.5), runif(200, 0.5, 1)))
colnames(df) <- c("Log Fold Change", "Adjusted P-Value")

# Calculate individual desirabilities for both columns
d <- data.frame(desire_individual(df$`Adjusted P-Value`, desire_type = 'low', cut_type = 'none'))
d[,2] <- data.frame(desire_individual(df$`Log Fold Change`, desire_type = 'extremes', cut_type = 'none'))

# Calculate overall desirability
d[,3] <- data.frame(desire_overall(d[,1], d[,2]))

