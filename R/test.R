source("http://bioconductor.org/biocLite.R")
biocLite("biomaRt")
library(biomaRt)

# Get 1000 random gene names
ensembl = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
genes <- data.frame(getBM(attributes = "hgnc_symbol", mart = ensembl))

# Make toy data
toy_data[,1] <- data.frame(sample(genes[,1], 1000))
toy_data[,2] <- data.frame(rnorm(1000, 0, 0.75))
toy_data[,3] <- data.frame(c(runif(150, 0, 0.001), runif(400, 0.001, 0.2), runif(250, 0.2, 0.5), runif(200, 0.5, 1)))
toy_data[,4] <- data.frame(rnorm(1000, 0, 10))
colnames(toy_data) <- c("Gene", "Log Fold Change", "Adjusted P-Value", "Mean Expression")

# Calculate individual desirabilities for both columns
d <- data.frame(desire_individual(toy_data$`Log Fold Change`, desire_type = 'extremes', cut_type = 'none'))
d[,2] <- data.frame(desire_individual(toy_data$`Adjusted P-Value`, desire_type = 'low', cut_type = 'none'))
d[,3] <- data.frame(desire_individual(toy_data$`Mean Expression`, desire_type = 'high', cut_type = 'none'))
colnames(d) <- c("Log Fold Change", "Adjusted P-Value", "Mean Expression")

# Calculate overall desirability
D <- data.frame(desire_overall(d[,1], d[,2], d[,3]))
colnames(D) <- c("Overall")

# Create data frame
for_plot <- data.frame(toy_data[,1], D, d)
colnames(for_plot) <- c("Gene", "Overall", "Log Fold Change", "Adjusted P-Value", "Mean Expression")
