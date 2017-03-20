# source("http://bioconductor.org/biocLite.R")
# biocLite("biomaRt")
# library(biomaRt)

# Make toy data
# Get 1000 random gene names
# ensembl = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
# genes <- data.frame(getBM(attributes = "hgnc_symbol", mart = ensembl))

# toy_data[,1] <- data.frame(sample(genes[,1], 1000))
# toy_data[,2] <- data.frame(rnorm(1000, 0, 0.75))
# toy_data[,3] <- data.frame(c(runif(150, 0, 0.001), runif(400, 0.001, 0.2), runif(250, 0.2, 0.5), runif(200, 0.5, 1)))
# toy_data[,4] <- data.frame(rnorm(1000, 0, 10))
# colnames(toy_data) <- c("Gene", "Log Fold Change", "Adjusted P-Value", "Mean Expression")

# Ackerman ###
# Calculate individual desirabilities for all columns
d1 <- data.frame(desire_individual(Ackerman$log2FC,
                                   desire_type = 'extremes',
                                   cut_type = 'none',
                                   min = 0.1))
d1[,2] <- data.frame(desire_individual(Ackerman$padj.value,
                                       desire_type = 'low',
                                       cut_type = 'none',
                                       min = 0.1))

# Calculate overall desirability
D1 <- data.frame(desire_overall(d1[,1],
                                d1[,2]))

# Create data frame
p1 <- data.frame(Ackerman[,1],
                 D1,
                 d1)
colnames(p1) <- c("Gene",
                  "Ackerman: Overall",
                  "Ackerman: Log2 Fold Change",
                  "Ackerman: Adjusted P-Value")

# Chim ###
# Calculate individual desirabilities for all columns
d2 <- data.frame(desire_individual(Chim$logFC,
                                   desire_type = 'extremes',
                                   cut_type = 'none',
                                   min = 0.1))
d2[,2] <- data.frame(desire_individual(Chim$padj.value,
                                       desire_type = 'low',
                                       cut_type = 'none',
                                       min = 0.1))

# Calculate overall desirability
D2 <- data.frame(desire_overall(d2[,1],
                                d2[,2]))

# Create data frame
p2 <- data.frame(Chim[,1],
                 D2,
                 d2)
colnames(p2) <- c("Gene",
                  "Chim: Overall",
                  "Chim: Log Fold Change",
                  "Chim: Adjusted P-Value")

# Heng ###
# Calculate individual desirabilities for all columns
d3 <- data.frame(desire_individual(Heng$logFC,
                                   desire_type = 'extremes',
                                   cut_type = 'none',
                                   min = 0.1))
d3[,2] <- data.frame(desire_individual(Heng$padj.value,
                                       desire_type = 'low',
                                       cut_type = 'none',
                                       min = 0.1))

# Calculate overall desirability
D3 <- data.frame(desire_overall(d3[,1],
                                d3[,2]))

# Create data frame
p3 <- data.frame(Heng[,1],
                 D3,
                 d3)
colnames(p3) <- c("Gene",
                  "Heng: Overall",
                  "Heng: Log Fold Change",
                  "Heng: Adjusted P-Value")

# Heng_proteomics ###
# Calculate individual desirabilities for all columns
d4 <- data.frame(desire_individual(Heng_proteomics$logFC,
                                   desire_type = 'extremes',
                                   cut_type = 'none',
                                   min = 0.1))
d4[,2] <- data.frame(desire_individual(Heng_proteomics$p.value,
                                       desire_type = 'low',
                                       cut_type = 'none',
                                       min = 0.1))

# Calculate overall desirability
D4 <- data.frame(desire_overall(d4[,1],
                                d4[,2]))

# Create data frame
p4 <- data.frame(Heng_proteomics[,1],
                 D4,
                 d4)
colnames(p4) <- c("Gene",
                  "Heng Proteomics: Overall",
                  "Heng Proteomics: Log Fold Change",
                  "Heng Proteomics: P-Value")

# Mayor_Lynn ###
# Calculate individual desirabilities for all columns
d5 <- data.frame(desire_individual(Mayor_Lynn$FC,
                                   desire_type = 'extremes',
                                   cut_type = 'none',
                                   min = 0.1))
d5[,2] <- data.frame(desire_individual(Mayor_Lynn$p.value,
                                       desire_type = 'low',
                                       cut_type = 'none',
                                       min = 0.1))

# Calculate overall desirability
D5 <- data.frame(desire_overall(d5[,1],
                                d5[,2]))

# Create data frame
p5 <- data.frame(Mayor_Lynn[,1],
                 D5,
                 d5)
colnames(p5) <- c("Gene",
                  "Mayor-Lynn: Overall",
                  "Mayor-Lynn: Fold Change",
                  "Mayor-Lynn: P-Value")

# Fernando ###
# Calculate individual desirabilities for all columns
d6 <- data.frame(desire_individual(Fernando$`Log Fold Change`,
                                   desire_type = 'extremes',
                                   cut_type = 'none',
                                   min = 0.1))
d6[,2] <- data.frame(desire_individual(Fernando$`Adjusted P-Value`,
                                       desire_type = 'low',
                                       cut_type = 'none',
                                       min = 0.1))

# Calculate overall desirability
D6 <- data.frame(desire_overall(d6[,1],
                                d6[,2]))

# Create data frame
p6 <- data.frame(Fernando[,1],
                 D6,
                 d6)
colnames(p6) <- c("Gene",
                  "Fernando: Overall",
                  "Fernando: Log Fold Change",
                  "Fernando: Adjusted P-Value")

# Cruickshank ###
# Calculate individual desirabilities for all columns
d7 <- data.frame(desire_individual(Cruickshank$`Log Fold Change`,
                                   desire_type = 'extremes',
                                   cut_type = 'none',
                                   min = 0.1))
d7[,2] <- data.frame(desire_individual(Cruickshank$`Adjusted P-Value`,
                                       desire_type = 'low',
                                       cut_type = 'none',
                                       min = 0.1))

# Calculate overall desirability
D7 <- data.frame(desire_overall(d7[,1],
                                d7[,2]))

# Create data frame
p7 <- data.frame(Cruickshank[,1],
                 D7,
                 d7)
colnames(p7) <- c("Gene",
                  "Cruickshank: Overall",
                  "Cruickshank: Log Fold Change",
                  "Cruickshank: Adjusted P-Value")

# PA ###
# Calculate individual desirabilities for all columns
d8 <- data.frame(desire_individual(PA$Status,
                                   desire_type = 'high',
                                   cut_type = 'num',
                                   cut1 = 0.1,
                                   cut2 = 0.9,
                                   min = 0.1))

# Create data frame
p8 <- data.frame(PA[,1],
                 d8)
colnames(p8) <- c("Gene",
                  "Protein Atlas: Overall")

# Overall ###
d <- data.frame(desire_overall(p1[,2],
                               p2[,2],
                               p3[,2],
                               p4[,2],
                               p5[,2],
                               p6[,2],
                               p7[,2],
                               p8[,2]))
D <- data.frame(Fernando[,1],
                d)
colnames(D) <- c("Gene",
                 "Overall Desirability")
D2 <- data.frame(D,
                 p1[,2],
                 p2[,2],
                 p3[,2],
                 p4[,2],
                 p5[,2],
                 p6[,2],
                 p7[,2],
                 p8[,2])
colnames(D2) <- c("Gene",
                  "Overall Desirability",
                  "Ackerman (RNA-Seq)",
                  "Chim (Microarray)",
                  "Heng (Microarray)",
                  "Heng (Proteomics)",
                  "Mayor-Lynn (Microarray)",
                  "Fernando (Methylation)",
                  "Cruickshank (Methylation)",
                  "Protein Atlas")

# Plots ###
dat.1 <- D2[rev(order(D2[,2])),]

P1 <- ggplot() +
  geom_point(aes(x = seq(length(dat.1[,2])),
                 y = dat.1[,2]),
             size = 2,
             color = "black",
             alpha = 0.5) +
  labs(x = "Rank",
       y = "Overall Desirability Score") +
  scale_y_continuous(limits = c(0, 0.5),
                     breaks=seq(0, 0.5, 0.1)) +
  theme_classic() +
  theme(axis.text = element_text(size = 16,
                                 face = "bold"),
        axis.title = element_text(size = 20,
                                  face = "bold"))

P1

P2 <- ggplot() +
  geom_point(
    aes(
      x = seq(1,10,1),
      y = dat.1[,2][1:10]),
    size = 5,
    color = "black",
    alpha = 1) +
  scale_x_continuous(
    breaks = 1:10, labels = c(
      paste('1\n', dat.1[1,1]),
      paste('2\n', dat.1[2,1]),
      paste('3\n', dat.1[3,1]),
      paste('4\n', dat.1[4,1]),
      paste('5\n', dat.1[5,1]),
      paste('6\n', dat.1[6,1]),
      paste('7\n', dat.1[7,1]),
      paste('8\n', dat.1[8,1]),
      paste('9\n', dat.1[9,1]),
      paste('10\n', dat.1[10,1]))) +
  labs(x = "Overall Desirability Rank",
       y = "Overall Desirability Score") +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16,
                                  face = "bold"))

P2

dat.2 <- D2[rev(order(D2[,2])),]

dat.2[,3:length(dat.2)] <- lapply(-dat.2[,3:length(dat.2)],
                              rank,
                              ties.method = 'min')
dat <- dat.2[1:10,]
dat.melt <- melt(dat[,-2],
                 id.vars = "Gene",
                 value.name = "Individual Rank",
                 variable.name = "Data")

dat.melt[,3] <- log10(dat.melt[,3])

P3 <- ggplot() +
  geom_line(data = dat.melt,
            aes(
              x = rep(seq(1,length(dat[,1]),1),length(dat)-2),
              y = dat.melt[,3],
              colour = variable),
            size = 1.5,
            alpha = 0.75) +
  scale_x_continuous(
    breaks = 1:10, labels = c(
      paste('1\n', dat.melt[1,1]),
      paste('2\n', dat.melt[2,1]),
      paste('3\n', dat.melt[3,1]),
      paste('4\n', dat.melt[4,1]),
      paste('5\n', dat.melt[5,1]),
      paste('6\n', dat.melt[6,1]),
      paste('7\n', dat.melt[7,1]),
      paste('8\n', dat.melt[8,1]),
      paste('9\n', dat.melt[9,1]),
      paste('10\n', dat.melt[10,1]))) +
  labs(x = "Overall Desirability Rank",
       y = "Individual Rank (log10)") +
  theme_classic() +
  theme(legend.position="right",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16,
                                  face = "bold"),
        legend.text=element_text(size = 10)) +
  scale_colour_brewer(type = "qual",
                      palette = "Paired",
                      guide = guide_legend(title = "Data")) +
  scale_y_reverse()

P3
