# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2017-03-21 14:57:33 x86_64-w64-mingw32 

# Rattle version 4.1.0 user 'MKHIGGI'

# This log file captures all Rattle interactions as R commands. 

Export this log to a file using the Export button or the Tools 
# menu to save a log of all your activity. This facilitates repeatability. For example, exporting 
# to a file called 'myrf01.R' will allow you to type in the R Console 
# the command source('myrf01.R') and so repeat all actions automatically. 
# Generally, you will want to edit the file to suit your needs. You can also directly 
# edit this current log in place to record additional information before exporting. 
 
# Saving and loading projects also retains this log.

# We begin by loading the required libraries.

library(rattle)   # To access the weather dataset and utility commands.
library(magrittr) # For the %>% and %<>% operators.

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2017-03-21 14:57:45 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- diab

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#============================================================
# Rattle timestamp: 2017-03-21 14:57:46 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 7555 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 5288 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 1133 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 1134 observations

# The following variable selections have been noted.

crs$input <- c("Age", "Gender", "Diabetes", "BMI",
     "HHIncome")

crs$numeric <- c("Age", "BMI")

crs$categoric <- c("Gender", "Diabetes", "HHIncome")

crs$target  <- "PhysActive"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2017-03-21 14:58:02 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 7555 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.8*crs$nobs) # 6044 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.2*crs$nobs) # 1511 observations
crs$test <- NULL

# The following variable selections have been noted.

crs$input <- c("Age", "Gender", "BMI", "HHIncome",
     "PhysActive")

crs$numeric <- c("Age", "BMI")

crs$categoric <- c("Gender", "HHIncome", "PhysActive")

crs$target  <- "Diabetes"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2017-03-21 14:58:11 x86_64-w64-mingw32 

# KMeans 

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Generate a kmeans cluster of size 10.

crs$kmeans <- kmeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"), 10)

#============================================================
# Rattle timestamp: 2017-03-21 14:58:11 x86_64-w64-mingw32 

# Report on the cluster characteristics. 

# Cluster sizes:

paste(crs$kmeans$size, collapse=' ')

# Data means:

colMeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"))

# Cluster centers:

crs$kmeans$centers

# Within cluster sum of squares:

crs$kmeans$withinss

# Time taken: 0.02 secs

#============================================================
# Rattle timestamp: 2017-03-21 14:58:20 x86_64-w64-mingw32 

# Display a scatterplot matrix for the KMeans clustering. 

# Select a sample from the dataset to calculate the statistics.

set.seed(42)
smpl <- sample(length(crs$kmeans$cluster), 4000)

# Generate a data plot.

plot(na.omit(crs$dataset[crs$sample, intersect(crs$input, crs$numeric)][smpl,]), col=crs$kmeans$cluster)
title(main="",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Generate a discriminant coordinates plot.

cluster::clusplot(na.omit(crs$dataset[crs$sample, intersect(crs$input, crs$numeric)]), crs$kmeans$cluster, color=TRUE, shade=TRUE, main='Discriminant Coordinates diab')


#============================================================
# Rattle timestamp: 2017-03-21 14:58:54 x86_64-w64-mingw32 

# Hierarchical Cluster 

# The 'amap' package provides the 'hclusterpar' function.

library(amap, quietly=TRUE)

# Generate a hierarchical cluster of the data.

crs$hclust <- hclusterpar(na.omit(crs$dataset[crs$sample, crs$numeric]), 
    method="euclidean", link="ward", nbproc=1)

# Time taken: 1.62 secs

#============================================================
# Rattle timestamp: 2017-03-21 14:59:05 x86_64-w64-mingw32 

# Dendrogram Plot 

# The 'ggplot2' package provides the 'ggplot' function.

library(ggplot2, quietly=TRUE)

# The 'ggdendro' package provides the 'dendro_data' function.

library(ggdendro, quietly=TRUE)

# Generate the dendrogram plot.

ddata <- dendro_data(crs$hclust, type="rectangle")
g <- ggplot(segment(ddata))
g <- g + geom_segment(aes(x = y, y = x, xend = yend, yend = xend))
g <- g + scale_y_discrete(labels = ddata$label$label)
g <- g + labs(x="Height", y="Observation")
g <- g + ggtitle(expression(atop("Cluster Dendrogram diab", atop(italic("Rattle 2017-Mar-21 14:59:05 MKHIGGI")))))
print(g)

# The 'fpc' package provides the 'cluster.stats' function.

library(fpc, quietly=TRUE)

# List the suggested cluster centers for each cluster

centers.hclust(na.omit(crs$dataset[crs$sample, crs$numeric]), crs$hclust, 3)

# Generate cluster statistics using the fpc package.

cluster.stats(dist(na.omit(crs$dataset[crs$sample, crs$numeric])), cutree(crs$hclust, 3))


# Generate a data plot.

plot(crs$dataset[crs$sample, c(1, 4)], col=cutree(crs$hclust, 3))
 title(main="",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Generate a discriminant coordinates plot.

cluster::clusplot(na.omit(crs$dataset[crs$sample, c(1, 4)]), cutree(crs$hclust, 3), color=TRUE, shade=TRUE, main='Discriminant Coordinates diab')

