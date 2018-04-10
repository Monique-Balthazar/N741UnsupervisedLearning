# ====================================
# Unsupervised Learning
#
# Melinda Higgins
# dated 4/10/2018
# ====================================

# load the iris dataset
# 4 measurements of flower dimensions
# for 3 species of irises,
# 50 flowers per species for a 
# total of 150 flowers (number of rows = 150)
# with 4 measurements each (number of columns = 4)

data(iris)
head(iris)

# lool at some 2 dimensional plots
plot(iris$Sepal.Length, iris$Sepal.Width)
plot(iris$Petal.Length, iris$Petal.Width)

# add color by species
plot(iris$Sepal.Length, iris$Sepal.Width,
     col = iris$Species)

# matrix scatterplot - use car package
library(car)
car::scatterplotMatrix(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width|Species, 
                   data=iris,
                   main="Three IRIS Species")

# install Ggobi - see instructions at
# http://www.ggobi.org/
# http://www.ggobi.org/downloads/
# use instructions for your operating system
# then install rggobi - R package
# if prompted, install GTK and GGobi

library(rggobi)

# View iris dataset - use KNOWN species to color
g <- ggobi(iris)
glyph_colour(g[1]) <- as.numeric(iris$Species)

# View iris dataset - use Cluster Analysis
# unsupervised clusters to color
g <- ggobi(iris)
clustering <- hclust(dist(iris[,1:4]),
                     method="average")
glyph_colour(g[1]) <- cutree(clustering, 3)

# look at clustering a bit further
# default method = "complete" linkage
clusters <- hclust(dist(iris[,1:4]))
plot(clusters)

# if we cut to get 3 clusters
# see how these line up with the
# original species
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

# notice that "cluster 2"
# is not "clean" - it has both
# versicolor and virginica
# clusters 1 and 3 are pretty good

# try another linkage method
# set method = "average"
clusters <- hclust(dist(iris[, 3:4]), 
                   method = "average")
plot(clusters)

# compare again
# cleaner clusters this time
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

# the heatmap() function is another good
# visualization tool - you get
# cluster analyses in 2 dimensions
# showing the associations between subjects
# and between variables
irismatrix <- as.matrix(iris[,1:4])
heatmap(irismatrix)

# the colors can be customized
# palette <- colorRampPalette(c('#f0f3ff','#0033BB'))(256)
palette <- colorRampPalette(c("green","blue"))(256)
heatmap(irismatrix, Colv=F, scale='none', col=palette)
