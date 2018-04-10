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

# setup for rJava
#Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_161')
#library(rJava)

# source("http://www.ggobi.org/downloads/install.r")

library(rggobi)
g <- ggobi(iris)
glyph_colour(g[1]) <- as.numeric(iris$Species)

g <- ggobi(iris)
clustering <- hclust(dist(iris[,1:4]),
                     method="average")
glyph_colour(g[1]) <- cutree(clustering, 3)



