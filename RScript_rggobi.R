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