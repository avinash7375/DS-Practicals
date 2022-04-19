# install.packages("ggplot2")
library(ggplot2)
scatter <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width))
scatter + geom_point(aes(color = Species, shape = Species)) + theme_bw() + xlab("Sepal Length") + ylab("Sepal Width") + ggtitle("Sepal Length-Width")

# new
ggplot(data = iris, aes(Sepal.Length, fill = Species)) + theme_bw() + geom_density(alpha = 0.25) + labs(x = "Sepal.Length", title="Species vs Sepal Length")

# new 
vol <- ggplot(data = iris, aes(x = Sepal.Length))
vol + stat_density(aes(ymax = ..density.., ymin = -..density.., fill = Species, color = Species), geom = "ribbon", position = "identity") + facet_grid(.~Species) + coord_flip() + theme_bw() + labs(x = "Sepal Length", title="Species vs Sepal Length")

# new
vol <- ggplot(data = iris, aes(x = Sepal.Width))
vol + stat_density(aes(ymax = ..density.., ymin = -..density.., fill = Species, color = Species), geom = "ribbon", position = "identity") + facet_grid(.~Species) + coord_flip() + theme_bw() + labs(x = "Sepal Width", title="Species vs Sepal Width")

# Method 1
irisData <- iris[, 1:4]
totalWSS <- c()
for (i in 1:15) {
  clusterIRIS <- kmeans(irisData, centers = i)
  totalWSS[i] <- clusterIRIS$tot.withinss
}
plot(x = 1:15, y = totalWSS, type = "b", xlab = "Number Of Clusters", ylab = "Within groups sum-of-squares")

# Method 2: NB Cluster
# install.packages("NbClust")
library(NbClust)
par(mar = c(2, 2, 2, 2))
nb <- NbClust(irisData, method = "kmeans")

# new
hist(nb$Best.nc[1, ], breaks = 15, main = "Histogram for Number of Clusters")

# Method 3
# install.packages("vegan")
library(vegan)
modelData <- cascadeKM(irisData, 1, 10, iter = 100)
plot(modelData, sortg = TRUE)
modelData$results[2, ]
which.max(modelData$results[2, ])

# Method 4
library(cluster)
cl <- kmeans(iris[, -5], 2)
dis <- dist(iris[, -5]) ^ 2
sil = silhouette(cl$cluster, dis)
plot(sil, main = "Clustering Data with Silhoutte plot using 2 Clusters", col = c("cyan", "blue"))

library(cluster)
cl <- kmeans(iris[, -5], 8)
dis <- dist(iris[, -5]) ^ 2
sil = silhouette(cl$cluster, dis)
plot(sil, main = "Clustering Data with Silhoutte plot using 8 Clusters", col = c("cyan", "blue", "orange", "yellow", "red", "gray", "green", "maroon"))

# install.packages("factoextra")
# install.packages("clustertend")
library(factoextra)
library(clustertend)
genx <-  function(x) {
  runif(length(x), min(x), (max(x)))
}
randomDf <- apply(iris[, -5], 2, genx)
randomDf <- as.data.frame(randomDf)
iris[, -5] <-  scale(iris[, -5])
randomDf <-  scale(randomDf)
res <- get_clust_tendency(iris[, -5], n = nrow(iris) - 1, graph = FALSE)
res$hopkins_stat
hopkins(iris[, -5], n = nrow(iris) - 1)
res <- get_clust_tendency(randomDf, n = nrow(randomDf) - 1, graph = FALSE)
res$hopkins_stat

