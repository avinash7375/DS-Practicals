#
data("iris")
head(iris)

#
summary(iris)

#
myPr <- prcomp(iris[, -5], scale = T)
plot(iris$Sepal.Length, iris$Sepal.Width)

#
plot(scale(iris$Sepal.Length), scale(iris$Sepal.Width))

#
myPr

#
summary(myPr)

#
plot(myPr, type='l')

#
biplot(myPr, scale = 0)

#
str(myPr)

#
myPr$x

#
iris2 <- cbind(iris, myPr$x[, 1:2])
head(iris2)

#
library(pls)
names(iris)

#
pcModel <- pcr(Sepal.Length~Species + Sepal.Width + Petal.Length + Petal.Width, ncomp = 3, data = iris, scale = T)
iris$pred <- predict(pcModel, iris, ncomp = 2)
head(iris)

