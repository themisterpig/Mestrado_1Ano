data(iris)
head(iris)
newdata <- trees[order(trees$Height),]
newdata
newdata <-trees[order(-trees$Height),]

iris
newdata <- iris[order(iris$Sepal.Length), ]
newdata

newdata <- iris[order(-iris$Sepal.Length,iris$Sepal.Width),]
newdata

iris




aggregate ( mtcars , by = list ( mtcars $cyl , mtcars $vs ) , FUN = mean)
            
            
irisTrain= iris[1:100,]
irisTrain

irisTest = iris[100:150,]
irisTest


rbind(irisTrain,irisTest)

