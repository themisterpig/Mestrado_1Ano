data(iris)
head(iris)
newdata <- trees[order(trees$Height),]
newdata
newdata <-trees[order(-trees$Height),]

sort(Sepal.Length)
newdata <- iris[order(Sepal.Length), ]

iris[order(-iris$Sepal.Length,iris$Sepal.Width),]
iris


aggregate ( mtcars , by = list ( mtcars $cyl , mtcars $vs ) , FUN = mean 
            
            
data(trees)
head(trees)

x <- c(7,8,9)
is.vector(x)
