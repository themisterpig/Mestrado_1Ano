'''Exercicio for loop'''

fruits = c("Banana","Apple","Mango","Pear","Kiwi")

for(val in fruits){
  print(val)
}


for ( x in fruits){
  print(paste(x,switch(x,"Banana"=1,"Apple"=2,"Mango"=3,"Pear"=5,"Kiwi"=5)))
}


for ( x in fruits){
  ifelse ( switch(x, "Banana"=1,"Apple"=2,"Mango"=3,"Pear"=4,"Kiwi"=5)<3,print(x),"")
}

----------//-------------- ou

for ( x in fruits){
  xa <- switch(x, "Banana"=1,"Apple"=2,"Mango"=3,"Pear"=4,"Kiwi"=5)
  if (xa < 3){
    print(x)
  }
}


fruitPrices <- list("Banana"= 4,"Apple"=1,"Mango"=3,"Pear"=2.5,"Kiwi"=1.5)

for (fruit in names(fruitPrices)){
  print(paste(fruit, fruitPrices[[fruit]]))
}


mat1.data <- c(1,2,3,4,5,6,7,8,9)
mat1 <- matrix(mat1.data,nrow=3,ncol=3,byrow=TRUE)
mat1
for(i in 1:ncol(mat1)){
  for( j in 1:nrow(mat1)){
    if (mat1[i,j] > 4){
      print(mat1[i,j])
    }
  }
}
