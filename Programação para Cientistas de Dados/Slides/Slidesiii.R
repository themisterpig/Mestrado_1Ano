stockprice = 50
change=c(5,-5)
while(stockprice >= 45){
  i = sample(1:2,1)
  stockprice = stockprice + change[i]
  print(stockprice)
}



carlist = list("bmw"=5000,"mercedes"=30000,"opel"=10000)


for (car in names(carlist)){
  if(carlist[[car]] > 22000){
    break
  }
  print(paste(car, carlist[[car]]))
}


x = matrix(1:9, nrow = 3, ncol = 3)
x
for(i in 1:ncol(x)){
  for( j in 1:nrow(x)){
    if(x[j,i] > 6){
      break
    }
    if (x[j,i] > 4){
      print(x[j,i])
    }
  }
}


x = matrix(1:9, nrow = 3, ncol = 3)
myfunction <- function(x){
  summ = sum(x)
  sdd = sd(x)
  meann = mean(x)
  relist = list("sum"=summ,"sdd" = sdd,"mean"=meann)
  return(relist)
}

listaB <-myfunction(x)
print(listaB)





recursive.binary <- function(n){
  if(n>1){
    recursive.binary(as.integer(n/2))
  }
  cat(n %% 2)
}
recursive.binary(52)
