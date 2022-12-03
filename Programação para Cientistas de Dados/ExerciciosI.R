x <- seq(2,5,by=0.1)
vectori <- c()
for(i in x){
  vectori = c(vectori, exp(i)*exp(i))
}

print(vectori)

#-----------------------------------
#ex2
vector2 <- 0

for( i in 5:50){
  vector2 = vector2 + i^4+3*i^2
}
vector2


#-----------------------------
#ex3

x3 <-seq(2,10)
y <- seq(5,50)
vector3 <- 0

for(i in x3){
  for(j in y){
    vector3 = vector3 + ((j^4 + 3*j^1/2)/i)
  }
}
vector3
#----------------
#ex4
y = floor(runif(10,0,1000))

for(i in y){
  if(i > 600){
    print(match(i,y))
  }
}
#-----------------
#ex5
print(y[order(y)])

#--------------------
#ex6

myfun <-function(x){
  my_vec = c()
  for(i in 3:length(x)){
   aux=((x[i]+x[i-1]+x[i-2])/3)
   my_vec = c(my_vec,aux)
  }
  return(my_vec)
}
vector = c(1,5,7,3,4,9)
print(myfun(vector))
#-------------------------
#ex7
myFunction <- function(xVector) {
vector1=c()
for(i in xVector){
    if(i < 0){
      aux = i^2+2*i +3
      
    }else if(x>=0 & x <2){
      aux = i +3
    }else{
      aux = i^2 + 4*i - 7
    }
    vector1 = c(vector1,aux)
  }

return(vector1)
}
vector = c(1,5,7,3,4,9)
print(myFunction(vector))
#-------------------
#ex8
sum(myFunction(vector))
