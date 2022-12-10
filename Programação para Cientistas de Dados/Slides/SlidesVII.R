double_odd = function(m){
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      if(m[i,j] %% 2 !=0){
        m[i,j] = m[i,j]*2
      }
    }
  }
  return(m)
}

m <- matrix(1:9, nrow=3, ncol=3)
print(double_odd(m))


average = function(m){
  v <- m[m%%2==0]
  #Compute the average
  mean(v)
  #Compute the standard deviation
  sd(v)
  print(mean(v))
  print(sd(v))
  
}

average(m)



create_vector = function(){
  vector = c()
  for (i in 1:20) {
    vector <- c(vector, (2^i)/i)
  }
  
  print(vector)
}
create_vector()


exercise2_2 = function(){
  xVec <- sample(0:999,150)
  yVec <- sample(0:999,150)
  
  finalvector= c()
  
  for(i in 2:150){
    finalvector = c(finalvector,yVec[i] - xVec[i-1])
  }
  print(finalvector)
}
exercise3()



exercise3_2 = function(){
  xVec = c(1,2,3,4,5,6,7,8,9,10)
  finalvector = c()
  media = mean(xVec)
  for( i in xVec){
    finalvector = c(finalvector,abs((i-media))^0.5)
  }
  print(finalvector)
}
exercise4()
#--------/////////-----------
xVec = c(1,2,3,4)
tmpFn1 = function(x){
  final_vector = c(x[1])
  for(a in 2:length(x)){
    print(x[a])
    final_vector= c(final_vector,x[a]^a)
  }  
  return(tmpFn2(final_vector))
}
tmpFn2 = function(x){
  final_vector = c(x[1])
  for(a in 2:length(x)){
    final_vector= c(final_vector,(x[a])/a)
  }  
  return(final_vector)
}

print(tmpFn1(xVec))


#----------------//--------------


quadrant = function(x){
  if(x>0 && x<90){
    print("Quadrant 1")
  }else if(x>90 && x <180){
    print("Quadrant 2")
  }else if(x>180 && x<270){
    print("Quadrant 3")
  }else if(x > 270 && x < 360){
    print("Quadrant 4")
  }else if(x> 360 && x < 450){
    print("Quadrant 1")
  }
}
quadrant(200)

#----------------------------------------
yVec = c(1,2,3,4)

testLoop2 = function(x){
  finalVector = c()
for(a in 1:length(x)){
  finalVector = c(finalVector,exp(a))
} 
}
#----------------------------------------
