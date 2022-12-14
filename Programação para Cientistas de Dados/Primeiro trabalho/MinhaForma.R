
quickhull = function(df){
  if(length(df)<=2){
    return(df)
  }
  
  convex_hull = c()
  sort = points[order(points[,1]),]
  p1 = sort[1]
  p2 = sort[length(sort)]
  convex_hull <- rbind(convex_hull, data.frame(x = c(p1[1]), y = c(p1[2])))
  convex_hull <- rbind(convex_hull, data.frame(x = c(p2[1]), y = c(p2[2])))
  print(sort)
  sort = sort[-c(1,),]
  sort = sort[-c(length(sort),)]
  print(sort)
  above = createSegment(p1, p2, sort,TRUE)
  below = createSegment(p1, p2, sort,FALSE)
  
  points(df, pch = 19, col = "black")
  points(above, pch = 19, col = "green")
  points(below, pch = 19, col = "red")
  lines(c(p1[1],p2[1]), c(p1[2],p2[2]), col = "black")
  
  convex_hull == convex_hull + quickhull2(p1,p2,above, "above")
  convex_hull == convex_hull + quickhull2(p1,p2,below, "below")
}



createSegment = function(p1, p2, df,beloworabove){
 above = data.frame(x = c(), y = c())
 below = data.frame(x = c(), y = c())
 
 if(p1[1] - p2[1]==0 || is.na(p1[1]) || is.na(p2[1])){
   return(NULL)
 }
 
 m = (p2[2]-p1[2]) / (p2[1] - p1[1])
 c = -m * p1[1] + p1[2]

 for(i in 1:nrow(df)){
   if(is.na(df[i,]$y) || is.na(df[i]$x)){
     break
   }
   greater =(df[i]$y > m * (df[]$x) + c)
   less =(df[i]$y < m * (df[i]$x) + c)
   
   
   #y > mx + c means it is above the line
   if(greater){
     above = rbind(above, data.frame(x = c(df[i,]$x), y = c(df[i,]$y)))
   }else if(less){
     below = rbind(below, data.frame(x = c(df[i,]$x), y = c(df[i,]$y)))
   }
 }
 if(beloworabove){
   return(above)
 }else{
   return(below)
 }
}


find_distance = function(p1,p2,p3){
  #using formula ax + bx + c = 0
  a = p1[2] - p2[2]
  b = p2[1] - p1[1]
  c = p1[1] * p2[2] - p2[1]*p1[2]
  
  result = (abs(a*p3[1] + b*p3[2] + c)/sqrt(a*a + b*b))
  
  
  return(result)
  
  
}

quickHull2 = function(p1,p2,segment,flag){

  #Exit case for recursion
  if (length(segment) == 0){
    return(c())
  }
  
  convex_hull = c()
  
  #calculate the distance of every point from the line to find the farthest point
  farthest_distance = -1
  farthest_point = 0
  for(point in segment){
    distance = find_distance(p1,p2,point)
    if(distance > farthest_distance){
      farthest_distance = distance
      farthest_point = point
    }
  }
  return(farthest_point)
}


points = c(2.5,1.5,6,1,4,5.5,2,4,7,4.5,4,5,5,2,3,3.5,3.5,3,4,3,4.5,3,6,3,4,2)
points = matrix(points, ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x","y")))
p1 = points[1,]
p1[2]

quickhull(points)



