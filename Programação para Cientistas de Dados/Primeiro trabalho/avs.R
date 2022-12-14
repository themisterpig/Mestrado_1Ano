#dftest <- data.frame(x = c(1,6,4,2,7,4,5,3,4,2), y = c(1,1,6,4,4,6,2,3,4,3))

quickhull = function(df){
  
  convex_hull = data.frame(x = c(), y = c())
  
  sorted = df[order(df$x),]
  sorted2 = df[order(df$x),]
  
  p1 = sorted[1,]
  p2 = tail(sorted, n =1)
  
  convex_hull = rbind(convex_hull, p1,p2)
  sorted <- head(sorted, - 1) 
  sorted = sorted[-c(1),]
  
  
  above = create_segment(p1,p2,sorted,1)
  below = create_segment(p1,p2,sorted,-1)
  
  convex_hull = rbind(convex_hull,quickhull2(above,p1,p2,1))
  convex_hull = rbind(convex_hull,quickhull2(below,p1,p2,-1))
  create_lines(convex_hull,p1,p2,above,below,max_point_above,max_point_below,sorted2)
  return(convex_hull[!duplicated(convex_hull), ])
  
}

create_lines = function(convex_hull,p1,p2,above,below,max_point_above,max_point_below,sorted){
  #plot sorted points
  plot(sorted, type = "p", col = "black", pch = 16, cex = 1)
  points(convex_hull, col = "blue", pch = 16, cex = 2)
  
}


create_segment = function(p1,p2,df,side){
  above = data.frame(x = c(), y = c())
  below = data.frame(x = c(), y = c())
  
  if(p2$x - p1$x == 0){
    return (data.frame(x = c(), y = c()))
  }
  m = (p2$y - p1$y)/(p2$x - p1$x)
  c = p1$y - m*p1$x
  
  for (i in 1:nrow(df)){
    if (df[i,2] > m*df[i,1] + c)
      above = rbind(above, df[i,])
    else
      below = rbind(below, df[i,])
  }
  
  if(side == 1){
    return(above)
  }else{
    return(below)
  }
  
}

find_distance = function(p1,p2,sorted){
  max_dist = 0
  max_point = data.frame(x = c(), y = c())
  
  a = p1$y - p2$y
  b = p2$x - p1$x
  c = p1$x * p2$x - p2$x*p1$y
  
  for (i in 1:nrow(sorted)){
    dist = abs(a*sorted[i,]$x+ b*sorted[i,]$y + c)/sqrt(a*a + b*b)
    if (length(dist)> 0 && dist > max_dist){
      max_dist = dist
      max_point = sorted[i,]
    }
  }
  return (max_point)
}
removeFromDataFrame = function(segment,point){
  if(dim(point)[1] == 0){
    return(segment)
  }
  for(num in 1:nrow(segment)){
    if(segment[num,]$x ==point$x && segment[num,]$y==point$y){
      index = num
      break
    }
  }
  segment = segment[-c(index),]
  
  return(segment)
}
quickhull2 = function(segment,p1,p2,side){

  if (dim(segment)[1] == 0 || is.null(dim(p1)) || is.null(dim(p2)) || is.null(dim(segment))){
    return(convex_hull)
    
  }
  convex_hull = data.frame(x = c(), y = c())
  
  farthest_point = find_distance(p1,p2,segment)
  
  convex_hull = rbind(convex_hull,find_distance(p1,p2,segment))
  
  segment = removeFromDataFrame(segment,farthest_point)
  
  print(convex_hull)
  point1above = create_segment(p1,farthest_point,segment,1)
  point1below = create_segment(p1,farthest_point,segment,-1)
  point2above = create_segment(p2,farthest_point,segment,1)
  point2below = create_segment(p2,farthest_point,segment,-1)
  
  if(side == 1){
    convex_hull = rbind(convex_hull,quickhull2(p1,farthest_point,point1above,1))
    convex_hull = rbind(convex_hull,quickhull2(farthest_point,p2,point2above,1))
  }else{
    convex_hull = rbind(convex_hull,quickhull2(p1,farthest_point,point1below,-1))
    convex_hull = rbind(convex_hull,quickhull2(farthest_point,p2,point2below,-1))
  }
  
  return(convex_hull)
  
  
}

dftest <- data.frame(x = runif(15,1,10), y = runif(15,1,10))

#valores <- dftest 
#valores2 <- dftest 
#valores3 <- dftest
quickhull(dftest)

quickhull(valores)
quickhull(valores3)

create_cluster(quickhull(dftest))
