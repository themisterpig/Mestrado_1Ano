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
  
  
  max_point_above = find_distance(p1,p2,above)
  max_point_below = find_distance(p1,p2,below)
  convex_hull = rbind(convex_hull, max_point_above,max_point_below)
  
  convex_hull = quickhull2(convex_hull,sorted2)
  create_lines(convex_hull,p1,p2,above,below,max_point_above,max_point_below,sorted2)
  return(convex_hull)
  
}

create_lines = function(convex_hull,p1,p2,above,below,max_point_above,max_point_below,sorted){
  #plot sorted points
  plot(sorted, type = "p", col = "blue", pch = 16, cex = 1)
  #line between p1 and p2
  lines(c(p1$x,p2$x), c(p1$y,p2$y), col = "blue", lwd = 2)
  #above points
  points(above, col = "green", pch = 16, cex = 1.5)
  #below points
  points(below, col = "orange", pch = 16, cex = 1.5)
  
  lines(c(max_point_above$x,p1$x), c(max_point_above$y,p1$y), col = "red", lwd = 2)
  lines(c(max_point_above$x,p2$x), c(max_point_above$y,p2$y), col = "red", lwd = 2)
  lines(c(max_point_below$x,p1$x), c(max_point_below$y,p1$y), col = "red", lwd = 2)
  lines(c(max_point_below$x,p2$x), c(max_point_below$y,p2$y), col = "red", lwd = 2)
  
  points(convex_hull, col = "black", pch = 16, cex = 2)

}


create_cluster = function(pontos){
  cluster = pontos[order(pontos$x),] 
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

quickhull2 = function(convex_hull,sorted){
  for(num in 1:nrow(convex_hull)){
    for(j in 1:nrow(convex_hull)){
    p1 = data.frame(x = c(convex_hull[num,]$x), y = c(convex_hull[num,]$y))
    p2 = data.frame(x = c(convex_hull[j,]$x), y = c(convex_hull[j,]$y))
    convex_hull = rbind(convex_hull,find_distance(p1,p2,sorted)) 
    
    }
  }
  
  
  return(convex_hull[!duplicated(convex_hull), ])
}
 
dftest <- data.frame(x = runif(30,1,15), y = runif(30,1,15))

#valores <- dftest 
#valores2 <- dftest 
#valores3 <- dftest
#valores4 = dftest
#valores5 = dftest
quickhull(dftest)
quickhull(valores)
quickhull(valores3)

create_cluster(quickhull(dftest))
