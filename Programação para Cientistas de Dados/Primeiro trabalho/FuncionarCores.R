dftest <- data.frame(x = c(1,6,4,2,7,4,5,3), y = c(1,1,6,4,4,6,2,3))

quickhull = function(df){
  
  convex_hull = data.frame(x = c(), y = c())
  
  sorted = df[order(df$x),]
  
  p1 = sorted[1,]
  p2 = sorted[nrow(sorted),]
  
  convex_hull = rbind(convex_hull, p1,p2)
  
  sorted = sorted[-c(1,nrow(sorted)),]
  
  above = create_segment(p1,p2,sorted,1)
  below = create_segment(p1,p2,sorted,-1)
  
  #plot sorted points
  plot(dftest, type = "p", col = "blue", pch = 16, cex = 1)
  #line between p1 and p2
  lines(c(p1$x,p2$x), c(p1$y,p2$y), col = "red", lwd = 2)
  #above points
  points(above, col = "green", pch = 16, cex = 1.5)
  #below points
  points(below, col = "orange", pch = 16, cex = 1.5)
  
  max_point = find_distance(p1,p2,sorted)
  lines(c(max_point$x,p1$x), c(max_point$y,p1$y), col = "red", lwd = 2)
  lines(c(max_point$x,p2$x), c(max_point$y,p2$y), col = "red", lwd = 2)
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
    dist = abs(a*sorted[i,]$x + b*sorted[i,]$y + c)/sqrt(a*a + b*b)
    if (dist > max_dist){
      max_dist = dist
      max_point = sorted[i,]
    }
  }
  return (max_point)
}

quickhull(dftest)

