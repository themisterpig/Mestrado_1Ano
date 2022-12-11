dftest <- data.frame(x = c(1,6,4,2,7,4,5,3), y = c(1,1,6,4,4,6,2,3))

quickhull = function(df){
  
  convex_hull = data.frame(x = c(), y = c())
  
  sorted = df[order(df$x),]
  
  p1 = sorted[1,]
  p2 = tail(sorted, n =1)
  
  convex_hull = rbind(convex_hull, p1,p2)
  sorted <- head(sorted, - 1) 
  sorted = sorted[-c(1),]
  
  
  above = create_segment(p1,p2,sorted,1)
  below = create_segment(p1,p2,sorted,-1)
  
  #plot sorted points
  plot(dftest, type = "p", col = "blue", pch = 16, cex = 1)
  #line between p1 and p2
  lines(c(p1$x,p2$x), c(p1$y,p2$y), col = "blue", lwd = 2)
  #above points
  points(above, col = "green", pch = 16, cex = 1.5)
  #below points
  points(below, col = "orange", pch = 16, cex = 1.5)
  
  max_point_above = find_distance(p1,p2,above)
  max_point_below = find_distance(p1,p2,below)
  lines(c(max_point_above$x,p1$x), c(max_point_above$y,p1$y), col = "red", lwd = 2)
  lines(c(max_point_above$x,p2$x), c(max_point_above$y,p2$y), col = "red", lwd = 2)
  lines(c(max_point_below$x,p1$x), c(max_point_below$y,p1$y), col = "red", lwd = 2)
  lines(c(max_point_below$x,p2$x), c(max_point_below$y,p2$y), col = "red", lwd = 2)
  
  #convex_hull == convex_hull + quickhull2(p1,p2,above, "above")
  #convex_hull == convex_hull + quickhull2(p1,p2,below, "below")
  
  return(convex_hull)
  
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
    print(df[i,2])
    print(df[i,1])
    
    if(is.na(df[i,2]) || is.na(df[i,1])){
      break
    }
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
  #max_dist = 0
  #max_point = data.frame(x = c(), y = c())
  
  a = p1$y - p2$y
  b = p2$x - p1$x
  c = p1$x * p2$x - p2$x*p1$y
  
  dist = (abs(a*sorted[1,]+ b*sorted[,1] + c)/sqrt(a*a + b*b))
  
  #for (i in 1:nrow(sorted)){
  #  dist = abs(a*sorted[i,]+ b*sorted[i,] + c)/sqrt(a*a + b*b)
  #  if (dist > max_dist){
  #    max_dist = dist
  #    max_point = sorted[i,]
  #  }
  #}
  
  return (dist$y)
}

quickhull2 = function(p1,p2,segment,flag){
  
  convex_hull = data.frame(x = c(), y = c())
  
  if(length(segment$x)==0){
    return(c())
  }
  
  farthest_distance = -1
  farthest_point =0
  
  for(i in 1:nrow(segment)) {       # for-loop over rows
    segment[i, ]$x -> x
    point = data.frame(x = c(segment[i, ]$x), y = c(segment[i, ]$y))
    
    distance = find_distance(p1,p2,point)
    if(distance > farthest_distance){
      farthest_distance = distance
      farthest_point = point
    }
  }
  convex_hull[nrow(convex_hull) + 1,] <- c(farthest_point$x,farthest_point$y)
  #segment %>%  filter(x==farthest_point$x | y > farthest_point$y)
  segment<-segment[!(segment$x==farthest_point$x | segment$y==farthest_point$y),]
  point1above = create_segment(p1,farthest_point,segment,1)
  point1below = create_segment(p1,farthest_point,segment,-1)
  point2above= create_segment(farthest_point,p2,segment,1)
  point2below= create_segment(farthest_point,p2,segment,-1)
  
  if(flag == "above"){
    convex_hull = convex_hull + quickhull2(p1,farthest_point,point1above, "above")
    convex_hull = convex_hull + quickhull2(farthest_point,p2,point2above, "above")
  }
  else{
    convex_hull = convex_hull + quickhull2(p1,farthest_point,point1below, "below")
    convex_hull = convex_hull + quickhull2(farthest_point,p2,point2below, "below")
  }
  return(convex_hull)
}


quickhull(dftest)

