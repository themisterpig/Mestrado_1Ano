df <- data.frame(x = c(2.5,6,4,2,7,4,5,3,4,5.5,4.5,6,4), y = c(1.5,1,5.5,4,4,5,2,3,4,4,3,3,2))
df
plot(df, col = "red", pch = 16)

quickhull = function(df){
  if(length(df$x)<=2){
    return(df)
  }
  
  convex_hull = c()
  sort = df[order(df$x),]
  p1 = sort[1,]
  p2 = sort[length(sort$x),]
  
  convex_hull <- rbind(convex_hull, data.frame(x = c(p1$x), y = c(p1$y)))
  convex_hull <- rbind(convex_hull, data.frame(x = c(p2$x), y = c(p2$y)))

  sort = sort[-1,]
  sort = sort[-length(sort$x),]
  
  above = createSegment(p1,p2,sort)
  below = createSegment(p1,p2,sort)
  
  convex_hull = rbind(convex_hull, quickhull2(p1,p2,above,"above"))
  convex_hull = rbind(convex_hull, quickhull2(p1,p2,below,"below"))
  
  return(convex_hull)
  
}

quickhull2 = function(p1,p2,segment,flag){
}
createSegment = function(p1,p2,v){
}
findDistance = function(p1,p2,p3){
}

quickhull(df)
