quickhull = function(v){
  if(length(v) < 2) return(v)
  
  convex_hull = c()
  
  sorted <- points[order(points[, 1]), ]
  p1 = sorted[1,]
  p2 = sorted[nrow(sorted),]
  
  convex_hull = c(convex_hull, p1, p2)
  
  sorted <- sorted[-c(1, nrow(sorted)), ]
  above = create_segment(p1, p2,sorted,1)
  below = create_segment(p1, p2,sorted,-1)
  
 
  plot(points, type = "p", col = "red", pch = 16, cex = 2)
  points(above, col = "blue", pch = 16, cex = 2)
  points(below, col = "blue", pch = 16, cex = 2)
  
  lines(c(p1[1],p2[1]), c(p1[2],p2[2]), col = "black")
  
  return(convex_hull)
  
}
create_segment = function(p1, p2, sorted, side){
  above = matrix(NA, nrow = 0, ncol = 2)
  below = matrix(NA, nrow = 0, ncol = 2)
  
  
  
  if(p2[1] - p1[1] == 0){
    return(c())
  }
  
  m = (p2[2]-p1[2]) / (p2[1] - p1[1])
  c = -m * p1[1] + p1[2]
  
  for(i in 1:nrow(sorted)){
    greater =(sorted[i,2]> m * sorted[i,1] + c)
    less =(sorted[i,2]< m * sorted[i,1] + c)

    #y > mx + c means it is above the line
    if(greater){
      above = rbind(above, c(sorted[1],sorted[2]))
    }else if(less){
      below = rbind(below, c(sorted[1],sorted[2]))

    }
  }
  
  if(side ==1){
    return(above)
  }else{
    return(below)
  }
  
}

points = matrix(sample(1:15, 20, replace = TRUE), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("x","y")))
points

quickhull(points)

