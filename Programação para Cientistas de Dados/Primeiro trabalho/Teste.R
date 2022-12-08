quickhull = function(v){
  #calculate the convex hull of a set of vertices v that
  
  convex_hull = c()
  
  #find the maximum and minimum points on the x-axis
  #sort the points by x-axis
  v = v[order(v[,1]),]
  p1 = v[1,]
  p2 = v[length(v)]
  
  convex_hull = convex_hull + p1 + p2
  
  #remove from the dataset as they are now part of the convex hull
  v = v[-c(1,length(v)),]
  
  #determine points above and below the line
  above = create_segment(p1, p2, v)
  below = create_segment(p1, p2, v)
  convex_hull == convex_hull + quickhull2(p1,p2,above, "above")
  convex_hull == convex_hull + quickhull2(p1,p2,below, "below")
  
  return(convex_hull)
}
#create_segment
create_segment = function(p1,p2,segment){
  #create a segment of points above or below the line
  #p1 and p2 are the points on the line
  #segment is the set of points to be tested
  #flag is either "above" or "below"
  #return a segment of points above or below the line
  
  new_segment = c()
  for(point in segment){
    if (point[2] > p1[2] && point[2] > p2[2]){
      new_segment = new_segment + point
    }
    if (point[2] < p1[2] && point[2] < p2[2]){
      new_segment = new_segment + point
    }
  }
  return(new_segment)
}
#quickhull2
quickhull2 = function(p1,p2,segment,flag){
  print("entrou")
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
  
  convex_hull = convex_hull + farthest_point
  
  #point is now in the convex hull so remove it from the segment
  segment.remove(farthest_point)
  
  point1above = create_segment(p1,farthest_point,segment)
  point1below = create_segment(p1,farthest_point,segment)
  point2above= create_segment(farthest_point,p2,segment)
  point2below= create_segment(farthest_point,p2,segment)
  
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
find_distance = function(p1,p2,point){
  #calculate the distance of a point from a line
  #p1 and p2 are the points on the line
  #point is the point to be tested
  #return the distance of the point from the line
  
  #calculate the distance of the point from the line
  #using the formula
  #distance = |(y2-y1)x0 - (x2-x1)y0 + x2y1 - y2x1|/sqrt((y2-y1)^2 + (x2-x1)^2)
  distance = abs((p2[2]-p1[2])*point[1] - (p2[1]-p1[1])*point[2] + p2[1]*p1[2] - p2[2]*p1[1])/sqrt((p2[2]-p1[2])^2 + (p2[1]-p1[1])^2)
  return(distance)
}

points = c(2.5,1.5,6,1,4,5.5,2,4,7,4.5,4,5,5,2,3,3.5,3.5,3,4,3,4.5,3,6,3,4,2)

points = matrix(points, ncol = 2, byrow = TRUE)
#plot the lines convex_hull
plot(points, type = "n", xlab = "x", ylab = "y")
#find the convex hull
convex_hull <- quickhull(points)
#plot the convex hull
lines(convex_hull, col = "blue")

