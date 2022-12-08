quickhull = function(v){

    if(length(v) < 2) return(v)

    convex_hull = c()

    sorted = sort(v)
    p1 = sorted[1]
    p2 = sorted[length(sorted)]

    convex_hull = c(convex_hull, p1, p2)
    sorted = sorted[order(sorted)]
    p1 = sorted[1]
    p2 = sorted[length(sorted)]

    sorted = sorted[-c(2, length(sorted))]
    
    above = create_segment(p1, p2,sorted,1)
    below = create_segment(p1, p2,sorted,-1)

    convex_hull = c(convex_hull, quickhull2(p1,p2,above,"above"))
    convex_hull = c(convex_hull, quickhull2(p1,p2,below,"below"))

}

create_segment = function(p1, p2, sorted, side){

    above = c()
    below = c()

    if(p2[1] - p1[1] == 0) return(c())

    m = (p2[2] - p1[2]) / (p2[1] - p1[1])
    c = p1[2] - m * p1[1]

    for(coordinate in sorted){
        if(coordinate[1] * m + c > coordinate[2]){
            above = c(above, coordinate)
        } else {
            below = c(below, coordinate)
        }
    }

    if(side == 1){
        return(above)
    } else {
        return(below)
    }

}

find_distance = function(p1, p2, p3){
    a = p1[2] - p2[2]
    b = p2[1] - p1[1]
    c = p1[1] * p2[2] - p2[1] * p1[2]

    return(abs(a * p3[1] + b * p3[2] + c) / sqrt(a^2 + b^2))

}

    max_dist = 0
    max_point = NULL

    for(i in 1:length(sorted)){
        dist = distance(p1, p2, sorted[i])
        if(dist > max_dist){
            max_dist = dist
            max_point = sorted[i]
        }
    }

    if(is.null(max_point)) return(c())

    sorted = sorted[-which(sorted == max_point)]

    above = create_segment(p1, max_point, sorted, 1)
    below = create_segment(p1, max_point, sorted, -1)

    return(c(above, max_point, below))

