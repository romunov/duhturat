# Misc functions not fitting anywhere else.
# 
# Author: Roman Luštrik, 30.8.2012
###############################################################################

# Extract feature coordinates from SpatialPolygons object.
getEdges <- function(x) {
    #x = SpatialPolygons object
    stopifnot(class(x) == "SpatialPolygons")
    lapply(x@polygons, function(y) {
                y@Polygons[[1]]@coords
            })
}

# This function finds two possible circle centers. Circles are defined by two
# points located somewhere on the circumference.
# Function by Dason http://stackoverflow.com/a/12267154/322912

findCenter <- function(x, y, R){
    dy <- diff(y)
    dx <- diff(x)
    # The radius needs to be at least as large as half the distance
    # between the two points of interest
    minrad <- (1/2)*sqrt(dx^2 + dy^2)
    if(R < minrad){
        stop("Specified radius can't be achieved with this data")
    }
    
    # I used a parametric equation to create the line going through
    # the mean of the two points that is perpendicular to the line
    # connecting the two points
    # 
    # f(k) = ((x1+x2)/2, (y1+y2)/2) + k*(y2-y1, x1-x2)
    # That is the vector equation for our line.  Then we can
    # for any given value of k calculate the radius of the circle
    # since we have the center and a value for a point on the
    # edge of the circle.  Squaring the radius, subtracting R^2,
    # and equating to 0 gives us the value of t to get a circle
    # with the desired radius.  The following are the coefficients
    # we get from doing that
    A <- (dy^2 + dx^2)
    B <- 0
    C <- (1/4)*(dx^2 + dy^2) - R^2
    
    # We could just solve the quadratic equation but eh... polyroot is good enough
    k <- as.numeric(polyroot(c(C, B, A)))
    
    # Now we just plug our solution in to get the centers
    # of the circles that meet our specifications
    mn <- c(mean(x), mean(y))
    ans <- rbind(mn + k[1]*c(dy, -dx),
            mn + k[2]*c(dy, -dx))
    
    colnames(ans) = c("x", "y")
    
    ans
}

# Find euclidean distance between two points. x is a matrix with x and y
# for columns.
eucDist <- function(x) {
    sqrt(diff(c(x[1, 1], x[2, 1]))^2 + diff(c(x[1, 2], x[2, 2]))^2)
}

# This function takes in a pair of coordinates of points that lie
# somewhere on the circumference of two possible circles. Once both
# possible points are found, the one lying closest to the
closestEdgeCenter <- function(x, pts, r, ctr) {
    ds <- function(x) {
        sqrt(diff(c(x[1, 1], x[2, 1]))^2 + diff(c(x[1, 2], x[2, 2]))^2)
    }
    
    pt.pair <- pts[x, ]
    
#    plot(edge.points, xlim = c(-120, 20), ylim = c(-20, 80), type = "n", asp = 1)
#    points(pt.pair)
    
    potential.points <- findCenter(x = pt.pair[, 1], y = pt.pair[, 2], R = r)
#    points(potential.points, pch = "+")
#    apply(potential.points, 1, FUN = function(x) {
#                plot(gBuffer(SpatialPoints(matrix(x, nrow = 1)), width = r), add = TRUE)
#            })
    
    # The most likely candidate for the correct home range is probably
    # the one which has center closer to the center of MCP. Another way
    # of determining would also be to see which home range includes
    # more points from the MCP.
    closest.to.Pp <- apply(potential.points, 1, FUN = function(x, tr = ctr) {
                eucDist(rbind(x, tr))
            })
    center <- potential.points[which(closest.to.Pp == min(closest.to.Pp)), , drop = FALSE]
    center
}

# To create a Polygon (or other *Polygon* objects) we need to close
# the ring of coordinates. What we do in this function is repeat the
# first coordinate at the end.
# x data.frame or matrix with two columns (coordinates)
closeRing <- function(x) {
    rbind(x[1:nrow(x), ], x[1, ])
}

# This function will find pairs of edge points of a polygon.
# x = SpatialPolygons object or data.frame
getEdgePairs <- function(x) {
    fx <- fortify(x)
    zoo::rollapply(1:nrow(fx), FUN = function(x) {
                x
            }, width = 2)
}

# X is a SpatialPoints or data.frame or matrix with two columns
getEdgePoints <- function(x) {
    if (class(x) == "SpatialPoints") edge.points <- fortify(x)[, c("long", "lat")]
    edge.points <- x
    names(edge.points) <- c("x", "y")
    edge.points
}

# x = a list of SpatialPolygons
getIntersection <- function(x) {
    obj <- x[[1]]
    for (i in 2:length(x)) {
        obj <- gIntersection(spgeom1 = obj, spgeom2 = x[[i]])
    }
    obj
}

getPotentialArea <- function(x, hr.size) {
    # if NULL, don't bother
    if (is.null(x)) return(NULL)
    
    frt <- as.data.frame(coordinates(x))
    
    # if not enough points to do a chull
    if (nrow(frt) == 1) {
        gi <- gBuffer(x, width = hr.size, quadsegs = 50)
    }
    
    if (nrow(frt) >= 2) {
        frt <- closeRing(frt)
        frt.chull <- frt[chull(frt), ]
        # Find area where possible potential home range centers
        # could be located.
        # Find area around individual point of a convex polygon.
        pot.locs <- apply(frt.chull, 1, function(x, hr.size) {
                    gBuffer(SpatialPoints(matrix(x, nrow = 1)), 
                            width = hr.size, quadsegs = 50)
                }, hr.size = hr.size)
        
        # Find an intersection.
        gi <- getIntersection(pot.locs) # in workhorse.R
#                plot(sa, axes = TRUE, xlim = c(-150, 100))
#                points(x)
#                lapply(pot.locs, plot, add = TRUE)
#                plot(gi, lwd = 2, border = "blue", add = TRUE)
    }
    gi
}

# x = result of getPotentialArea
# hr.size = assumed home range size
# sa = SpatialPolygons of sampling area
# n.size = number of points to be used 
estimateSP <- function(x, hr.size, sa, n.size) {
    if (is.null(x)) return(NULL)
    sampled.x <- spsample(x, n = n.size, type = "random")
    est <- apply(as.data.frame(sampled.x), MARGIN = 1, function(y, hr.size, sa) {
                y <- SpatialPoints(matrix(y, nrow = 1))
                plg <- gBuffer(y, width = hr.size, quadsegs = 50)
                sa.orig.area <- gArea(sa)
                sa.plg.difference <- gDifference(spgeom1 = plg, spgeom2 = sa)
                
                # gDifference returns the value of plg NOT inside sa. So if
                # sa.plg.difference is NULL, this means that the entire plg
                # is inside the sampling area. Zero should not be possible,
                # since points need to lie inside the sampling polygon and
                # the polygon we're trying to fit randomly needs to have all
                # the sampled points inside. Even at an extreme one point,
                # zero should not be possible.
                if (is.null(sa.plg.difference)) {
                    sa.plg.difference <- 1
                } else {
                    clipped <- gArea(sa.plg.difference)/sa.orig.area
                    sa.plg.difference <- 1 - clipped
                }
                
#                Sys.sleep(1)
#                plot(sa, axes = TRUE, xlim = c(-150, 100))
#                plot(y, pch = 1, add = TRUE)
#                plot(plg, add = TRUE)
                #diagnostics to see if all poinst fall inside circle
#            gCovers(spgeom1 = plg, spgeom2 = pp) 
#                plot(plg, add = TRUE, border = "lightgrey", lty = "dotted")
                sa.plg.difference
            }, hr.size = hr.size, sa = sa)
}

# x = list of SpatialPoints
# y = list of SpatialPolygons
# s = sleep time in seconds
checkPlot <- function(x, y, s, sa) {
    if (is.null(x) | is.null(y)) return(NULL)
    Sys.sleep(s)
    bb <- bbox(sa)
    plot(sa, xlim = c(bb[1,1]-50, bb[1,2]+50), ylim = c(bb[2,1]-50, bb[2,2]+50))
    plot(y, add = TRUE)
    plot(x, add = TRUE)
    invisible()
}

bbox.ltraj <- function(obj) {
    xr <- do.call("rbind", lapply(obj, function(m) range(m$x)))
    yr <- do.call("rbind", lapply(obj, function(m) range(m$y)))
    xr <- c(xmin = min(xr[, 1]), xmax = max(xr[, 2]))
    yr <- c(ymin = min(yr[, 1]), ymax = max(yr[, 2]))
    out <- rbind(xr, yr)
    colnames(out) <- c("min", "max")
    rownames(out) <- c("x", "y")
    out
}