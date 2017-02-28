# naredim nekaj to�k in jih probam zarotirat
library(sp)
library(maptools)
library(rgeos)
source("d:/workspace/trpd/workhorse.R")
Q <- structure(list(x = c(-26.2499852159262, -80.5468700352413, -74.2968689049604, 
                        -10.624982390224, 70.2344072327848, 69.0625320208572, 5.39064550612077, 
                        -26.2499852159262), y = c(72.9941789797681, 44.2151034025968, 
                        -16.8314205489786, -82.2384104970951, -57.3837543168108, 28.953472414703, 
                        74.3023187787304, 72.9941789797681)), .Names = c("x", "y"), row.names = c(NA, 
                8L), class = "data.frame")
Q <- SpatialPolygons(list(Polygons(list(Polygon(Q)), ID = 1)))
Pp <- structure(list(x = c(-69.9999931278923, -65.7031173508242, -62.1874917150412, 
                        -59.8437412911859, -54.3749903021901, -46.9531139599816, -52.0312398783348, 
                        -53.593740160905, -46.562488889339, -50.078114525122, -52.8124900196199, 
                        -55.9374905847603, -58.6718660792582, -61.0156165031135, -67.2656176333944, 
                        -60.2343663618284, -65.3124922801816), y = c(29.3895190143571, 
                        28.953472414703, 16.7441676243879, 28.953472414703, 23.2848666191995, 
                        25.9011462171242, 32.4418452119358, 34.1860316105523, 37.238357808131, 
                        42.4709170039804, 40.2906840057098, 42.9069636036345, 45.5232432015591, 
                        40.2906840057098, 40.2906840057098, 35.4941714095146, 28.953472414703
                )), .Names = c("x", "y"), row.names = c(NA, 17L), class = "data.frame")

P.hull <- chull(Pp) # najdi zunanje to�ke
P.xy <- Pp[c(P.hull, P.hull[1]), ]

# naredi to�ke in poligon iz zunanjih to�k (chull)
P <- SpatialPolygons(list(Polygons(list(Polygon(P.xy)), ID = 1))) # tega verjetno ne bomo nucal
pp <- SpatialPoints(Pp)
#pp.centr <- gCentroid(pp)
Pp.centr <- matrix(coordinates(P), nrow = 1)

P.xy
P.xy.max <- apply(P.xy, 1, FUN = function(x, ref = Pp.centr) {
            # v dist po�lje� dve to�ki, vsaka v svoji vrstici
            dist(rbind(matrix(x, nrow = 1), ref))
        })

#P.xy[2, 2] <- -50

# naredi home range na podlagi effect.distance.
# recimo, da je hr dvakrat ve�ji od najdalj�e razdalje znotraj to�k
hr <- 30
#delta <- hr - P.xy.max[which(P.xy.max == max(P.xy.max))]
# moramo uporabit delta/2
#home.range <- gBuffer(SpatialPoints(Pp.centr, proj4string = CRS(as.character(NA))), width = hr/2)
potential.centroid.loc <- gBuffer(SpatialPoints(Pp.centr, proj4string = CRS(as.character(NA))), width = hr)

plot(0, 0, type = "n", ylim = c(-100, 100), xlim = c(-100, 100), asp = 1)
plot(Q, add = TRUE) # sampling area
#plot(home.range, add = T)
plot(pp, add = T)
#points(P.xy[which(P.xy.max == max(P.xy.max)),], pch = 2)
points(Pp.centr)
plot(P, add = TRUE)
plot(potential.centroid.loc, add = TRUE, lty = "dashed", border = "lightgrey")

edge.pairs <- getEdgePairs(P)
edge.points <- getEdgePoints(P)

# Function closestEdgeCenter finds the position of all edge cases
# where a circle could still be placed so that all sampled points
# lie inside. Result is a smaller polygon, defined by centers of
# all edge cases mentioned in the first line of this comment.
min.of.possible.polys <- t(suppressWarnings(apply(edge.pairs,
                        MARGIN = 1, 
                        FUN = closestEdgeCenter,
                        pts = edge.points,
                        r = hr/2, ctr = Pp.centr)))
points(min.of.possible.polys, pch = "+")
apply(min.of.possible.polys[1:4, ], 1, FUN = function(x) {
            # Sys.sleep(2)
            plot(gBuffer(SpatialPoints(matrix(x, nrow = 1)), width = hr/2), add = TRUE, lty = "dotted", border = "lightgrey")
        })

# now that we have centers of possible border-line home.ranges,
# we populate that polygon with random points that will serve
# as centers for "all" possible home range positions
polygon.min.centroid <- closeRing(min.of.possible.polys)
polygon.min.centroid <- SpatialPolygons(list(Polygons(list(Polygon(polygon.min.centroid)), ID = 1)))
points.polygon.min.centroid <- spsample(polygon.min.centroid, n = 10, type = "random")

plot(edge.points, xlim = c(-120, 20), ylim = c(-20, 80), asp = 1)
#points(Pp.centr, col = "red")
plot(polygon.min.centroid, add = TRUE)
plot(points.polygon.min.centroid, add = TRUE)

library(snowfall)
sfInit(cpus = 4, parallel = TRUE, type = "SOCK")
sfLibrary(rgeos)
sfExport(list = c("Q", "hr", "polygon.min.centroid"))
#seq(from = 10000, to = 500000, by = 50000)
system.time(check.var2 <- sfSapply(1:500, simulateSP))

#plot(check.var2)
#abline(h = mean(check.var2, na.rm = TRUE))
#abline(h = mean(check.var[300:500]))


#plot(scale(check.var))
#points(scale(check.mean), col = "red")
#xy <- melt(data.frame(var = scale(check.var), mean = scale(check.mean)))
#xy$pos <- rep(1:length(check.var), ecah = 2)
#ggplot(xy, aes(x = pos, y = value, colour = variable)) + geom_point(shape = 1) + stat_smooth(method = loess, na.rm = TRUE)

#plot(scale(check.mean2)[-1:-5])
#points(scale(check.var2)[-1:-5])
#
#x <- check.mean2
#mean(xmean <- x[200:500])
#plot(xmean)
#abline(v = mean(xmean))
#abline(v = mean(xmean) + 1.96 * (sd(xmean)/sqrt(length(xmean))), lty = "dashed", col = "grey")
#abline(v = mean(xmean) - 1.96 * (sd(xmean)/sqrt(length(xmean))), lty = "dashed", col = "grey")
#
#dev.new()
#z <- rnorm(301, mean = mean(xmean), sd = sd(xmean))
#hist(z)
#roll.xmean <- zoo::rollapply(xmean, width = 20, FUN = mean)
#plot(roll.xmean)

####################################
#### simulacija za več primerov ####
####################################
library(rgeos)
library(ggplot2)
source("d:/workspace/trpd/workhorse.R")

hr.size <- 30
# naredimo vzorčno območje
sa <- gBuffer(SpatialPoints(matrix(c(0, 0), nrow = 1)), width = 100, quadsegs = 30)
#plot(sa, axes = TRUE, xlim = c(-150, 100))

# ki ga naselimo z gradientom walkerjev
hr <- sapply(seq(-120,-20, by = 10), FUN = function(x) {
            gBuffer(SpatialPoints(matrix(c(x, 0), nrow = 1)), width = hr.size, quadsegs = 30)
        })
#lapply(hr, FUN = plot, add = TRUE)

n <- 5 # number of points to be sampled
# Sample points in home range of each walker.
hr.sampled <- lapply(hr, FUN = function(x, sa, n) {
            pts <- spsample(x, n = n, type = "random")
#            points(pts)
            sa.fort <- fortify(sa)
            pts.fort <- coordinates(pts)
            inout <- point.in.polygon(point.x = pts.fort[, "x"], point.y = pts.fort[, "y"], pol.x = sa.fort[, "long"], pol.y = sa.fort[, "lat"])
            message(inout)
            
            if (all(!as.logical(inout))) return(NULL)
            
            pts.in <- pts.fort[as.logical(inout), , drop = FALSE]
            out <- SpatialPoints(pts.in)
            
#            Sys.sleep(1)
#            plot(sa, axes = TRUE, xlim = c(-150, 100))
#            points(pts.in, pch = 16)
#            points(pts)
#            points(out, pch = 16)
            
            out
        }, sa = sa, n = n)

# Find the area where centers of all potential home ranges could lie.
# Returns a (list of) SpatialPolygons object(s).
# hr.size = assumed home range size
hr.chull <- lapply(hr.sampled, getPotentialArea, hr.size = hr.size)

invisible(mapply(FUN = checkPlot, hr.sampled, hr.chull, MoreArgs = list(s = 1, sa = sa)))

# Sample points inside the polygon for potential home range centers
# and estimate contribution to sampling area.
library(snowfall)
sfInit(parallel = TRUE, cpus = 4, type = "SOCK")
sfLibrary(rgeos)
sfLibrary(sp)
sfExport(list = c("hr.size", "sa", "hr", "hr.sampled", "hr.chull", "estimateSP"))
# za vsak walker izračunaj varianco prispevnega območja z različnim številom točk v vzorčnem območju
conts <- sapply(1:length(hr.chull), 
        function(xy, hr.chull, hr.size, sa, hr.sampled, hr) {
            sapply(c(100, 200, 500, 1000, 5000), function(x) lapply(hr.chull[xy], estimateSP, hr.size = hr.size, sa = sa, n.size = x))
        }, 
        hr.chull = hr.chull, hr.size = hr.size, sa = sa, hr.sampled = hr.sampled, hr = hr, simplify = FALSE)

lapply(conts, FUN = function(x) {
            if (is.null(x[[1]])) return(NULL)
            rst <- data.frame(size = rep(c(100, 200, 500, 1000, 5000), times = c(100, 200, 500, 1000, 5000)),
                    data = unlist(x))
            print(ggplot(rst, aes(x = as.factor(size), y = data)) + geom_boxplot() +
                            ylim(0.9, 1))
            Sys.sleep(5)
            data.frame(size = c(100, 200, 500, 1000, 5000), mean = sapply(x, mean, simplify = TRUE), var = sapply(x, var, simplify = TRUE), sd = sapply(x, sd, simplify = TRUE))
        })

pdf("diagnostika.pdf", height = 10, width = 6)
lapply(conts, FUN = function(x) {
            if (is.null(x[[1]])) return(NULL)
            par(mfrow = c(5, 1))
            lapply(x, FUN = function(y) {
                        plot(1:(length(y)-1), zoo::rollapply(y, width = 2, FUN = var), type = "l")
                    })
        })
dev.off()
