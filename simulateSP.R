simulateSP <- function(mm) {
    
    points.polygon.min.centroid <- spsample(polygon.min.centroid, n = mm, type = "random")
    check <- apply(as.data.frame(points.polygon.min.centroid), 1, function(x, sa = Q) {
                x <- SpatialPoints(matrix(x, nrow = 1))
                #FIXME: tale hr ni importan, pa ga išče zunaj
                plg <- gBuffer(x, width = hr/2, quadsegs = 30)
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

#            Sys.sleep(1)
#            plot(0, 0, type = "n", ylim = c(-100, 100), xlim = c(-100, 100), asp = 1)
#            plot(pp, add = T)
#            plot(x, pch = 1, add = TRUE)
#            plot(plg, add = TRUE)
#            browser()
#            plot(Q,add = T)
                #diagnostics to see if all poinst fall inside circle
#            gCovers(spgeom1 = plg, spgeom2 = pp) 
                sa.plg.difference
            })
    check
}