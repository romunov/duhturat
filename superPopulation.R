#' Calculate contribution area of the sampling area based on walk distance
#' accumulation curve.
#' 
#' Cells that are outside the reach, but inside the sampling area, of the
#' accumulation curve, are not calculated and are assumed to be one. Cells in
#' the rim around the border sampling area are found and each cell's
#' contribution is found.
#' Cell calculations are done on multiple cores using snowfall package.
#' 
#' @param sap.poly SpatialPolygons. Polygon of the sampling area.
#' @param effect.distance Numeric. Distance of the maximum walk.
#' @param ring.weigts data.frame/list. A list of data.frames with columns for
#' lower, mean and upper bounds of the accumulation distance curve.
#' @param .object RasterLayer. An empty RasterLayer object of the sampling area
#' with some padding.
#' @param ... Arguments passed to \code{snowfall::sfInit}, e.g. \code{cpus}
#' and \code{type}.
#' 
#' @author Roman Luštrik

superPopulation <- function(.object, sap.poly, effect.distance, ring.weights, ...) {
  
  ## Store resolution of the raster.
  rsln <- max(res(.object)) # choose max
  
  ## Populate cells with cell numbers.
  rst.cellpos <- .object
  rst.cellpos[] <- 1:ncell(.object)
  
  ## We first need to find cells that "contribute" walkers to the
  ## sampling area.
  out.rim <- gBuffer(spgeom = sap.poly, width = effect.distance, quadsegs = 100)
  out.rim <- rasterize(x = out.rim, y = .object)
  
  # If home range (and effectively effect distance) is bigger than the sampling
  # area, it doesn't make sense to find the edge. In that case, use all cells
  # inside the sampling area.
  # If effect distance is equal or less than r of the sampling area, use
  # in.rim and out.rim when calculating which cells may have their contribution
  # less than 1.
  # Otherwise just assume all walkers need to have their contribution calculated
  # because we can't assume that their contribution to the sampling area is 1.
  if (effect.distance <= extent(sap.poly)@xmax) {
    in.rim <- gBuffer(spgeom = sap.poly, width = -effect.distance, quadsegs = 100)
    in.rim <- rasterize(x = in.rim, y = .object)
    ## how many cells with contribution "1" are there in the in.rum?
    infill <- as.numeric(table(in.rim[])) 
    
    ## in.rim should be set to NA, since these are the cells that we've already
    ## calculate their contribution, which is "1".
    in.rim[which(is.na(in.rim[]))] <- 2 # 2 is a value that should not appear
    in.rim[in.rim[] == 1] <- NA # set in.rim to NA
    ## these are the cells that we need to compute their contribution
    rim <- raster::mask(x = out.rim, mask = in.rim) 
    rim.cells <- which(rim[] == 1)
    rm(in.rim)
    rm(rim)
  } else {
    rim.cells <- which(out.rim[] == 1)
    infill <- 0
    rm(out.rim)
  }
  
  ## Find coordinates of cells of interest.
  xy <- xyFromCell(object = .object, cell = rim.cells)
  xy <- cbind(cell = rim.cells, xy)
  
  
  ## Performance is improved if we are using a vector of cell numbers as a mask
  ## over a raster mask.
  rst.dist.mask <- rasterize(x = sap.poly, y = .object)
  numerical.mask <- which(rst.dist.mask[] == 1)
  
  if (!sfParallel())	sfInit(parallel = TRUE, ...) 
  
  message(paste("Started calculating super population on: ", start.time <- Sys.time()))
  
  ## Calculate contribution for each cell.
  get.world <- lapply(X = ring.weights, FUN = function(
      w,
      efd = effect.distance,
      ras.mas = numerical.mask,
      obj = rst.cellpos,
      em.obj = .object,
      .xy = xy
    ) {
      
#      iterate.over <- c("lower", "mean", "upper") 
      iterate.over <- c("mean") 
      
      sapply(iterate.over, function(k,
          ief = efd,
          irm = ras.mas,
          ring.w = w,
          iobj = obj,
          ieo = em.obj,
          ..xy = .xy
        ) {
          
          ring.curve <- ring.w[, k]
          
          sfLibrary(raster)
          sfExport(list = c("iobj", "ief", "calcDistanceNumeric", "ieo", "irm", "ring.curve"),
            local = TRUE)
          
          cont.vector <- sfApply(x = xy, margin = 1, fun = function(
#					cont.vector <- apply(X = ..xy[1:1000, ], MARGIN = 1, FUN = function(
              xy,
              .effect.distance = ief,
              object = iobj,
              empty.object = ieo,
              .ring.weights = ring.curve,
              raster.mask = irm) {
              
              ## raster must not be a raster with NAs or pointDistance will crap out?
              ## xy	a matrix of two columns (coordinates of called cells)
              ## raster	a raster with 1:ncell(object) values
              ## effect.distance	buffer around a walker/polygon
              ## mask	a RasterLayer object used to mask out the calculated layer
              ## ring.weights	a result of ringWeights() function
              x <- xy[c("x", "y")]
              out <- calcDistanceNumeric(origin.point = x, object = iobj,
                effect.distance = .effect.distance, ring.weights = .ring.weights,
                empty.object = empty.object, raster.mask = raster.mask)
              
              return(sum(out, na.rm = TRUE))
            })
          
          # A data.frame with columns cell number, x, y and contribution value.
          # Zeros are pruned because they didn't contribute anything to the
          # sampling area.
          dv <- data.frame(xy, cont = cont.vector)
          
          # Make a subset vector of values that are not zero
          dv.zero <- dv[dv$cont != 0, ]
          #dv.zero.length <- length(distance.vector[distance.vector != 0])
          
          #	# Calculate the contribution area of the super population
          # To get the correct density based on super population, you need to divide
          # number of walkers in super population with the value of distance.vector,
          # with denominator (distance.vector) multiplied by the area of the cell.
          # So for example, if the rsln (resolution) is 4, you have multiply the 
          # contribution size with 16. You will get approximate density calculated
          # by dividing number of generated walkers with the area of the "world".
          area.supop <- (sum(cont.vector, na.rm = TRUE) + infill) * rsln^2 #rsln^2 is area of each cell
          # Calculate contribution area of the sampling area.
          find.xy <- which(rst.dist.mask[] == 1)
          area.sample <- dv.zero[dv.zero[, "cell"] %in% find.xy, ]
          area.sample <- sum(area.sample$cont, na.rm = TRUE) * rsln^2
          # add values that are always 1
          
          out <- c(supop = area.supop, sample = area.sample)
          
          return(out)
        })
    })
  
  message(paste("Calculation of super population took", Sys.time() - start.time))
  return(get.world)
}