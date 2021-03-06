#' Simulation that runs functions to calculate contribution area.
#' Work horse, if you will.
#' @author Roman Luštrik

#' @param seed Integer. Seed number.
#' @param sap Radius of sampling area polygon. If radius, it a positive integer should be supplied. Can also be SpatialPolygon.
#' @param work.dir A directory where all the magic happens. If missing. \code{work.dir} is used.
#' @param	summary.file A file into which simulation results are to be written. Defaults to \code{simulation_list.txt}
#' @param home.range Radius (r) of the walker's home range. Amount by which sampling area will be padded.
#' @param area Numeric. By how much the sampling polygon will be enlarge. This is used to populate walkers around the sampling
#' area of interest and not "litter" the super world.
#' @param rsln Numeric. Grid resolution.
#' @param SD Standard deviation (square root of variance) for walker's walk from centroid.
#' @param prob Probability of capture, on the interval of \code{[0, 1]}.
#' @param weight.switch Logical. If TRUE, pairwise data is bootstrapped and a weighted and unweighted data.frame of bins are created.
#' @param sim.dist When simulating contribution, use (half) normal function to construct probability of occurrence. Possible
#' values are "empirical" or "normal".
#' Default is NULL, but accepted value is "normal".
#' @num.boots Integer. Number of bootstraps to be used for calculating CI of the walk function.

simulation <- function(
  sap,
  num.walkers,
  work.dir,
  seed = NULL,
  summary.file,
  home.range,
  prob,
  rsln,
  area,
  SD = NULL,
  sessions,
  num.boots,
  weight.switch,
  custom.walkers = NULL, # for debugging purposes
  sim.dist,
  ...
) {
  
  ## Make sure some of the input complies with the requirements.
  stopifnot(is.numeric(prob))
  stopifnot(is.numeric(home.range))
  stopifnot(is.numeric(seed) | is.null(seed))
  stopifnot(class(sap) == "SpatialPolygon" | class(sap) == "numeric")
  stopifnot(any(c("normal", "empirical") %in% sim.dist))
  stopifnot(!is.null(SD))
  stopifnot(!is.null(num.walkers))
  
  ## If work.dir missing, create it.
  if (is.na(file.info(work.dir)$isdir)) dir.create(work.dir)
  
  ## Each simulation run can have its own seed.
  if (!is.null(seed) & is.numeric(seed)) {
    message(sprintf("Settings seed to %d", seed))
    set.seed(seed) 
  }
  
  ## Create sampling area. This creates a circle of defined radius.
  if (is.numeric(sap)) {
    sap.poly <- gBuffer(SpatialPoints(matrix(c(0, 0), nrow = 1)),
                        width = sap, quadsegs = 50)
    message("Creating a circular sampling area polygon.")
  }
  
  start.date <- Sys.time() # record simulation start time
  message(paste("Simulation started at:", start.date))
  
  ## If summary.file is missing, create a default one.
  if (!file.exists(summary.file)) {
    summary.file <- "simulation_list.txt"
    message("Summary file missing. Creating a default one.")
  }
  
  ## Create a filename with a unique tag (date and time up to a minute).
  file.name <- paste("mark-", format(Sys.time(), "%Y-%m-%d-%H-%M"), ".txt", sep = "")
  
  ## Create an empty object of the world.
  object <- raster(nrow = area * 2, ncol = area * 2,
                   xmn = -area, xmx = area,
                   ymn = -area, ymx = area,
                   crs = NA) # no projection, can be extended to use one
  res(object) <- c(rsln, rsln)
  
  ## Construct a list of parameters to be passed on.
  pars <- list()
  pars$area <- area
  pars$rsln <- rsln
  pars$file.name <- file.name
  pars$start.date <- start.date
  pars$sap <- sap
  pars$work.dir <- work.dir
  pars$home.range <- home.range
  pars$prob <- prob
  pars$summary.file <- summary.file
  pars$seed <- seed
  pars$custom.walkers <- ifelse(!is.null(custom.walkers), TRUE, FALSE)
  pars$num.walker <- num.walkers
  pars$num.boots <- num.boots
  pars$sim.dist <- sim.dist
  pars$SD <- SD
  
  ## Ok, things are ready now. Time to generate walkers and calculate their
  ## contribution area.
  walk.data <- walkerContribution(num.walkers = num.walkers, sw = sw, area = area, seed = seed,
                                  home.range = home.range, sap.poly = sap.poly, prob = prob, work.dir = work.dir,
                                  sessions = sessions, weight.switch, .object = object, .num.boots = num.boots, 
                                  custom.walkers = custom.walkers, SD = SD, sim.dist = sim.dist, ...)
  
  
  writeINP(object = walk.data, pars = pars, seed = seed)
}
