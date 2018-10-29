library(raster)
library(rgeos)
library(cluster)
library(splancs)
library(foreach)
library(doParallel)

if (Sys.info()["sysname"] == "Windows") {
  ncores <- 4
} else {
  ncores <- 46
}

#############
# empirical #
#############
set.seed(357) # use seed for reproducibility of generating starting values
nsim <- 3000
sap <- 200

# Because radius and area are not linearly correlated (in a straight line), 
# we first calculate area, calculate area if we divide it by 1, 2,... 1000
ratio <- c(1000, 1)
H <- (pi * sap^2)/ratio
H <- seq(from = H[1], to = H[2], length.out = 10)
r <- round(sqrt(H/pi))

xy <- data.frame(SD = sample(r, size = nsim, replace = TRUE),
                 prob = runif(nsim, min = 0.1, max = 0.3),
                 num.walkers = sample(c(500, 800, 1000, 1300, 1500), size = nsim, replace = TRUE),
                 sessions = sample(c(5, 10, 15), size = nsim, replace = TRUE)
)

###############
# DIAGNOSTICS #
###############

# plot(gBuffer(SpatialPoints(matrix(c(0,0), nrow = 1)), width = max(r)))
# plot(gBuffer(SpatialPoints(matrix(c(0,0), nrow = 1)), width = min(r)), add = TRUE)

# ar <- min(r):max(r)
# plot(ar, y = scale(ar, center = FALSE), type = "l", ylim = c(0, 2.5))
# lines(ar, y = scale(pi * (ar)^2, center = FALSE), type = "l")

# table(xy$SD, xy$num.walkers, xy$sessions)

# SD=60, home range extends from 0 to about 200, see
# curve(dnorm(x, sd = 60), from = 0, to = 600)
# SD=20, home range extends from 0 to about 50
# curve(dnorm(x, sd = 20), from = 0, to = 600)

##################
# </DIAGNOSTICS> #
##################

xy$sap <- sap
xy$home.range <- xy$SD
xy$area <- 1000
xy$work.dir <- "data"
xy$seed <- 1:nrow(xy)
xy$rsln <- 1
xy$weight.switch <- TRUE
xy$num.boots <- 5000
xy <- xy[rep(1:nrow(xy), each = 2), ]
xy$sim.dist <- c("empirical", "normal")
xy$summary.file <- sprintf("simulation_list_%s.txt", xy$sim.dist)

cl <- makeCluster(ncores, outfile = "clusterfuck.txt")
registerDoParallel(cl)
on.exit(stopCluster(cl))

foreach(i = (1:nrow(xy))) %dopar% {
  library(raster)
  library(rgeos)
  library(cluster)
  library(splancs)
  
  source("simulation.R")
  source("walkerContribution.R")
  source("populateWorld.R")
  source("sampleWorld.R")
  source("sampleWalkers.R")
  source("numberOfBins.R")
  source("calculateContribution.R")
  source("calculateBins.R")
  source("weighDistances.R")
  source("distWeights.R")
  source("individualContribution.R")
  source("calcNormal2D.R")
  source("writeINP.R")
  
  out <- simulation(
    SD = xy$SD[i],
    prob = xy$prob[i],
    sessions = xy$sessions[i],
    num.walkers = xy$num.walkers[i],
    sap = xy$sap[i],
    area = xy$area[i],
    work.dir = xy$work.dir[i],
    seed = xy$seed[i],
    summary.file = xy$summary.file[i],
    home.range = xy$home.range[i],
    rsln = xy$rsln[i],
    weight.switch = xy$weight.switch[i],
    sim.dist = xy$sim.dist[i],
    num.boots = xy$num.boots[i]
  )
  
  if (any(class(out) %in% c("error", "warning"))) {
    message(out$message)
    ftw <- "./data/failed.errors.%s.txt"
    cat(out$message, file = sprintf(ftw, xy$sim.dist[i]), append = TRUE)
    cat(sprintf("\ndied in seed %s \n", xy$seed[i]), file = sprintf(ftw, xy$sim.dist[i]), append = TRUE)
  }
  out
}
