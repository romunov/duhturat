library(raster)
library(rgeos)
library(cluster)
library(splancs) #csr
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
nsim <- 2000
xy <- data.frame(SD = round(runif(nsim, min = 20, max = 60)),
                 prob = runif(nsim, min = 0.15, max = 0.4),
                 num.walkers = sample(c(1000, 2000, 3000, 4000, 5000), size = nsim, replace = TRUE),
                 # 0.005014205 0.006367245 0.007959056 0.011142679 0.015042616 # densities range [╩0.005, 0.015] walkers/unit, 
                 # 3 fold increase for sap 200 and area 400
                 # if not enough walkers, sampling fails. why 3-fold? because a 3 fold increase in a population could be considered a lot
                 sessions = sample(4:7, size = nsim, replace = TRUE)
)

# SD=60, home range extends from 0 to about 200, see
# curve(dnorm(x, sd = 60), from = 0, to = 600)
# SD=20, home range extends from 0 to about 50
# curve(dnorm(x, sd = 20), from = 0, to = 600)

xy$sap <- 200
xy$home.range <- xy$SD
xy$area <- 700
xy$work.dir <- "data"
xy$seed <- 1:nrow(xy)
xy$sim.dist <- "empirical"
xy$summary.file <- sprintf("simulation_list_%s.txt", xy$sim.dist)
xy$rsln <- 0.5
xy$weight.switch <- TRUE
xy$num.boots <- 5000

cl <- makeCluster(ncores)
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
  source("superPopulation.R")
  source("writeINP.R")
  source("calcNormal2D.R")

  out <- tryCatch({
    simulation(
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
  },
  error = function(e) e,
  warning = function(w) w)

  if (any(class(out) %in% c("error", "warning"))) {
    message(out$message)
    cat(out$message, file = sprintf("./data/failed.errors.%s.txt", xy$sim.dist[i]), append = TRUE)
  }
  out
}

##########
# normal #
##########

# all parameters remain the same, fitted distribution changes
xy$sim.dist <- "normal"

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
  source("superPopulation.R")
  source("writeINP.R")
  source("calcNormal2D.R")
  
  out <- tryCatch({
    simulation(
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
  }, 
  error = function(e) e,
  warning = function(w) w)
  
  if (any(class(out) %in% c("error", "warning"))) {
    message(out$message)
    cat(out$message, file = "./data/failed.errors.txt", append = TRUE)
  }
  out
}
