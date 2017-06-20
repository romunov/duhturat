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
xy <- data.frame(SD = round(runif(nsim, min = 400, max = 1600)),
                 prob = runif(nsim, min = 0.15, max = 0.4),
                 num.walkers = sample(c(630, 800, 1000, 1400, 1890), size = nsim, replace = TRUE),
                 # 0.005014205 0.006367245 0.007959056 0.011142679 0.015042616 # densities range [╩0.005, 0.015] walkers/unit, 
                 # 3 fold increase for sap 200 and area 400
                 # if not enough walkers, sampling fails. why 3-fold? because a 3 fold increase in a population could be considered a lot
                 sessions = sample(4:7, size = nsim, replace = TRUE)
)

xy$sap <- 200
xy$area <- 400
xy$work.dir <- "data"
xy$seed <- 1:nrow(xy)
xy$sim.dist <- "empirical"
xy$summary.file <- sprintf("simulation_list_%s.txt", xy$sim.dist)
xy$home.range <- sqrt(qchisq(0.68, xy$SD))
# mogoče zaradi tega ne dela simulacija, ker sta SD in home.range različno velika?
# glej sqrt(qchisq(0.68, seq(400, 1600, by = 50)))
xy$SD <- xy$home.range 
xy$rsln <- 0.5
xy$weight.switch <- TRUE
xy$num.boots <- 5000

cl <- makeCluster(ncores)
registerDoParallel(cl)
on.exit(stopCluster(cl))

foreach(i = 1:nrow(xy)) %dopar% {
  library(raster)
  library(rgeos)
  library(cluster)
  library(splancs) #csr
  
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
  source("stopWatch.R")
  source("writeINP.R")
  source("calcNormal2D.R")
  # funkcije za analizo
  source("extractMarkResults.R")
  source("markAnalysis.R")
  source("readRunModels.R")
  
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

foreach(i = 1:nrow(xy)) %dopar% {
  library(raster)
  library(rgeos)
  library(cluster)
  library(splancs) #csr
  
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
  source("stopWatch.R")
  source("writeINP.R")
  source("calcNormal2D.R")
  # funkcije za analizo
  source("extractMarkResults.R")
  source("markAnalysis.R")
  source("readRunModels.R")
  
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