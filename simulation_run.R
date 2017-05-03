library(parallel)
library(raster)
library(rgeos)
library(cluster)
library(splancs) #csr
library(foreach)
library(doParallel)

# source("simulation.R")
# source("walkerContribution.R")
# source("populateWorld.R")
# source("sampleWorld.R")
# source("sampleWalkers.R")
# source("numberOfBins.R")
# source("calculateContribution.R")
# source("calculateBins.R")
# source("weighDistances.R")
# source("distWeights.R")
# source("individualContribution.R")
# source("writeINP.R")
# # funkcije za analizo
# source("extractMarkResults.R")
# source("markAnalysis.R")
# source("readRunModels.R")
# source("calcNormal2D.R")

# set.seed(357) # for empirical walkers
set.seed(9) # for normal walkers
xy <- data.frame(SD = rep(seq(from = 20, to = 50, by = 5), each = 10*4*6*5),
                 prob = rep(seq(from = 0.15, to = 0.4, by = 0.05), each = 4),
                 num.walkers = c(100, 200, 400, 800, 1000),
                 sessions = 4:7
)

xy$sap <- 200
xy$area <- 600
xy$work.dir <- "data"
# xy$seed <- 1:nrow(xy) # empirical
xy$seed <- (1:nrow(xy)) + 8400 # normal
xy$summary.file <- "simulation_list.txt"
xy$home.range <- sqrt(qchisq(0.68, xy$SD))
xy$rsln <- 0.5
xy$weight.switch <- TRUE
xy$sim.dist <- "empirical"
xy$num.boots <- 5000

cl <- makeCluster(20)
registerDoParallel(cl)

# exclude already processed runs
fls <- list.files("./data", pattern = ".inp")
nms <- as.numeric(gsub("^(.*_)(\\d+).inp$", "\\2", fls))
message(sprintf("Skipping %d simulations.", length(nms)))

# if there are any simulations to skip, do so
if (length(nms) != 0) {
  sim.seq <- (1:nrow(xy))[-nms]
} else {
  sim.seq <- 1:nrow(xy)
}

foreach(i = sim.seq) %dopar% {
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
