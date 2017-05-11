library(raster)
library(rgeos)
library(cluster)
library(splancs) #csr
library(RMark)
library(foreach)
library(doParallel)

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

set.seed(357)
xy <- data.frame(SD = rep(seq(from = 50, to = 100, by = 5), each = 10*4*6*5),
                 prob = rep(seq(from = 0.15, to = 0.4, by = 0.05), each = 4),
                 num.walkers = c(100, 200, 400, 800, 1000),
                 sessions = 4:7
)

xy$sap <- 200
xy$area <- 600
xy$work.dir <- "data"
xy$seed <- 1:nrow(xy)
xy$summary.file <- "simulation_list.txt"
xy$home.range <- sqrt(qchisq(0.68, xy$SD))
xy$rsln <- 2
# xy$rsln <- 0.5
xy$weight.switch <- TRUE
xy$sim.dist <- "normal"
xy$num.boots <- 5000
xy["sim.dist"] <- "empirical"

i <- sample(1:nrow(xy), size = 1, replace = FALSE)

rdt <- data.frame(
  SD = as.numeric(xy[i, "SD"]),
  prob = as.numeric(xy[i, "prob"]),
  sessions = as.numeric(xy[i, "sessions"]),
  num.walkers = as.numeric(xy[i, "num.walkers"]),
  sap = as.numeric(xy[i, "sap"]),
  area = as.numeric(xy[i, "area"]),
  work.dir = xy[i, "work.dir"],
  seed = as.numeric(xy[i, "seed"]),
  summary.file = xy[i, "summary.file"],
  home.range = as.numeric(xy[i, "home.range"]),
  rsln = as.numeric(xy[i, "rsln"]),
  weight.switch = as.logical(xy[i, "weight.switch"]),
  sim.dist = xy[i, "sim.dist"],
  num.boots = as.numeric(xy[i, "num.boots"]),
  comment = "not passed",
  stringsAsFactors = FALSE
)

# out <- tryCatch({
  simulation(
    SD = rdt$SD,
    prob = rdt$prob,
    sessions = rdt$sessions,
    num.walkers = rdt$num.walkers,
    sap = rdt$sap,
    area = rdt$area,
    work.dir = rdt$work.dir,
    seed = rdt$seed,
    summary.file = rdt$summary.file,
    home.range = rdt$home.range,
    rsln = rdt$rsln,
    weight.switch = rdt$weight.switch,
    sim.dist = rdt$sim.dist,
    num.boots = rdt$num.boots
  )
# }, 
# error = function(e) e,
# warning = function(w) w)

# paralelna verzija
cl <- makeCluster(4)
registerDoParallel(cl)


foreach(i = 1:5) %dopar% {
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
}

# setwd("..")
out <- markAnalysis(fn = "./data/mark-2017-05-01-14-32_1417.inp", wd = "./data")
