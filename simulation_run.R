library(parallel)
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
source("calcDistance.R")
source("calcDistanceNumeric.R")
source("superPopulation.R")
source("stopWatch.R")
source("writeINP.R")
# funkcije za analizo
source("extractMarkResults.R")
source("markAnalysis.R")
source("readRunModels.R")
source("calcNormal2D.R")

set.seed(357)
xy <- data.frame(SD = rep(seq(from = 20, to = 50, by = 5), each = 10*4*6*5),
                 prob = rep(seq(from = 0.15, to = 0.4, by = 0.05), each = 4),
                 num.walkers = c(100, 200, 400, 800, 1000),
                 sessions = 4:7
)

xy$sap <- 200
xy$area <- 600
xy$work.dir <- "./data"
xy$seed <- 1:nrow(xy)
xy$summary.file <- "simulation_list.txt"
xy$home.range <- sqrt(qchisq(0.68, xy$SD))
xy$rsln <- 0.5
xy$weight.switch <- TRUE
xy$sim.dist <- "normal"
xy$num.boots <- 5000

# paralelno računanje
cl <- makeCluster(3)
clusterEvalQ(cl = cl, expr = {
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
  source("calcDistance.R")
  source("calcDistanceNumeric.R")
  source("superPopulation.R")
  source("stopWatch.R")
  source("writeINP.R")
})

result <- parApply(cl = cl, X = xy[1:5, ], MARGIN = 1, FUN = function(x) {
  
  rdt <- data.frame(
    SD = as.numeric(x["SD"]),
    prob = as.numeric(x["prob"]),
    sessions = as.numeric(x["sessions"]),
    num.walkers = as.numeric(x["num.walkers"]),
    sap = as.numeric(x["sap"]),
    area = as.numeric(x["area"]),
    work.dir = x["work.dir"],
    seed = as.numeric(x["seed"]),
    summary.file = x["summary.file"],
    home.range = as.numeric(x["home.range"]),
    rsln = as.numeric(x["rsln"]),
    weight.switch = as.logical(x["weight.switch"]),
    sim.dist = x["sim.dist"],
    num.boots = as.numeric(x["num.boots"]),
    comment = "not passed",
    stringsAsFactors = FALSE
  )
  
  out <- tryCatch({
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
  }, 
  error = function(e) e,
  warning = function(w) w)
  
  if (any(class(out) %in% "error") | any(class(out) %in% "warning")) {
    cat(out, file = "oow.txt")
    write.table(x = rdt, file = paste(rdt$work.dir, "/failed_attempts.txt", sep = ""), row.names = FALSE, col.names = FALSE,
                append = TRUE, sep = ";", fileEncoding = "UTF-8")
  }
  out
})
