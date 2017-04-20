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

set.seed(357)
xy <- data.frame(SD = rep(seq(from = 20, to = 50, by = 5), each = 10*4*6*5),
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
xy$rsln <- 0.5
xy$weight.switch <- TRUE
xy$sim.dist <- "empirical"
xy$num.boots <- 5000

# paralelno računanje
# cl <- makeCluster(20, type = "FORK")
# clusterEvalQ(cl = cl, expr = {
#   library(raster)
#   library(rgeos)
#   library(cluster)
#   library(splancs)
#   
#   source("simulation.R")
#   source("walkerContribution.R")
#   source("populateWorld.R")
#   source("sampleWorld.R")
#   source("sampleWalkers.R")
#   source("numberOfBins.R")
#   source("calcNormal2D.R")
#   source("calculateContribution.R")
#   source("calculateBins.R")
#   source("weighDistances.R")
#   source("distWeights.R")
#   source("individualContribution.R")
#   source("superPopulation.R")
#   source("writeINP.R")
# })

cl <- makeCluster(20)
registerDoParallel(cl)
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
    write.table(x = rdt, file = paste(rdt$work.dir, "/failed_attempts.txt", sep = ""), row.names = FALSE, col.names = FALSE,
                append = TRUE, sep = ";", fileEncoding = "UTF-8")
    cat(out$message, file = "./data/failed.errors.txt", append = TRUE)
  }
  out
}
