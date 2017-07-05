# Chunk 1 load packages and scripts
library(capwire)
library(parallel)

source("markAnalysis.R")
source("readRunModels.R")
source("../getQs.R")
source("../calcNormal2D.R")
source("calculateIndices.R")

if (Sys.info()["sysname"] == "Windows") {
  ncores <- 4
  cl <- makeCluster(ncores, outfile = "clusterfuck.txt")
} else {
  ncores <- 46
  cl <- makeCluster(ncores, type = "FORK", outfile = "clusterfuck.txt")
}


on.exit(closeCluster(cl))

load("simulations.RData")

clusterEvalQ(cl, source("../calcNormal2D.R"))
clusterEvalQ(cl, source("../getQs.R"))
clusterEvalQ(cl, source("readRunModels.R"))
clusterEvalQ(cl, library(capwire))

lf <- list.files("../data/", pattern = ".inp", full.names = TRUE)
system.time(xy <- clusterMap(cl = cl, fun = calculateIndices, x = xy.mark, lf = lf, SIMPLIFY = FALSE))
# mapply(FUN = calculateIndices, x = xy.mark, lf = lf, SIMPLIFY = FALSE)
xy <- do.call(rbind, xy)
rownames(xy) <- NULL

save(xy, file = "simulations_calculated_indices.RData")
