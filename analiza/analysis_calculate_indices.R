# Chunk 1 load packages and scripts
library(parallel)

source("markAnalysis.R")
source("readRunModels.R")
source("../getQs.R")
source("../calcNormal2D.R")
source("calculateIndices.R")

load("simulations.RData")
xy <- xy.mark

if (Sys.info()["sysname"] == "Windows") {
  ncores <- 4
} else {
  ncores <- 46
}

cl <- makeCluster(ncores)
on.exit(stopCluster(cl))

clusterEvalQ(cl, source("../calcNormal2D.R"))
clusterEvalQ(cl, source("../getQs.R"))
clusterEvalQ(cl, source("readRunModels.R"))
clusterEvalQ(cl, library(capwire))

lf <- list.files("../data/", pattern = ".inp", full.names = TRUE)
aee <- parSapply(cl = cl, X = xy, FUN = calculateIndices, lf = lf, simplify = FALSE)
# aee <- sapply(X = xy, FUN = calculateIndices, lf = lf, simplify = FALSE)
aee <- do.call(rbind, aee)
rownames(aee) <- NULL
save(aee, file = "simulations_calculated_indices.RData")