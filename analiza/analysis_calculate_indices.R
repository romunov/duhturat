# Chunk 1 load packages and scripts
library(parallel)
library(capwire)

source("markAnalysis.R")
source("readRunModels.R")
source("../getQs.R")
source("../calcNormal2D.R")
source("calculateIndices.R")

load("simulations.RData")
load("simulations_test.RData")

if (Sys.info()["sysname"] == "Windows") {
  ncores <- 8
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
xy <- parSapply(cl = cl, X = xy.mark, FUN = calculateIndices, lf = lf, simplify = FALSE)
# xy <- sapply(X = xy.mark, FUN = calculateIndices, lf = lf, simplify = FALSE) # for debugging
xy <- mapply(FUN = calculateIndices, x = xy.mark, lf = lf, SIMPLIFY = FALSE)
xy <- do.call(rbind, xy)
rownames(xy) <- NULL
save(xy, file = "simulations_calculated_indices.RData")
