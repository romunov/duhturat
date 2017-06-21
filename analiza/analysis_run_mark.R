library(RMark)
library(parallel)

source("markAnalysis.R")
source("readRunModels.R")

if (Sys.info()["sysname"] == "Windows") {
  ncores <- 8
} else {
  ncores <- 46
}

cl <- makeCluster(ncores)
on.exit(stopCluster(cl))

clusterEvalQ(cl = cl, expr = source("readRunModels.R"))
clusterEvalQ(cl = cl, expr = library(RMark))

xy <- list.files("../data/", pattern = ".inp", full.names = TRUE)

system.time(xy.mark <- parSapply(cl = cl, X = xy, FUN = markAnalysis, simplify = FALSE))

save(xy.mark, file = "simulations.RData")
