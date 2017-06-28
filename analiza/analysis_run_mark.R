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
on.exit(close.connection(cl))

clusterExport(cl = cl, varlist = c("extractPars", "readMark"))
clusterEvalQ(cl = cl, expr = library(RMark))

xy <- list.files("../data/", pattern = ".inp", full.names = TRUE)

system.time(xy.mark <- parSapply(cl = cl, X = xy, FUN = markAnalysis, simplify = FALSE))
# system.time(xy.mark <- sapply(X = xy, FUN = markAnalysis, simplify = FALSE))

save(xy.mark, file = "simulations.RData")
