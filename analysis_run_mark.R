library(RMark)

xy <- list.files("../data/", pattern = ".inp", full.names = TRUE)

if (!dir.exists("mark_intermediate")) {
  dir.create("mark_intermediate")
}
# analyse simulations using MARK, didn't manage to get it working in parallel
xy.mark <- sapply(xy, FUN = markAnalysis,
                  wd.model = "./mark_intermediate/", simplify = FALSE)

save(xy.mark, file = "simulations.RData")