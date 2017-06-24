library(RMark)
library(capwire)
library(ggplot2)

set.seed(357)
# simulate data
p <- 0.3 # true detection rate
K <- 5 # number of sessions
lw <- 30 # line width in plot

calculateMarkFast <- function(x) {
  
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  tmp <- tempdir()
  on.exit(unlink(tmp), add = TRUE)
  setwd(tmp)
  
  # Run MARK analysis using the Huggins model.
  tr.process <- process.data(x, model = "Huggins")
  tr.ddl <- make.design.data(tr.process)
  
  # https://sites.google.com/site/workshoponcmr/home/sche/10-closed-capture/huggins
  p.cequal.dot <- list(formula = ~ 1, share = TRUE)
  
  # collect models and run
  cml <- create.model.list("Huggins")
  tr.result <- mark.wrapper(cml, data = tr.process, ddl = tr.ddl, adjust = FALSE,
                            invisible = TRUE)
  setwd(oldwd)
  tr.result
}

#' @param p Numeric. Detection probability.
#' @param K Integer. Number of sessions.
#' @param N.fudge Integer. How many walkers to "fudge" - take out some capture and simulate heterogeneity.
#' @param rm.1 Integer. From \code{N.fudge}, how many 1s to remove.
rs <- sapply(rep(seq(200, 1000, by = 100), each = 5), FUN = function(N, p, K, N.fudge, rm.1) {
  
  # Simulate some fine and dandy CMR data.
  xy <- replicate(N, {
    rbinom(K, size = 1, prob = p)
  }, simplify = FALSE)
  xy <- do.call(rbind, xy)
  count <- apply(xy, MARGIN = 1, FUN = sum)
  xy <- data.frame(ch = apply(xy, MARGIN = 1, paste, collapse = ""), stringsAsFactors = FALSE)
  xy$count <- count
  xy <- xy[count > 0, ]
  
  # Estimate population sizes using Huggins' model and  TIRM.
  tr.result <- calculateMarkFast(x = xy)
  mdl <- fitTirm(data = buildClassTable(xy$count), max.pop = N * 3)
  
  out <- list()
  out$huggins <- data.frame(N = N,
                            pop.size = tr.result$p.cequal.dot$results$derived$`N Population Size`$estimate,
                            se = tr.result$p.cequal.dot$results$derived$`N Population Size`$se,
                            p.est = tr.result$p.cequal.dot$results$real$estimate,
                            p.est.se = tr.result$p.cequal.dot$results$real$se,
                            model = "huggins", method = "closureOK")
  out$tirm <- data.frame(N = N,
                         pop.size = mdl$ml.pop.size, 
                         se = NA,
                         p.est = NA,
                         p.est.se = NA,
                         model = "tirm", method = "closureOK")
  
  # decrease p due to edge effect
  l.fudge <- (nrow(xy) - N.fudge + 1):nrow(xy)
  
  fudged <- do.call(rbind, strsplit(xy[l.fudge, "ch"], ""))
  fudged[sample(which(fudged == 1), rm.1, replace = FALSE)] <- 0
  fudged.count <- apply(fudged, MARGIN = 1, FUN = function(x) sum(as.numeric(x)))
  fudged <- data.frame(ch = apply(fudged, MARGIN = 1, FUN = paste, collapse = ""), 
                       count = fudged.count, stringsAsFactors = FALSE)
  xy[l.fudge, ] <- fudged
  xy <- xy[xy$count > 0, ]
  
  # Estimate population sizes using Huggins' model and  TIRM for fudged data.
  tr.result <- calculateMarkFast(x = xy)
  mdl <- fitTirm(data = buildClassTable(xy$count), max.pop = N * 3)
  
  out$huggins.fudge <- data.frame(N = N,
                                  pop.size = tr.result$p.cequal.dot$results$derived$`N Population Size`$estimate,
                                  se = tr.result$p.cequal.dot$results$derived$`N Population Size`$se,
                                  p.est = tr.result$p.cequal.dot$results$real$estimate,
                                  p.est.se = tr.result$p.cequal.dot$results$real$se,
                                  model = "huggins", method = "closureViolated")
  out$tirm.fudge <- data.frame(N = N,
                               pop.size = mdl$ml.pop.size, 
                               se = NA,
                               p.est = NA,
                               p.est.se = NA,
                               model = "tirm", method = "closureViolated")
  
  out <- as.data.frame(do.call(rbind, out))
  out
}, p = p, K = K, N.fudge = 100, rm.1 = 50, simplify = FALSE)

rs <- do.call(rbind, rs)

ggplot(rs, aes(x = N, y = pop.size, color = method, shape = model)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_segment(aes(x = N-lw, y = N, xend = N+lw, yend = N), size = 1, alpha = 0.75) +
  geom_jitter()
