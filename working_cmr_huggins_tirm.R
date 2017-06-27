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
#' @param N.fudge Numeric. Proportion of N to introduce heterogeneity to.
#' @param rm.1 Numeric. Proportion of \code{N.fudge} to remove.
rs <- sapply(rep(c(50, 100, 200, 500), each = 5), FUN = function(N, p, K, N.fudge = 1, rm.1) {
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
  mark.estimate <- tr.result$p.cequal.dot$results$derived$`N Population Size`$estimate
  mark.se <- tr.result$p.cequal.dot$results$derived$`N Population Size`$se
  mark.p <- tr.result$p.cequal.dot$results$real$estimate
  mark.p.se <- tr.result$p.cequal.dot$results$real$se
  
  mdl <- fitTirm(data = buildClassTable(xy$count), max.pop = N * 3)
  mdl.ci <- bootstrapCapwire(mdl, bootstraps = 300)

  out <- list()
  out$huggins <- data.frame(N = N,
                            pop.size = mark.estimate,
                            ci.low = mark.estimate - 1.96 * mark.se,
                            ci.high = mark.estimate + 1.96 * mark.se,
                            p.est = mark.p,
                            p.est.se = mark.p.se,
                            model = "huggins", method = "closureOK")
  out$tirm <- data.frame(N = N,
                         pop.size = mdl$ml.pop.size, 
                         ci.low = mdl.ci$conf.int[1],
                         ci.high = mdl.ci$conf.int[2],
                         p.est = NA,
                         p.est.se = NA,
                         model = "tirm", method = "closureOK")
  
  # If you want to fudge the data (introduce heterogeneity due to violation of closure), specify
  # N.fudge > 0.
  if (N.fudge < 0) {
    # decrease p due to edge effect
    l.fudge <- sample(1:nrow(xy), size = round(nrow(xy) * N.fudge), replace = FALSE)
    
    fudged <- do.call(rbind, strsplit(xy[l.fudge, "ch"], ""))
    fudged[sample(which(fudged == 1), round(nrow(xy) * N.fudge * rm.1), replace = FALSE)] <- 0
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
  }
  
  out <- as.data.frame(do.call(rbind, out))
  out
}, p = p, K = K, N.fudge = 0, rm.1 = 0.5, simplify = FALSE)

rs <- do.call(rbind, rs)
rs$id <- rep(1:length(unique(rs$N)), each = 5)
rs$id <- 1:nrow(rs)

ggplot(rs, aes(x = id, y = pop.size, color = model)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(palette = "Set1") +
  ggtitle(sprintf("Simulated p = %s, shown with 95%% CI", p)) +
  ylab("Simulated/estimated population size") +
  xlab("Simulation number") +
  scale_y_continuous(breaks = c(50, 100, 200, 500)) +
  geom_pointrange(aes(ymin = ci.low, ymax = ci.high))

ggsave("./figures/primerjava huggins tirm.png")
