load("simulations.RData")
library(tidyr)
library(dplyr)
library(ggplot2)

out <- lapply(xy.mark, FUN = function(x) {
  data.frame(p.1 = x$real.fun.pars$estimate[1],
             p.sp = x$real.fun.pars$estimate[2],
             size.1 = x$est.der.pars$N.Population.Size.estimate[1],
             size.sp = x$est.der.pars$N.Population.Size.estimate[2],
             num.generated.walkers = x$simulation.pars$num_of_walkers_supop,
             sessions = x$simulation.pars$sessions,
             hr = x$simulation.pars$home_range,
             fun = x$simulation.pars$sim.dist,
             area.naive = pi * x$simulation.pars$sampling_area_r^2,
             true.p = x$simulation.pars$capture_prob,
             sap = x$simulation.pars$sampling_area_r)
})

out <- do.call(rbind, out)
rownames(out) <- NULL
out

intmd <- out[out$fun == "empirical", ]
# first examine p
xep <- intmd[, c("true.p", "p.1", "p.sp", "num.generated.walkers", "hr",
                 "sessions", "area.naive")]
xep <- gather(xep, key = p.var, value = p.val, starts_with("p."))
xep <- xep[xep$p.val > 0, ] # include only p values with sensible estimate above 0
xep$hr.sap.ratio <- with(xep, hr/sqrt(xep$area.naive/pi))
xep$p.ratio <- xep$p.val/xep$true.p

ggplot(xep, aes(x = hr.sap.ratio, y = p.ratio)) +
  theme_bw() +
  geom_jitter(size = 0.5, alpha = 0.1, height = 0) +
  geom_smooth(method = "loess") +
  facet_grid(num.generated.walkers ~ sessions)