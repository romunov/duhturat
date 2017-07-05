load("simulations.RData")

xy.mark[[4]]$est.der.pars[, c("N.Population.Size.estimate", "model.name")]
diff(xy.mark[[4]]$est.der.pars$N.Population.Size.estimate)

out <- sapply(xy.mark, FUN = function(x) diff(x$est.der.pars$N.Population.Size.estimate))
hist(out[out < quantile(out, probs = 0.9)], breaks = 30)

getQnormal(probs = 0.68, SD = 1)
qnorm(p = 0.341+0.5, sd = 1)
curve(dnorm(x, sd = 1), from = 0, to = 3)
