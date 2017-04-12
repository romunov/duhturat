SD <- 10
xmin <- -400
xmax <- 400
sap <- 200
cnt <- 180

plot(0, 0, type = "n", xlim = c(xmin, xmax), ylim = c(0, 0.05))
curve(dnorm(x, mean = cnt, sd = SD), from = xmin, to = xmax, add = TRUE)
abline(v = c(-sap, sap), col = "red")
