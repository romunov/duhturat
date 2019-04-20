library(ggplot2)
library(sp)
library(mixtools)

set.seed(2)

N <- 1000
SD <- 2

xy <- data.frame(x = runif(N, min = -5, max = 5),
                 y = runif(N, min = -5, max = 5))

SS <- 5000
ss <- data.frame(x = rnorm(SS, mean = xy$x, sd = SD),
                 y = rnorm(SS, mean = xy$y, sd = SD)
)


ggplot(ss, aes(x = x, y = y)) +
  theme_bw() +
  geom_density2d(h = 1.5) +
  geom_point(alpha = 0.1) +
  geom_point(data = xy, aes(x = x, y = y))

#####

# koliko točk je znotraj 1 SD?
N <- 10000
set.seed(357)
xy <- data.frame(x = rnorm(n = N, mean = 0, sd = 1),
                 y = rnorm(n = N, mean = 0, sd = 1))

# SD ni preprosto SD_x ali SD_y, ampak SD_x + SD_y + 2*korelacija(x,y) * cov_x * cov_y
# https://www.probabilitycourse.com/chapter5/5_3_2_bivariate_normal_dist.php
radius <- sqrt(sum(diag(var(xy))) + (2 * cor(xy$x, xy$y) * sd(xy$x) * sd(xy$y)))

theta <- seq (0, 2 * pi, length = 1000)
krog <- data.frame(x = 0 + radius * cos(theta),
                   y = 0 + radius * sin(theta))

sp.krog <- SpatialPolygons(list(Polygons(list(Polygon(krog)), ID = 1)))

sp.xy <- xy
coordinates(sp.xy) <- ~ x + y

plot(xy, asp = 1)
plot(sp.krog, lwd = 2, border = "red", add = TRUE, col = NA)

pins <- over(x = sp.xy, y = sp.krog)
sum(pins, na.rm = TRUE)/N
pins[is.na(pins)] <- FALSE

points(sp.xy[pins == 1, ], col = "green")

# poskus s tole kodo iz mixtools::ellipse:
# http://stats.stackexchange.com/a/36023/144
# https://stats.stackexchange.com/a/127486/144
# plot(sp.xy, asp = 1)
# plot(sp.krog, lwd = 2, border = "red", add = TRUE, col = NA)

# https://en.wikipedia.org/wiki/Multivariate_normal_distribution
es <- eigen(cov(xy))
e1 <- es$vec %*% diag(sqrt(es$val))
r1 <- sqrt(qchisq(0.68, 2)) # 1 SD
theta <- seq(0, 2 * pi, len = 2000)
v1 <- cbind(r1 * cos(theta), r1 * sin(theta))
pts <- t(colMeans(xy) - (e1 %*% t(v1)))
colnames(pts) <- c("x", "y")
sp.krog2 <- SpatialPolygons(list(Polygons(list(Polygon(pts)), ID = 1)))

plot(xy, asp = 1, axes = TRUE)
lines(pts, col = "blue", lwd = 2)
abline(v = range(pts[, "x"]), col = "red")
abline(h = range(pts[, "y"]), col = "red")

pins <- over(x = sp.xy, y = sp.krog2)
sum(pins, na.rm = TRUE)/N


#########################
# prikaz weibullove CDF #
#########################
x <- seq(0, 40, by = 0.1)

plot(x, 1 - pweibull(q = x, shape = 8, scale = 20), type = "l", ylab = "", 
     main = "KPV Weibullove porazdelitve \n z različnimi vrednostmi parametra a")
lines(x, 1 - pweibull(q = x, shape = 2, scale = 20), col = "red")
lines(x, 1 - pweibull(q = x, shape = 4, scale = 20), col = "green")
lines(x, 1 - pweibull(q = x, shape = 6, scale = 20), col = "blue")

xy <- sapply(seq(2, 8, by = 2), FUN = function(s, x) {
  data.frame(wb = 1 - pweibull(q = x, shape = s, scale = 20), 
             s = s, x = x)
}, x = x, simplify = FALSE)

xy <- do.call(rbind, xy)
xy$s <- as.factor(xy$s)

ggplot(xy, aes(x = x, y = wb, color = s)) +
  theme_bw() +
  geom_line() +
  xlab("") + ylab("") +
  scale_color_brewer(palette = "Set1", name = "parameter: a")

ggsave("./figures/weibull_platfon.pdf", width = 5, height = 5)


source("calcNormal2D.R")
# TODO: na podlagi ocenjenih parametrov simuliraj naključne vrednosti in izračunaj kvantile
curve(weibullLikeDistribution(x, sigma = 163, b = -2, mx = 11), from = 0, to = 500)

# Rejection sampling
# https://theoreticalecology.wordpress.com/2015/04/22/a-simple-explanation-of-rejection-sampling-in-r/

weibullLikeDistribution <- function(x, sigma, b, mx) {
  exp(-(x/sigma)^(-b)) * mx
}

set.seed(357)
xrange <- 300 # from 0 (implicit) to x
N <- 50000 # number of samples

xy <- data.frame(proposed = runif(N, min = 0, max = xrange))
b <- -2.16
mx <- 35.48
sigma <-  147.17
xy$fit <- weibullLikeDistribution(x = xy$proposed, sigma = sigma, b = b, mx = mx)
xy$fit <- (xy$fit - min(xy$fit))/(max(xy$fit) - min(xy$fit))
xy$random <- runif(N, min = 0, max = 1)

maxDens <- max(xy$fit)

xy$accepted <- with(xy, random <= fit/maxDens)
xy.out <- xy[xy$accepted, ] # retain only those values that are "below" the custom distribution

hist(xy.out$proposed, freq = FALSE, breaks = 100, col = "light grey")
curve(weibullLikeDistribution(x, sigma = sigma, b = b, mx = mx)/4500, # divide to make it look fit nicely
      from = 0, to = 300, add = TRUE, col = "red", lwd = 2)

abline(v = quantile(xy.out$proposed, probs = c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)),
       lwd = 2)


library(ggplot2)

xys <- xy[order(xy$proposed), ]
xys <- xys[seq(1, nrow(xys), by = 11), ]

ggplot(xy, aes(x = proposed, y = fit/maxDens)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  geom_line(alpha = 0.5) +
  geom_point(data = xys, aes(y = random, color = accepted), alpha = 0.5) +
  geom_point(data = xys, aes(x = proposed, y = fit/maxDens), alpha = 0.5) +
  geom_segment(data = xys, aes(x = proposed, y = random, xend = proposed, yend = fit/maxDens), alpha = 0.3)

getQcustom(fun = weibullLikeDistribution, sigma = 147, mx = 35, b = -2, N = 50000,
           xrange = list(0, 400))

getQnormal(mu = 0, sd = 1)
qnorm(c(0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99), mean = 0, sd = 1)
