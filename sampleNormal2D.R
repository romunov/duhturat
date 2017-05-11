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
krog <- data.frame(x = radius * cos(theta),
                   y = radius * sin(theta))

sp.krog <- SpatialPolygons(list(Polygons(list(Polygon(krog)), ID = 1)))

sp.xy <- xy
coordinates(sp.xy) <- ~ x + y

plot(sp.xy, asp = 1)
plot(sp.krog, lwd = 2, border = "red", add = TRUE, col = NA)

pins <- over(x = sp.xy, y = sp.krog)
sum(pins, na.rm = TRUE)/N
pins[is.na(pins)] <- FALSE

points(sp.xy[pins == 1, ], col = "green")

# poskus s tole kodo iz mixtools::ellipse:
# http://stats.stackexchange.com/a/36023/144
# plot(sp.xy, asp = 1)
# plot(sp.krog, lwd = 2, border = "red", add = TRUE, col = NA)

es <- eigen(cov(xy))
e1 <- es$vec %*% diag(sqrt(es$val))
r1 <- sqrt(qchisq(0.68, 2)) # 1 SD
theta <- seq(0, 2 * pi, len = 2000)
v1 <- cbind(r1 * cos(theta), r1 * sin(theta))
pts <- t(colMeans(xy) - (e1 %*% t(v1)))
colnames(pts) <- c("x", "y")
sp.krog2 <- SpatialPolygons(list(Polygons(list(Polygon(pts)), ID = 1)))

# plot(sp.xy, asp = 1)
lines(pts, col = "blue", lwd = 2)

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
# TODO: naredi rejection sampling
# https://stats.stackexchange.com/questions/88697/sample-from-a-custom-continuous-distribution-in-r

# tale ne dela dobro, ker nima mx parametra
inverseCDFWeibull <- function(x, sigma, b) {
  (x * ((x/sigma)^(-b))^(1/b) * pgamma(q = x, shape = (-1/b), rate = (x/b)^(-b)))/b
}
curve(CDFWeibullStandard(x, sigma = 113, b = 4), from = 0, to = 400)
curve(CDFWeibull(x, sigma = 113, b = -4, mx = 12), from = 0, to = 400)

abline(v = inverseCDFWeibull(0.5, sigma = 163, b = -2))

inverseCDFWeibull <- function(x, sigma, b) {
  sigma * pgamma(x, (-1/b), (x/b)^(-b))
}

x <- seq(0, 400, by = 0.1)
y <- customDistribution(x, sigma = 163, mx = 11, b = -2)
curve(customDistribution(x, sigma = 163, mx = 11, b = -2), from = 0, to = 400,
      ylab = "")

inverse <- function (f, lower = -100, upper = 100, ...) {
  function (y) uniroot((function (x) f(x, ...) - y), lower = lower, upper = upper)[1]
}

CDI <- inverse(customDistribution, lower = 0, upper = 500, sigma = 163, mx = 11, b = -2)
CDI(50)
