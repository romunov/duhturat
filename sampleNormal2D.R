library(ggplot2)
library(sp)
library(mixtools)

set.seed(2)

N <- 5
SD <- 1

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