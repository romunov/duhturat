library(ggplot2)

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
