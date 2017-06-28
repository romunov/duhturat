library(ggplot2)

xy <- data.frame(x = seq(0, 100, by = 0.1))
xy$y <- dnorm(xy$x, sd = 20)

ggplot(xy, aes(x = x, y = y)) +
  theme_bw() +
  ylab("Gostota") +
  xlab("Oddaljenost od centroida") +
  geom_line() +
  geom_ribbon(data = xy[xy$y > dnorm(0.8413447, sd = 20), ], aes(ymax = y), ymin = 0, fill = "red", alpha = 0.5)


curve(dnorm(x, sd = 1), from = 0, to = 4)
abline(h = dnorm(pnorm(1)))

pnorm(1)
qnorm(0.8413447)
dnorm()