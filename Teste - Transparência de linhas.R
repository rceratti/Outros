par(mfrow = c(1, 2))
plot(dist ~ speed, cars)

pr.df <- data.frame(speed = seq(5, 30, 1))
B <- 1e3
pr.mat <- matrix(NA, B, nrow(pr.df))

for(i in 1:B) {
  cars.b <- cars[sample.int(nrow(cars), rep = T), ]
  cars.lo <- loess(dist ~ speed, cars.b)
  pr.lo <- predict(cars.lo, pr.df)
  pr.mat[i, ] <- pr.lo
  lines(pr.df$speed, pr.lo, col = rgb(0, 0, 0, .2))
}

lines(pr.df$speed, apply(pr.mat, 2, median, na.rm = T), col = 2)


plot(pr.df$speed, predict(loess(dist ~ speed, cars), pr.df), ty = 'l')
lines(pr.df$speed, apply(pr.mat, 2, median, na.rm = T), col = 2)