library(geoRglm)
library(tweedie)


x <- seq(0, 2, l = 40)
locs <- as.matrix(expand.grid(x = x, y = x))
u <- grf(grid = locs, cov.pars = c(.1, 0.5), kappa = 1.5, method = "RF")
plot(u)
image(u, col = cm.colors(12))
summary(u$data)


tes <- u
tes$data <- exp(tes$data)
image(tes, col = cm.colors(12))


well <- c(1.2, 1.2)
points(well[1], well[2], pch = 19, col = 4)
r <- spDistsN1(locs, well)
plot(r, tes$data)


mu <- exp(3 + u$data + -2*r)  #
summary(mu)
y <- rtweedie(nrow(locs), 1.2, mu, .1)

plot(r, y)

dat <- u
dat$data <- y
image(dat, col = cm.colors(40))
points(well[1], well[2], pch = 19, col = 4)