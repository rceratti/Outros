library(geoRglm)
library(maptools)


base <- 'C:/Users/Rubem/Downloads/TM_WORLD_BORDERS_SIMPL-0.3'
setwd(base)



wsf <- readShapePoly("TM_WORLD_BORDERS_SIMPL-0.3.shp")
plot(wsf)


ger <- wsf@polygons[[72]]
ger1 <- ger@Polygons[[20]]@coords

l <- 60
x <- seq(bbox(ger)['x', 1], bbox(ger)['x', 2], l = l)
y <- seq(bbox(ger)['y', 1], bbox(ger)['y', 2], l = l)
locs <- as.matrix(expand.grid(x = x, y = y))

sim <- grf(grid = locs, borders = ger1, cov.model = "matern",
           cov.pars = c(.1, 2), kappa = .5)

image(sim)





sim2 <- grf(grid = locs, borders = ger1, cov.model = "matern",
            cov.pars = c(.5, 2), kappa = .5, nsim = 2)


nt <- 5e1
sts1 <- arima.sim(list(order = c(1,0,0), ar = .9), nt, sd = .4)
sts2 <- arima.sim(list(order = c(1,0,0), ar = .9), nt, sd = .4)
tF <- rbind(sts1, sts2)
tF <- apply(tF, 2, cumsum)

L <- sim2$data
L.tF <- L %*% tF

sim.hold <- sim2
sim.hold$data <- L.tF  # 1-exp(-exp(-.1 + sim$data))

pdf('gerMap.pdf')
for(i in 1:ncol(L.tF))
  image(sim.hold, i, zlim = range(L.tF), col = topo.colors(30))
dev.off()
