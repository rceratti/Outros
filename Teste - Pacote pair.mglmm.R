library(testpack)
library(powell)

# Simulated data
phi <- 1; p <- 1.6
mydat <- data.sim(3, 'CP', exp, xi=p, phi=phi)
dat <- mydat$Data

mainForm0 <- value~-1+variable:period+(-1+variable|ID)
mainForm1 <- value~-1+variable+(-1+variable|ID)

# Pairwise models
cl<-makeCluster(4)
registerDoParallel(cl)
clusterEvalQ(cl, library(testpack))

system.time(m0.1 <- mglmmCP(mainForm0, dat$variable, dat))
system.time(m1.1 <- mglmmCP(mainForm1, dat$variable, dat))

stopCluster(cl)


summary(m0.1)
ranef(m0.1)
logLik(m0.1)
resid(m0.1)
fitted(m0.1)

summary(m1.1)
ranef(m1.1)
logLik(m1.1)
resid(m1.1)
fitted(m1.1)


system.time(m0 <- cpglmm(mainForm0, data=dat))
summary(m0)