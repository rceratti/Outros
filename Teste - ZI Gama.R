library(numDeriv)
library(doParallel)


llzigamma <- function(th, y, X, z) {
  x <- X
  alpha <- exp(th[1])
  beta <- th[2:(ncol(x) + 1)]
  zeta <- th[(ncol(x) + 2):(ncol(x) + 1 + ncol(z))]

  mu <- exp(x %*% beta)
  psi <- plogis(z %*% zeta)

  ind <- y == 0

  ll1 <- log(psi[ind])
  ll2 <- log(1 - psi[!ind]) + dgamma(y[!ind], alpha, alpha/mu[!ind], log = T)
  -(sum(ll1) + sum(ll2))
}


glm.zig <- function(formula, formula.zi, data = NULL, start = NULL, ...) {
  if(is.null(data))
    data <- parent.frame()

  mf.zi <- model.frame(formula.zi, data = data)
  z <- model.matrix(attr(mf.zi, "terms"), data = mf.zi)

  mf <- model.frame(formula, data = data)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)

  if(is.null(start))
    start <- rnorm(ncol(x) + 1 + ncol(z))
  
  fit <- nlminb(start, llzigamma, y = y, X = x, z = z)
  th <- fit$par
  h <- hessian(llzigamma, th, y = y, X = x, z = z)

  result <- list(coefficients = th, cov = solve(h))
  class(result) <- 'zig'
  result
}


vcov.zig <- function(object) object$cov

myconfint <- function(object, level = .95) {
  cf <- coef(object)
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  ep <- sqrt(diag(vcov(object)))
  lapply(1:length(cf), function(i) c(cf[i] + qnorm(a)*ep[i], cf[i]))
}


simulate.zig <- function(n = 1e3, alpha = 1, beta = c(1, -1), zeta = c(-.85, .2)) {
  x <- rnorm(n, sd = 2)
  mu <- exp(model.matrix(~ x) %*% beta)
  y <- rgamma(n, alpha, alpha/mu)

  psi <- plogis(model.matrix(~ x) %*% zeta)
  i <- apply(psi, 1, function(x) sample(0:1, 1, T, c(x, 1 - x)))
  y <- i * y

  list(data = data.frame(x = x, y = y), parms = c(alpha, beta, zeta))
}



cl <- makeCluster(3)
registerDoParallel(cl)
clusterEvalQ(cl, library(numDeriv))
clusterExport(cl, c('llzigamma', 'glm.zig', 'vcov.zig',
                    'myconfint', 'simulate.zig'))
snow::clusterSetupRNGstream(cl, seed = 3245636)

R <- 1e4
sim <- foreach(i = 1:R) %dopar% {
  m0 <- glm.zig(y ~ x, ~ x, simulate.zig()$data)
  myconfint(m0)
}

stopCluster(cl)


sim.1 <- lapply(1:length(sim[[1]]), function(i) t(sapply(sim, '[[', i)))
parms <- simulate.zig(1)$parms
parms[1] <- log(parms[1])

# Taxa de cobertura
mapply(function(x, y) mean((x[,1] < y) & (y < x[,2])),
       x = sim.1, y = parms)
# Resultado: [1] 0.9506 0.9493 0.9521 0.9494 0.9519


# Boxplots dos valores estimados menos valores dos parâmetros
par(mfrow = c(2, 3))
mapply(function(x, y) boxplot(x[,3] - y), sim.1, parms)

# Amplitudes dos intervalos de confiança
par(mfrow = c(2, 3))
lapply(sim.1, function(x) boxplot(x[,2] - x[,1]))