# Dados fictícios
y1 <- 2; y2 <- 3
R <- 1.4 * matrix(c(1, .25, .25, 1), 2, 2)


# Monte Carlo
f.v0 <- function(y1, y2, u){
  prod(dpois(c(y1, y2), exp(.1 + u)))
}

library(MASS)
U <- mvrnorm(1e5, rep(0, 2), R)
mean(apply(U, 1, function(u) f.v0(y1, y2, u)))


# Aproximação de Laplace
f.v1 <- function(y1, y2, u, R){
  Rinv <- chol2inv(chol(R))
  q <- length(u)
  f.u <- ((2* pi)^q * det(R))^-(1/2) * exp(-.5 * t(u) %*% Rinv %*% u)
  f.v0(y1, y2, u) * as.vector(f.u)
}

log.f.v1 <- function(y1, y2, u, R) log(f.v1(y1, y2, u, R))

q <- 2
u.hat <- optim(rep(0, q), log.f.v1, method = "BFGS", hessian = T,
               y1 = y1, y2 = y2, R = R, control = list(fnscale = -1))

hess <- u.hat$hessian

(2*pi)^(q/2) * f.v1(y1, y2, u.hat$par, R) * abs(det(hess))^-.5


# Quadratura Gaussiana
f.v2 <- function(y1, y2, u, R) f.v1(y1, y2, u, R) * prod(exp(u^2/2)) * 2 * pi

library(SparseGrid)
pr.grid <- createProductRuleGrid('GQN', 2, 15, sym = F)
f.v2.nodes <- apply(pr.grid$nodes, 1, function(u) f.v2(y1, y2, u, R))
f.v2.nodes %*% pr.grid$weights


library(bayespack)
ghbp <- banint(log.f.v1, start = c(0,0), y1 = y1, y2 = y2, R = R, trans = 'none')
exp(ghbp$nrmcon[1])