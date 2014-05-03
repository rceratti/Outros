library(deSolve)



SIR <- function(tau, vals, parms) {
  nu <- parms['nu']
  beta <- parms['beta']

  S <- vals['S']
  I <- vals['I']

  dS <- - beta * I * S 
  dI <- beta * I * S - nu * I
  dR <- nu * I

  list(c(dS, dI, dR))
}


vals <- c(S = 499, I = 1, R = 0)

h <- .1
times <- seq(0, 100, h)

out <- lsoda(vals, times, SIR, parms = c(nu = 4e-1, beta = 1e-3))
plot(out)



