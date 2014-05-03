base <- "~/Rubem_Ceratti/Outros"
setwd(base)

# Base de dados
cancer <- read.table("Cancer.txt", h = T)

# Dados para o modelo
y <- as.integer(cancer$VAR2 == "B")

x <- cancer[, paste0('VAR', 3:ncol(cancer))]
x.trans <- log(x + 1)
X <- model.matrix(~., x.trans)

J <- ncol(X)

mon.names <- "LP"
parm.names <- as.parm.names(list(beta = rep(0, J)))
PGF <- function(data) rnormv(data$J, 0, 10)
mod.data <- list(J = J, PGF = PGF, X = X, mon.names = mon.names,
                 parm.names = parm.names, y = y)


# Log-posteriori
model <- function(parm, data){
  # Parameters
  beta <- parm[1:data$J]

  # Log of Prior Densities
  beta.prior <- sum(dnormv(beta, 0, 1000, log = TRUE))

  # Log-Likelihood
  mu <- plogis(data$X %*% beta)
  LL <- sum(dbinom(data$y, 1, mu, log = TRUE))

  # Log-Posterior
  LP <- LL + beta.prior

  Modelout <- list(LP = LP, Dev = -2*LL, Monitor = LP,
                   yhat = rbinom(length(mu), 1, mu), parm = parm)
  return(Modelout)
}


init <- GIV(model, mod.data, PGF = TRUE)


# Reversible Jump MCMC
bin.n <- J-1
bin.p <- 0.5
parm.p <- c(1, rep(1/(J-1), (J-1)))
selectable <- c(0, rep(1, J-1))

m0 <- LaplacesDemon(model, Data = mod.data, init,
                    Covar = NULL, Iterations = 1e4, Status=1000, Thinning = 1,
                    Algorithm = "RJ", Specs = list(bin.n = bin.n, bin.p = bin.p,
                    parm.p = parm.p, selectable = selectable,
                    selected = c(0,rep(1,J-1))))

m0
PosteriorChecks(m0)

burnin <- m0$Rec.BurnIn.Thinned
plot(m0, burnin, mod.data, PDF = FALSE)


# Adaptative Metropolis
m1 <- LaplacesDemon(model, Data = mod.data, init,
                    Covar = NULL, Iterations = 1e5, Status=1000, Thinning = 100,
                    Algorithm = "AM", Specs = list(Adaptive=500, Periodicity=10))


burnin <- m1$Rec.BurnIn.Thinned
plot(m1, burnin, mod.data, PDF = FALSE)


B <- m1$Posterior2  # Posteriori final ('thinned' e sem amostras de 'burn-in')
pred <- plogis(B %*% t(X))
pred <- as.vector(colMeans(pred))

table(y, pred > .5)

result <- data.frame(ID = cancer$VAR1, y = y, pred = pred)
result[y != (pred > .5), ]