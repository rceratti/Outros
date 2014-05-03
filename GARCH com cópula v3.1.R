library(tseries)
library(fGarch)
library(copula)
library(reshape2)


# Obtenção dos dados
acao <- c("BBDC3", "BBDC4", "BRFS3", "BVMF3", "CIEL3", 
          "ITSA4", "ITUB4", "VALE3", "VALE5", "NATU3", "CRUZ3")
acao <- paste0(acao, ".SA")
inicio <- '2007-01-01'
fim <- Sys.Date()


dat <- lapply(acao, function(x) {
  get.hist.quote(x, inicio, fim, quote = 'Close')
})
names(dat) <- acao



op <- par(mfrow = c(3, 5))

# Gráfico das séries
mapply(plot, x = dat, ylab = acao)


# Gráfico dos logaritmos dos retornos
dat1 <- lapply(dat, function(x) as.vector(x$Close))
dat1.1 <- lapply(dat1, function(x) diff(log(x)))
sapply(dat1.1, plot, type = 'l')

par(op)


# Ajuste de modelos Garch(1, 1) com distribuiçao condicional t de student
cond.dist <- 'sstd'

m0 <- lapply(dat1.1, garchFit, formula = ~ garch(1, 1), cond.dist = cond.dist,
             trace = F, algorithm = "nlminb+nm")


# Residuos padronizados dos modelos GARCH ajustados
pModDist <- function(x, model) {
  dist <- model@fit$params$cond.dist
  parms <- coef(model)
  switch(dist, 
         'sstd' = psstd(x, 0, 1, parms['shape'], parms['skew']),
         'std' = pstd(x, 0, 1, parms['shape']),
         'ged' = pged(x, 0, 1, parms['shape']))
}

resFun <- function(model.list, data.list) {
  resids <- lapply(model.list, residuals, standardize = TRUE)

  resids <- mapply(function(x, y) {
    n <- nrow(y)
    zoo::zoo(as.matrix(x), time(y)[2:n])
  }, x = resids, y = data.list, SIMPLIFY = F)

  resids.1 <- do.call(merge, resids)
  resids.2 <- na.omit(resids.1)

  pRes <- matrix(0, nrow(resids.2), ncol(resids.2))
  for(j in 1:ncol(pRes)) {
    tmpRes <- as.vector(resids.2[, j])
    pRes[, j] <- pModDist(tmpRes, model.list[[j]])
  }
  
  colnames(pRes) <- names(resids.2)
  pRes
}

pRes <- resFun(m0, dat)


# Ajuste da copula t para os resíduos padronizados
ini <- rep(.1, ncol(pRes)*(ncol(pRes) - 1)/2)
tcop <- tCopula(ini, dim = ncol(pRes), dispstr = "un", df = 20)
m1 <- fitCopula(tcop, pRes, method = "ml")


# Simulaçao de observaçoes a partir da cópula estimada
qModDist <- function(x, model) {
  dist <- model@fit$params$cond.dist
  parms <- coef(model)
  switch(dist, 
         'sstd' = qsstd(x, 0, 1, parms['shape'], parms['skew']),
         'std' = qstd(x, 0, 1, parms['shape']),
         'ged' = qged(x, 0, 1, parms['shape']))
}


simRetFun <- function(copula.fit, model.list, n.ahead, B) {
  # Simulaçao de observaçoes a partir da cópula estimada
  coefCop <- coef(copula.fit)
  coefs <- coefCop[-length(coefCop)]
  dimCop <- length(model.list)
  n <- B * n.ahead
  newObs <- rCopula(n, tCopula(coefs, dimCop, "un", coefCop['df']))
  dim(newObs) <- c(B, n.ahead, ncol(newObs))

  # Previsão de n passos à frente da volatilidade
  pred <- lapply(model.list, predict, n.ahead = n.ahead, plot = F)

  # Simulaçao da distribuiçao dos log-retornos n passos à frente
  newObs.1 <- matrix(0, B, dimCop)
  for(k in 1:dimCop) {
    pVol <- pred[[k]]
    p1 <- as.matrix(qModDist(newObs[,, k], m0[[k]]))
    p2 <- p1 %*% diag(pVol$meanError, nrow(pVol), nrow(pVol))
    I <- matrix(1, B, n.ahead)
    p3 <- I %*% diag(pVol$meanForecast, nrow(pVol), nrow(pVol))
    p3 <- p3 + p2
    newObs.1[, k] <- rowSums(p3)
  }
  newObs.1
}


n.ahead <- 1
B <- 2e5
newObs.1 <- simRetFun(m1, m0, n.ahead, B)


# Otimização dos pesos em relação ao VaR ou ES
loss <- function(w, x, alpha, val, ES = TRUE) {
  if(any(w < 0))
    return(1e5)
  rets <- as.vector(x %*% w)/sum(w)
  valRets <- val * (exp(rets) - 1)
  VaR <- quantile(valRets, alpha)
  # Se ES, Expected Shortfall, c.c. Value-at-Risk
  ifelse(ES, -mean(valRets[valRets < VaR]), -VaR)  
}

l <- ncol(newObs.1)
w <- rep(1, l)
flagES <- FALSE
val <- 173405.29
w.op <- optim(w, loss, x = newObs.1, alpha = .01, val = val, ES = flagES)

w <- w.op$par
loss(w, newObs.1, .01, val, flagES)
rets <- as.vector(newObs.1 %*% w)/sum(w)
hist(exp(rets), fr = F, br = 30, col = rgb(0, 0, 1, 0.5))
w/sum(w)


tes0 <- data.frame(acao, monies = val*w/sum(w))

tes <- read.table('clipboard', sep = '\t')
tes <- tes[, c('V1', 'V6')]
tes$V6 <- as.numeric(gsub('[,.]', '', tes$V6))/100

tes$V1 <- paste0(tes$V1, '.SA')

tes1 <- merge(tes0, tes, by.x = 'acao', by.y = 'V1', all = T)
tes1[is.na(tes1)] <- 0
tes1$var <- with(tes1, monies - V6)
tes1[, c('acao', 'var')]