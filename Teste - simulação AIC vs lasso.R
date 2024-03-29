library(MASS)
library(glmnet)
library(ROCR)
library(doParallel)
library(reshape2)
library(ggplot2)



# ::: Fun��es auxiliares ::: #

# Fun��o de gera��o dos dados a partir dos par�metros fixados
simDataFun <- function(n, ppart, beta) {
  b.len <- length(beta)-1                # N.o de var�aveis (s/ intercepto)
  x <- rnorm(n * b.len,, 2)              # Gera��o das var's explicativas
  x <- matrix(x, n, b.len)               # Formato de matrix
  colnames(x) <- paste0('V', 1:b.len)    # Nomes das vari�veis
  x <- as.data.frame(x)                  # Formato data.frame

  X <- model.matrix(~., x)               # Matriz de delineamento
  mu <- as.vector(plogis(X %*% beta))    # Vetor de valores esperados
  y <- rbinom(n, 1, mu)                  # Vetor da vari�vel resposta
  
  obs.data <- data.frame(y = y, x)       # Dados observados total
  n.train <- floor(n * ppart)            # N�mero de observa��es de treinamento
  tr.index <- sample(1:n, n.train)       # �ndice de sela��o para treinamento
  train <- obs.data[tr.index, ]          # Dados de treinamento
  test <- obs.data[-tr.index, ]          # Dados de teste

  list(train = train, test = test)
}


# Fun��es modificadas de estima��o e predi��o para cv.glmnet
cv.glmnet1 <- function(formula, family, data, ...) {
  mf <- model.frame(formula, data = data)
  x <- model.matrix(attr(mf, "terms"), data = mf)[, -1]
  y <- model.response(mf)

  cv.fit <- cv.glmnet(x, y, family = family, ...)
  cv.fit$formula <- formula
  class(cv.fit) <- c('glmnet1', class(cv.fit))
  cv.fit
}


predict.glmnet1 <- function(object, newdata, type) {
  mf <- model.frame(object$formula, data = newdata)
  x <- model.matrix(attr(mf, "terms"), data = mf)[, -1]
  class(object) <- class(object)[-1]
  predict(object, newx = x, "lambda.min", type = 'response')[, 1]
}


# Fun��o para estima��o e sele��o de par�metros
simEstFun <- function(formula, family, data) {
  m.glm <- glm(formula, family, data)           # Modelo GLM
  m.aic <- stepAIC(m.glm, trace = F)            # GLM com sele��o via AIC
  m.net <- cv.glmnet1(formula, family, data)    # Modelo lasso
  list(glm = m.glm, aic = m.aic, net = m.net)   # Lista final de modelos
}


# Fun��o para avalia��o de performance dos modelos estimados
simPredFun0 <- function(model, newdata, type, response) {
  score <- predict(model, newdata = newdata, type = type)  # Valores preditos

   if('matrix' %in% class(score))
    score <- score[, 2]                              # Preditos da classe positiva

  pred <- prediction(score, newdata[, response])     # Objeto de preid��o ROCR
  p1 <- performance(pred, "tpr", "fpr")              # Curva tpr vs fpr
  ks <- max(p1@y.values[[1]] - p1@x.values[[1]])     # Estat�stica KS
  acc <- mean((score >= .5) == newdata[, response])  # Acur�cia
  auc <- performance(pred, "auc")                    # �rea sob a curva ROC

  c(ks = ks, acc = acc, auc = auc@y.values[[1]])     # Combina��o dos resultados
}


simPredFun <- function(object, types, data, response) {
  preds <- mapply(simPredFun0, model = object, type = types, SIMPLIFY = FALSE,
         MoreArgs = list(newdata = data, response = response))
  preds <- do.call(rbind, preds)            # Medidas de performance dos modelos
  rownames(preds) <- NULL
  data.frame(Model = names(object), preds)  # Medidas em formato data.frame
}


# Fun��o que gera 1 simula��o do processo
simRun <- function(n, ppart, beta) {
  simData <- simDataFun(n, ppart, beta)
  simEst <- simEstFun(y ~ ., 'binomial', simData$train)
  simPredFun(simEst, rep('response', 3), simData$test, 'y')
}



# ::: Par�metros da simula��o ::: #

n <- 5e3        # N�mero de observa��es total
ppart <- .7     # Propor��o do particionamento de treinamento
p <- 5          # N�mero de par�metros != 0
p.add <- 10     # N�mero de par�metros == 0

beta <- rep(0, p + p.add)              # Vetor de par�metros inicial
b.index <- sample(1:(p + p.add), p)    # V�ri�veis com beta_j != 0
beta[b.index] <- rnorm(p,, 2)          # Valor dos par�metros != 0



# ::: Rodando a simula��o ::: #

cl <- makeCluster(3)
registerDoParallel(cl)
clusterEvalQ(cl, {library(MASS);
                  library(glmnet);
                  library(ROCR)})
clusterExport(cl, c('simDataFun',
                    'cv.glmnet1',
                    'predict.glmnet1',
                    'simEstFun',
                    'simPredFun0',
                    'simPredFun'))

B <- 1e3  # N�mero de simula��es

system.time({
simResults <- foreach(i = 1:B, .combine = rbind) %dopar% {
  suppressWarnings(simRun(n, ppart, beta))
}
})

stopCluster(cl)



# ::: Visualiza��o dos resultados ::: #

simResults.1 <- melt(simResults, id.vars = 'Model')

g <- ggplot(simResults.1, aes(Model, value, fill = Model)) + 
     geom_boxplot() + facet_grid(. ~ variable) + theme_bw()
g