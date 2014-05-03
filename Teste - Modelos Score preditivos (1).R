library(ROCR)
library(caret)
library(glmnet)
library(randomForest)
library(MCMCpack)
library(VGAM)
library(rpart)
library(rpart.plot)
library(rattle)



# ::: Carregamento dos dados originais ::: #
#dat <- DAAG::dengue
#dat <- na.omit(dat)

Url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data'
a <- read.table(Url)
a$V21 <- a$V21 - 1
a$V21 <- factor(a$V21)
names(a)[ncol(a)] <- 'NoYes'

dat <- a



# ::: Divisão dos dados originais em treinamento e teste ::: #

# Número de observações e divisão em ~50% dos dados
nr <- nrow(dat)
n.sample <- floor(.5 * nr)

# Separação dos dados
set.seed(12527457)
train.index <- sample(seq_len(nr), n.sample)
train <- dat[train.index, ]
test <- dat[-train.index, ]



# ::: Ajuste de modelos candidatos ::: #

# Modelo logístico 'ingênuo'
m.glm <- glm(NoYes ~ ., binomial, train)
summary(m.glm)


# Partição com Random Forest
m.rf <- randomForest(NoYes ~ ., train, ntree = 1e3)
varImpPlot(m.rf)


# CART (árvore de decisão)
m.tree <- rpart(NoYes ~ ., train, method = "class") #, control=rpart.control(cp=0)
printcp(m.tree)
plotcp(m.tree)
m.tree$variable.importance
fancyRpartPlot(m.tree)


# Modelo glm com lasso
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

m.net <- cv.glmnet1(NoYes ~ ., 'binomial', train, type.measure = 'class')



# ::: Comparação entre os modelos ::: #

perfFun0 <- function(model, newdata, type, response, cfp, cfn) {
  score <- predict(model, newdata = newdata, type = type)

  if('matrix' %in% class(score))
    score <- score[, 2]

  pred <- prediction(score, newdata[, response])
  perf1 <- performance(pred, "tpr", "fpr")
  perf2 <- performance(pred, "acc")
  perf3 <- performance(pred, "cost", cost.fp = cfp, cost.fn = cfn)

  list(score = score, perf1 = perf1, perf2 = perf2, perf3 = perf3)
}

perfFun <- function(mod.list, mod.types, data, response, cfp = 1, cfn = 1) {
  mapply(perfFun0, model = mod.list, type = mod.types, SIMPLIFY = FALSE,
         MoreArgs = list(newdata = data, response = response, cfp = cfp, cfn = cfn))
}

perfPlot <- function(perf.list) {
  perf <- lapply(2:4, function(i) lapply(perf.list, '[[', i))
  par(mfrow = c(2, 2))
  sapply(perf, function(x) {
    l <- length(x)
    ysup <- max(sapply(x, function(z) max(z@y.values[[1]])))
    mapply(plot, x, col = 1:l, lty = 1:l, add = c(F, rep(T, l-1)),
           MoreArgs = list(ylim = c(0, ysup)))
  })
  plot.new()
  l <- length(perf[[1]])
  legend("topright", names(perf[[1]]), lty = 1:l, col = 1:l)
}

perfScore <- function(perf.list) {
  lapply(perf.list, '[[', 1)
}


mod.list <- list(glm = m.glm, rf = m.rf, tree = m.tree, net = m.net)
mod.type <- c('response', 'prob', 'prob', 'response')
perf.list <- perfFun(mod.list, mod.type, test, 'NoYes', cfp = 1, cfn = 1)

# Gráficos ROC, Acurácia e de custo por erro de classificação
perfPlot(perf.list)

# Medida KS de cada modelo
rocr <- lapply(perf.list, '[[', 2)
ks <- sapply(rocr, function(x) max(x@y.values[[1]] - x@x.values[[1]]))
ks

# Cut-off com base em custos no erro de classificação
cost <- lapply(perf.list, '[[', 4)
co <- lapply(cost, function(x) x@x.values[[1]][which.min(x@y.values[[1]])])

scores <- perfScore(perf.list)

confusionMatrix(as.integer(scores$net > co$net), test$NoYes, '1')
confusionMatrix(as.integer(scores$rf > co$rf), test$NoYes, '1')



# ::: Teste com validação cruzada ::: #
tes <- createFolds(dat$NoYes)



# ::: Teste com modelo bayesiano com priori Laplace ::: #
# Log-posteriori
model <- function(parm, x, y){
  # Parameters
  beta <- parm[1:ncol(x)]
  lambda <- exp(parm[ncol(x) + 1])
  
  # Log of Prior Densities
  beta.prior <- sum(dlaplace(beta, 0, lambda, log = TRUE))
  lambda.prior <- dunif(lambda, .001, 10)
  
  # Log-Likelihood
  mu <- plogis(x %*% beta)
  LL <- sum(dbinom(y, 1, mu, log = TRUE))
  
  # Log-Posterior
  LL + beta.prior + lambda.prior
}


bglm <- function(formula, data = NULL, ...) {
  if(is.null(data))
    data <- parent.frame()
  
  mf <- model.frame(formula, data = data)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)
  
  init <- c(rep(0, ncol(x)), 1)
  chain <- MCMCmetrop1R(model, init, 5e3, 2e5, 4, x = x, y = y)
  
  fit <- list(chain = chain, formula = formula)
  class(fit) <- "bglm"
  fit
}


predict.bglm <- function(model, newdata, type = NULL) {
  mf <- model.frame(model$formula, data = newdata)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  
  beta <- model$chain[, 1:ncol(x)]
  pred <- plogis(x %*% t(beta))
  as.vector(rowMeans(pred))
}


m.bglm <- bglm(NoYes ~ ., train)
summary(m.bglm$chain)
plot(m.bglm$chain)

tes <- perfFun(list(glm = m.glm, bglm = m.bglm), c('response', ''), test, 'NoYes')
perfPlot(tes)  # Preticamente a mesma performance

