library(pscl)
library(VGAM)
library(ggplot2)
library(doSNOW)
library(MASS)


m0<-zeroinfl(art~.|1,bioChemists,dist="poisson")
m1<-glm(art~.,data=bioChemists,family=poisson)
m2<-zeroinfl(art~.|1,bioChemists,dist="negbin")
m3<-glm.nb(art~.,data=bioChemists)


# Inicialização do 'cluster' e geração do gráfico meio-normal
cl<-makeCluster(4)
registerDoSNOW(cl)
clusterEvalQ(cl,c(library(pscl),library(VGAM),library(MASS)))

p0<-hnplot(m0,100,T) # Antes de rodar, carregue a função abaixo!
p1<-hnplot(m1,100,T)
p2<-hnplot(m2,100,T)
p3<-hnplot(m3,100,T)


pushViewport(viewport(layout=grid.layout(2,2)))

print(p0,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(p1,vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(p2,vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(p3,vp=viewport(layout.pos.row=2,layout.pos.col=2))



# Função para gerar o gráfico meio normal com envelope simulado
hnplot <- function(glmfit, nsim = 100, par = FALSE, qinf = .01, qsup = .99) {
  res <- resid(glmfit)
  res.1 <- sort(abs(res))

  sim.hnp <- function(i, glmfit){
    mf <- model.frame(glmfit)
    
    if(class(glmfit)[1] == "zeroinfl") {
      X <- model.matrix(glmfit$terms$count, mf)
      Z <- model.matrix(glmfit$terms$zero, mf)

      beta <- glmfit$coefficients$count
      gamma <- glmfit$coefficients$zero

      mu <- exp(X%*%beta)
      phi <- glmfit$linkinv(Z%*%gamma)
      
      if(glmfit$dist == "poisson"){
        mf[,1] <- rzapois(nrow(X), lambda = mu, pobs0 = phi)
      }
      if(glmfit$dist == "negbin"){
        theta <- glmfit$theta
        mf[, 1] <- rzinegbin(nrow(X), size = theta, munb = mu, pstr0 = phi)
      }   
    }
    
    if(class(glmfit)[1] == "glm" | class(glmfit)[1] == "negbin"){
      mf[, 1] <- c(simulate(glmfit))$sim_1
    }

    mfun <- update(glmfit, data = mf)
    rp <- resid(mfun)
    sort(abs(rp))
  }


  if(par && foreach::getDoParWorkers() > 1){
    tes <- foreach(i = 1:nsim, .combine = rbind) %dopar% sim.hnp(i, glmfit)
  } 
  else{
    tes <- lapply(1:nsim, sim.hnp, glmfit = glmfit)
    tes <- do.call(rbind, tes)
  }

  linf <- apply(tes, 2, quantile, qinf)
  lsup <- apply(tes, 2, quantile, qsup)
  meio <- apply(tes, 2, mean)

  nobs <- ncol(tes); t.i <- 1:nobs
  ind <- qnorm((t.i+nobs-1/8)/(2*nobs+1/2))
  
  tes.1 <- data.frame(ind, linf, meio, lsup, res.1)

  p <- ggplot(tes.1, aes(ind, meio)) + geom_line()
  p <- p + geom_line(aes(ind, linf), linetype = "dashed")
  p <- p + geom_line(aes(ind, lsup), linetype = "dashed")
  p <- p + geom_point(aes(ind, res.1), size = 1.5, colour = "red")
  p <- p + xlab("Valor esperado do quantil \nmeio normal")
  p + ylab("Resíduos absolutos ordenados") + theme_bw()
}