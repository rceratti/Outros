library(ggplot2)
library(boot)
library(doSNOW)

period<-factor(1:4)
herd<-factor(1:50)

dat<-expand.grid(period=period,herd=herd)

beta<-c(.7,1.45,1.65,1.9)
X<-model.matrix(~period,dat)

mu<-as.vector(exp(X%*%beta))

dat$resp<-rpois(nrow(dat),mu)

m1<-glm(resp~period,poisson,dat)
summary(m1)



# Inicialização do 'cluster' e geração do gráfico meio-normal
cl<-makeCluster(4)
registerDoSNOW(cl)
clusterEvalQ(cl,library(boot))

hnplot(m1,1000,T) # Antes de rodar, carregue a função abaixo!



# Função para gerar o gráfico meio normal com envelope simulado
hnplot<-function(glmfit,nsim=100,par=FALSE,qinf=.01,qsup=.99){
  res<-glm.diag(glmfit)$rp
  res.1<-sort(abs(res))


  sim.hnp<-function(i,glmfit){
    y<-as.matrix(simulate(glmfit))
    x<-model.matrix(glmfit)

    mfun<-glm(y~x,family(glmfit))

    rp<-glm.diag(mfun)$rp
    sort(abs(rp))
  }


  if(par && foreach::getDoParWorkers()>1){
    tes<-foreach(i=1:nsim,.combine=rbind) %dopar% sim.hnp(i,glmfit)
  } 
  else{
    tes<-lapply(1:nsim,sim.hnp,glmfit=glmfit)
    tes<-do.call(rbind,tes)
  }

  linf<-apply(tes,2,quantile,qinf)
  lsup<-apply(tes,2,quantile,qsup)
  meio<-apply(tes,2,mean)

  nobs<-ncol(tes); t.i<-1:nobs
  ind<-qnorm((t.i+nobs-1/8)/(2*nobs+1/2))
  
  tes.1<-data.frame(ind,linf,meio,lsup,res.1)

  p<-ggplot(tes.1,aes(ind,meio))+geom_line()
  p<-p+geom_line(aes(ind,linf),linetype="dashed")
  p<-p+geom_line(aes(ind,lsup),linetype="dashed")
  p<-p+geom_point(aes(ind,res.1),colour="red")
  p<-p+xlab("Valor esperado do quantil \nmeio normal")
  p+ylab("Resíduos padronizados \nabsolutos ordenados")
}
