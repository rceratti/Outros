library(ggplot2)
library(lme4)
library(doSNOW)


# Simulação de dados
period<-factor(1:4)
herd<-factor(1:50)

dat<-expand.grid(period=period,herd=herd)

beta<-c(.7,1.45,1.65,1.9)
X<-model.matrix(~period,dat)

u<-rnorm(length(levels(dat$herd)),sd=.3)
Z<-model.matrix(~-1+herd,dat)

mu<-as.vector(exp(X%*%beta+Z%*%u))  
# mu<-as.vector(plogis(X%*%beta+Z%*%u))
# n<-30

dat$resp<-rpois(nrow(dat),mu)  
# dat$resp<-rbinom(nrow(dat),n,mu)


# Ajuste do modelo
# gm0<-glmer(cbind(resp,n-resp)~period+(1|herd),family=binomial,data=dat)
# gm1<-glmer(cbind(resp,n-resp)~(1|herd),family=binomial,data=dat)
gm0<-glmer(resp~period+(1|herd),family=poisson,data=dat)
gm1<-glmer(resp~(1|herd),family=poisson,data=dat)


# Inicialização do 'cluster' e geração do gráfico meio-normal
cl<-makeCluster(4)
registerDoSNOW(cl)
clusterEvalQ(cl,library(lme4))

(graf0<-hnplot(gm0,par=T,type='cond'))  # Rodar a função abaixo antes!
(graf1<-hnplot(gm0,par=T,type='marg'))
(graf2<-hnplot(gm1,par=T,type='cond'))
(graf3<-hnplot(gm1,par=T,type='marg'))

stopCluster(cl)

pdf("hnp_glmmPois.pdf",w=11)
print(graf0);print(graf1);print(graf2);print(graf3)
dev.off()



# Gráfico meio-normal com envelope simulado para GLMM (lme4)
hnplot<-function(glmfit,nsim=99,par=FALSE,type){
  
  myres<-function(glmfit,type){
            
    if(type=='marg'){
      fam<-eval(glmfit@call$family)
      if(is.function(fam)) fam<-fam()      
      
      w<-glmfit@pWt
      y<-glmfit@y*w
      
      X<-model.matrix(glmfit)
      eta<-as.vector(X%*%fixef(glmfit))
      mu<-fam$linkinv(eta)
      V<-fam$variance(mu)  
      
      res<-(y-w*mu)/sqrt(w*V)
    }
    
    if(type=='cond'){
      res<-resid(glmfit)
    }
    
    res
  }
  
  res<-myres(glmfit,type)
  res.1<-sort(abs(res))


  sim.hnp<-function(i,glmfit,type){
    y<-simulate(glmfit)
    mfun<-refit(glmfit,y)

    rp<-myres(mfun,type)
    sort(abs(rp))
  }


  if(par && foreach::getDoParWorkers()>1){
    tes<-foreach(i=1:nsim,.combine=rbind) %dopar% sim.hnp(i,glmfit,type)
  } 
  else{
    tes<-lapply(1:nsim,sim.hnp,glmfit=glmfit)
    tes<-do.call(rbind,tes)
  }

  linf<-apply(tes,2,min)  # apply(tes,2,quantile,.025)
  lsup<-apply(tes,2,max)  # apply(tes,2,quantile,.975)
  meio<-apply(tes,2,mean)

  nobs<-ncol(tes); t.i<-1:nobs
  ind<-qnorm((t.i+nobs-1/8)/(2*nobs+1/2))
  
  tes.1<-data.frame(ind,linf,meio,lsup,res.1)

  p<-ggplot(tes.1,mapping=aes(ind,meio))+geom_line()
  p<-p+geom_line(mapping=aes(ind,linf),linetype="dashed")
  p<-p+geom_line(mapping=aes(ind,lsup),linetype="dashed")
  p<-p+geom_point(mapping=aes(ind,res.1),colour="red")
  p<-p+xlab("Valor esperado do quantil \nmeio normal")
  p+ylab("Resíduos absolutos ordenados")
}