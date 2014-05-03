library(ggplot2)
library(doSNOW)
library(tweedie)
library(cplm)


## Simulação de dados
period<-factor(1:4)                       # Fator de efeito fixo
herd<-factor(1:50)                        # Fator de efeito aleatório

dat<-expand.grid(period=period,herd=herd)
dat$extra<-rgamma(nrow(dat),2,2)

# beta<-c(.7,1.45,1.65,1.9)
beta<-c(-.3,1.7,2.5,3.4)                  # Vetor de efeitos fixos
X<-model.matrix(~period,dat)              # Matriz de delineamento de efeitos fixos

u<-rnorm(length(levels(dat$herd)),sd=.3)  # Vetor de efeitos aleatórios
Z<-model.matrix(~-1+herd,dat)             # Matriz de delineamento de ef. aleatórios

mu<-as.vector(exp(X%*%beta+Z%*%u))        # Vetor de efeitos médios
phi<-1; p<-1.6                            # Parâmetros da dist. Poisson Composta

dat$resp<-rtweedie(nrow(dat),p,mu,phi)    # Simulação da var. resposta



## Ajuste do modelo  ,nAGQ=5
m0<-cpglmm(resp~period+(1|herd),data=dat)  # QGH-A com 5 pontos
m1<-cpglmm(resp~(1|herd),data=dat)
m2<-cpglmm(resp~extra+(1|herd),data=dat)


## Inicialização do 'cluster' e geração do gráfico meio-normal
cl<-makeCluster(4)            # library(doMC); registerDoMC()
registerDoSNOW(cl)
clusterEvalQ(cl,{library(tweedie);library(cplm)})

(graf0<-hnplot(m0,99,T,'cond'))  # Rodar a função abaixo antes!
(graf1<-hnplot(m1,99,T,'cond'))
(graf2<-hnplot(m2,99,T,'cond'))

(graf0.1<-hnplot(m0,99,T,'marg'))
(graf1.1<-hnplot(m1,99,T,'marg'))
(graf2.1<-hnplot(m2,99,T,'marg'))

stopCluster(cl)


pdf("hnp_PCglmm_cond.pdf",w=11)
print(graf0);print(graf1);print(graf2)
dev.off()

pdf("hnp_PCglmm_marg.pdf",w=11)
print(graf0.1);print(graf1.1);print(graf2.1)
dev.off()



## Gráfico meio-normal com envelope simulado para CP-GLMM (cplm)
hnplot<-function(glmfit,nsim=20,par=FALSE,type){
  
  myres<-function(glmfit,type){   
    y<-glmfit@y
          
    if(type=='marg'){
      X<-model.matrix(glmfit)
      eta<-as.vector(X%*%fixef(glmfit))
      mu<-exp(eta)
    }
    if(type=='cond'){
      mu<-fitted(glmfit)
    }
    
    V<-mu^glmfit@p  
    (y-mu)/sqrt(V)
  }
    
  res<-myres(glmfit,type)
  res.1<-sort(abs(res))


  sim.hnp<-function(i,glmfit,type){
    dat.1<-glmfit@frame
    dat.1[,1]<-rtweedie(nrow(dat.1),glmfit$p,fitted(glmfit),glmfit$phi)
  
    mfun<-update(glmfit,data=dat.1)

    rp<-myres(mfun,type)
    sort(abs(rp))
  }


  if(par){
    tes<-foreach(i=1:nsim,.combine=rbind) %dopar% sim.hnp(i,glmfit,type)
  } else {
    tes<-lapply(1:nsim,sim.hnp,glmfit=glmfit)
    tes<-do.call(rbind,tes)
  }

  linf<-apply(tes,2,quantile,.01)   # apply(tes,2,min)
  lsup<-apply(tes,2,quantile,.99)   # apply(tes,2,max)
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