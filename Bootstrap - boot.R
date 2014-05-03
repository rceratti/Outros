library(MASS)
library(boot)


# Simulação de dados
S<-matrix(c(10,3,3,2),2,2)
dat<-as.data.frame(mvrnorm(30,c(7,5),S))


# Bootstrap não paramétrico
test.fun<-function(data,i){
  dat<-data[i,]
  colMeans(dat)
}

bs0<-boot(dat,test.fun,5e3,parallel='snow',ncpus=4)

for(i in 1:ncol(bs0$t)) print(boot.ci(bs0,i=i,typ='perc'))
plot(bs0$t)



# Bootstrap paramétrico
rg.fun<-function(data,mle){
  data<-MASS::mvrnorm(nrow(data),mle$mean,mle$Var)
  data
}

mle<-list(mean=colMeans(dat),Var=var(dat))

bs1<-boot(dat,colMeans,5e3,'parametric',ran.gen=rg.fun,mle=mle,parallel='snow',ncpus=4)

for(i in 1:ncol(bs1$t)) print(boot.ci(bs1,i=i,typ='perc'))
plot(bs1$t)



bs0; bs1
apply(dat,2,sd)/sqrt(nrow(dat))


