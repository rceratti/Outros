library(lme4)
library(MASS)
library(Matrix)
library(reshape)


# Simulação de dados
period<-factor(1:4)
herd<-factor(1:30)

dat<-expand.grid(period=period,herd=herd)


beta.c1<-c(0.70,1.45,1.65,1.90)
beta.c2<-c(0.96,1.39,0.40,1.19)
beta.c3<-c(1.25,1.86,0.19,-0.39)

beta<-matrix(c(beta.c1,beta.c2,beta.c3),4,3)
X<-model.matrix(~-1+period,dat)


f.S<-function(n){
  S<-matrix(0,n,n)
  diag(S)<-rep(1,n)

  corr<-runif(n*(n-1)/2,-.9,.9)
  S[lower.tri(S)]<-S[upper.tri(S)]<-corr

  nearPD(S,corr=T)$mat
}

S<-f.S(3)
u<-mvrnorm(length(levels(dat$herd)),c(0,0,0),S)
Z<-model.matrix(~-1+herd,dat)


Eta<-X%*%beta+Z%*%u
Mu<-exp(Eta)

Mu<-data.frame(Mu)
names(Mu)<-paste("C",1:3,sep="")

dat<-cbind(dat,Mu)
dat<-melt(dat,id=c('period','herd'))
dat$value<-rpois(nrow(dat),dat$value)


# Ajuste do modelo
gm0<-glmer(value~-1+period:variable+(-1+variable|herd),family=poisson,data=dat)

gm1.1<-glmer(value~-1+period+(1|herd),dat[dat$variable=='C1',],poisson)
gm1.2<-glmer(value~-1+period+(1|herd),dat[dat$variable=='C2',],poisson)
gm1.3<-glmer(value~-1+period+(1|herd),dat[dat$variable=='C3',],poisson)

list(gm1.1,gm1.2,gm1.3)


m1.1<-glm(value~-1+period,poisson,data=dat[dat$variable=='C1',])
m1.2<-glm(value~-1+period,poisson,data=dat[dat$variable=='C2',])
m1.3<-glm(value~-1+period,poisson,data=dat[dat$variable=='C3',])

lapply(list(m1.1,m1.2,m1.3),summary)