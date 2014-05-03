library(sspir)
library(dlm)

# DLM utilizando o pacote sspir
# Série de altura do rio Nilo
data("Nile", package = "datasets")

# 1a forma de construção do modelo (passeio aleatório com ruido normal)
m1.sspir<-SS(Fmat=function(tt,x,phi){
return(matrix(1))
},Gmat=function(tt,x,phi) {
return(matrix(1))
},Vmat=function(tt,x,phi) {
return(matrix(exp(phi[1])))
},Wmat=function(tt,x,phi) {
return(matrix(exp(phi[2])))
},y=as.matrix(Nile,ncol=1))

C0(m1.sspir)<-matrix(10^7,1,1)
phi(m1.sspir)<-c(9,7)

m1.sspir.f<-kfilter(m1.sspir)


# 2a forma de construção do modelo
mod2.sspir<-ssm(Nile~tvar(1),family="gaussian",C0=diag(1)*10^7)

plot(Nile)
lines(1871:1970,mod2.sspir$ss$mu,lty=2,col=4)


# Modelo com tendência liear variando no tempo
mod3.sspir<-ssm(Nile~tvar(polytime(1:length(Nile)),1),family="gaussian")
lines(1871:1970,mod3.sspir$ss$mu,lty=2,col=2)


# Modelo Holt-Winters (sem tendência e sem sazonalidade) para comparação
mod<-HoltWinters(Nile,beta=F,gamma=F)
plot(mod)
plot(fitted(mod))



# Utilizando o pacote dlm
# Construçao do modelo utilizando todos os 'defaults'
buildFun<-function(x) {
dlmModPoly(1)
}
mod1<-dlmMLE(Nile,parm=0,build=buildFun)

modNileReg<-buildFun(mod1$par)

modSmooth<-dlmSmooth(Nile,mod=modNileReg)
plot(Nile)
lines(ts(modSmooth$s[-1],start=1871),lty=2,col=2)


# Construçao do modelo pelo autor
buildFun<-function(x) {
dlmModPoly(1,dV=exp(x[1]),dW=exp(x[2]))
}
mod2<-dlmMLE(Nile,parm=0,build=buildFun)

modNileReg<-buildFun(mod2$par)

modSmooth<-dlmSmooth(Nile,mod=modNileReg)
lines(ts(modSmooth$s[-1],start=1871),lty=2,col=4)


# Modelo com intervenção (com todos os 'defaults' das funções)
x<-matrix(c(rep(0,27),rep(1,length(Nile)-27)),ncol=1)
buildFun <- function(theta) {
dlmModReg(x)
}
mod3<-dlmMLE(Nile,parm=0,build=buildFun)

modNileReg<-buildFun(mod3$par)

modSmooth<-dlmSmooth(Nile,mod=modNileReg)
lines(ts(modSmooth$s[-1,1]+modSmooth$s[-1,2]*x,start=1871),lty=2)

nileFilt<-dlmFilter(Nile,modNileReg)
lines(ts(nileFilt$m[-1,1]+nileFilt$m[-1,2]*x,start=1871),lty=2,col=4)

nileFore<-dlmForecast(nileFilt,nAhead=10)



# Dados de insetos - contagem no tempo
b<-read.table("clipboard")
b<-ts(b)

buildFun<-function(x) {
dlmModPoly(2)+dlmModSeas(12)
}
mod1<-dlmMLE(log(b),parm=0,build=buildFun)

modbReg<-buildFun(mod1$par)

modFilt<-dlmFilter(log(b),mod=modbReg)
filtro<-dropFirst(modFilt$m)


plot(log(b))

m3<-ssm(log(b)~-1+tvar(polytime(1:length(b)),1)+
  tvar(polytrig(1:length(b)),12),family="gaussian")
lines(1:length(b),m3$ss$mu,type='l',lty=2,col=4)


buildFun<-function(x) {
dlmModPoly(1)
}
mod1<-dlmMLE(y,parm=0,build=buildFun)

modbReg<-buildFun(mod1$par)

modSmooth<-dlmSmooth(y,mod=modbReg)
lines(ts(modSmooth$s[-1]),lty=2,col=4)

modFilt<-dlmFilter(y,mod=modbReg)
lines(ts(modFilt$m[-1]),lty=2,col=5)

for (i in 1:100){
  lines(dropFirst(dlmBSample(modFilt)),col="brown")
}





