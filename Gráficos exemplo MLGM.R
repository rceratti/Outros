tmp<-seq(0,5,.5)
mu<-plogis(-2+1*tmp)


# Efeito aleatório de intercepto
u1<-rnorm(10,sd=.5)

y<-lapply(tmp,function(tmp) plogis(-2+1*tmp+u1))
y<-do.call(rbind,y)


limites<-range(y)
plot(tmp,mu,ty='l',ylim=limites,lwd=2,col=4)
for(i in 1:10) lines(tmp,y[,i],lty=2)





# Efeito aleatório de intercepto e coeficiente angular
u1<-rnorm(10,sd=.5)
u2<-rnorm(10,sd=.2)

y<-lapply(tmp,function(tmp) plogis(-2+1*tmp+u1+u2*tmp))
y<-do.call(rbind,y)


limites<-range(y)
plot(tmp,mu,ty='l',ylim=limites,lwd=2,col=4)
for(i in 1:10) lines(tmp,y[,i],lty=2)

df<-data.frame(tmp=tmp,y)

library(reshape)
df<-melt(df,id='tmp')



