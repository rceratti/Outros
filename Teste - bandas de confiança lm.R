n<-100
x<-rnorm(n,sd=3)
mu<--1+.4*x; sig<-sqrt(4)

y<-rnorm(n,mu,sig)

plot(x,y)


m0<-lm(y~x)

mu.h<-fitted(m0)
sig.h<-summary(m0)$sigma

lines(xyTable(x,mu.h))

B<-1e4
mat<-matrix(0,B,n)

for(i in 1:B){
  y.st<-rnorm(n,mu.h,sig.h)
  m1<-lm(y.st~x)
  mat[i,]<-sort(fitted(m1))
}

q.025<-apply(mat,2,quantile,.025)
q.975<-apply(mat,2,quantile,.975)
x.st<-sort(x)

lines(x.st,q.025,lty=2)
lines(x.st,q.975,lty=2)


y.h<-predict(m0,new=data.frame(x=x),se.fit=T)

lines(x.st,sort(y.h$fit-1.96*y.h$se.fit),lty=2,col=2)
lines(x.st,sort(y.h$fit+1.96*y.h$se.fit),lty=2,col=2)



library(doSNOW)

cl<-makeCluster(4)
registerDoSNOW(cl)
clusterSetupRNG(cl)

clusterExport(cl,c('x','n','mu.h','sig.h'))

mat.par<-foreach(i=1:B,.combine=rbind) %dopar% {
  y.st<-rnorm(n,mu.h,sig.h)
  m1<-lm(y.st~x)
  sort(fitted(m1))
}

stopCluster(cl)


q.025p<-apply(mat.par,2,quantile,.025)
q.975p<-apply(mat.par,2,quantile,.975)

lines(x.st,q.025p,lty=2,col=4)
lines(x.st,q.975p,lty=2,col=4)



