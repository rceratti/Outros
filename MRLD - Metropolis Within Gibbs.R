library(MASS)
library(mvtnorm)
library(coda)

data(trees)
attach(trees)
y<-Volume^(1/3)
x<-z<-model.matrix(~Girth+Height)

theta<-MWG.HetReg(y,x,z,1e4,1e3)

apply(theta,2,mean)
apply(theta,2,sd)
summary(theta)

plot(theta)


##
MWG.HetReg<-function(y,x,z,mcmc.iter=5e4,burnin=1e4){
  cx<-ncol(x)
  cz<-ncol(z)
  cpar<-cx+cz
  
  # Posteriori
  log.post<-function(y,x,z,beta,gama) {
    parm<-c(beta,gama)
    
    ll<-sum(dnorm(y,x%*%beta,sqrt(exp(z%*%gama)),log=T))
    lp<-sum(dnorm(parm,0,1e2,log=T))
    
    ll+lp
  }
  
  
  # Gibbs (beta) e M-H (gama)
  # Prioris das propostas:
  b.g<-rep(0,cx); B.g<-diag(rep(10^2,cx)); B.g.inv<-solve(B.g)
  g.b<-rep(0,cz); G.b<-diag(rep(10^2,cz)); G.b.inv<-solve(G.b)
  
  # Valores iniciais:
  theta0<-rep(0,cpar)
  
  M<-mcmc.iter
  theta<-matrix(NA,M,cpar)
  theta[1,]<-theta0
  
  for(i in 2:M){
    b.old<-theta[i-1,1:cx]
    g.old<-theta[i-1,(cx+1):(cx+cz)]
    
    # Beta:
    Sig.inv<-diag(c(1/exp(z%*%g.old)))
    B.g.star<-solve(B.g.inv+t(x)%*%Sig.inv%*%x)
    b.g.star<-B.g.star%*%(B.g.inv%*%b.g+t(x)%*%Sig.inv%*%y)
    b.new<-mvrnorm(1,b.g.star,B.g.star)
    theta[i,1:cx]<-b.new
    
    # Gama:
    y.til<-z%*%g.old+((y-x%*%b.new)^2/exp(z%*%g.old))-1
    G.b.star<-solve(G.b.inv+.5*t(z)%*%z)
    g.b.star<-G.b.star%*%(G.b.inv%*%g.b+.5*t(z)%*%y.til)
    g.prop<-mvrnorm(1,g.b.star,G.b.star)
    y.til.cond<-z%*%g.prop+((y-x%*%b.new)^2/exp(z%*%g.prop))-1
    g.b.cond<-G.b.star%*%(G.b.inv%*%g.b+.5*t(z)%*%y.til.cond)
    
    pos.g<-(log.post(y,x,z,b.new,g.prop)+
            dmvnorm(c(g.old),c(g.b.cond),G.b.star,log=T))-
           (log.post(y,x,z,b.new,g.old)+
            dmvnorm(g.prop,c(g.b.star),G.b.star,log=T))
    
    u2<-runif(1)  
    if(log(u2) < pos.g){
      theta[i,4:6]<-g.prop
    } 
    else{
      theta[i,4:6]<-g.old
    }
  }
  
  mcmc(theta[-(1:burnin),],burnin+1)
}



