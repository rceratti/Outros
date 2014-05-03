library(lme4)
(gm1<-glmer(cbind(incidence,size-incidence)~period+(1|herd),
            family=binomial,data=cbpp))


glm(cbind(incidence,size-incidence)~period,binomial,data=cbpp)


y<-cbpp$incidence; n<-cbpp$size
X<-model.matrix(~period,cbpp)
Z<-model.matrix(~-1+herd,cbpp)


f<-function(y,X,n){

  W0<-diag(rep(1,length(y)))
  beta<-rep(10,ncol(X))
  beta.n<-rep(0,ncol(X))
  k<-0; e<-1

  while(e>1e-6){
    k<-k+1
    beta<-beta.n
    eta<-X%*%beta
    mu<-c(n*plogis(eta))
    z<-eta+diag(n/(mu*n-mu^2))%*%(y-mu)
    W<-solve(diag(n/(mu*n-mu^2))%*%W0)
    beta.n<-solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z
    e<-max(abs(beta.n-beta))
  }  
    
  list(beta=beta,k=k)
}

f(y,X,n)

  
  






myglm<-function(formula,family,data){
  
  if(is.character(family)) {
    fa<-eval(call(family))
  } else{
    if(is.function(family))  fa<-family() else fa<-eval(family)
  }
  
  mfx<-model.frame(formula,data=data)
  X<-model.matrix(attr(mfx,"terms"),data=mfx)
  
  mr<-model.response(mfx)
  
  if(fa$family=="binomial") {
    y<-as.vector(mr[,1])
    n<-as.vector(mr[,2])
  }
  
  if(fa$family!="binomial") {
    y<-as.vector(mr)
    n<-rep(1,length(y)) 
  }
  
  W0<-diag(rep(1,length(y)))
  beta<-rep(10,ncol(X))
  beta.n<-rep(0,ncol(X))
  k<-0; e<-100

  while(e>1e-6){
    k<-k+1
    beta<-beta.n
    eta<-c(X%*%beta)
    mu<-n*fa$linkinv(eta)
    d.mu<-n*fa$mu.eta(eta)
    z<-eta+diag(1/d.mu)%*%(y-mu)
    W<-solve(diag(1/d.mu)^2%*%diag(fa$variance(mu/n))%*%W0)
    beta.n<-solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z
    e<-max(abs(beta.n-beta))
  }

  b<-beta.n; var.b<-solve(t(X)%*%W%*%X)
  se<-sqrt(diag(var.b))
  tval<-b/se; p.value<-2*pnorm(-abs(tval))
  coef<-data.frame(Estimativa=b,EP=se,t.obs=tval,p.valor=p.value)
  
  rownames(coef)<-colnames(X)
        
  printCoefmat(coef,P.value=TRUE,has.Pvalue=TRUE)
}

myglm(cbind(incidence,size)~period,binomial,data=cbpp)
  
  
  
  