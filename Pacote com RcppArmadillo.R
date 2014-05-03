library(RcppArmadillo)
library(inline)


funCode1<-'
arma::mat    A=Rcpp::as<arma::mat>(a);
arma::colvec B=Rcpp::as<arma::colvec>(b);

return(Rcpp::wrap(A*B));
'

funCode2<-'
arma::mat    A=Rcpp::as<arma::mat>(a);
arma::colvec B=Rcpp::as<arma::colvec>(b);

arma::mat C=arma::ones(A.n_rows,1)*B.t();

return(Rcpp::wrap(A+C));
'

funSig1<-funSig2<-signature(a='numeric',b='numeric')

funcs<-cxxfunction(sig=list(myFun1=funSig1,myFun2=funSig2),
                   body=list(funCode1,funCode2),
                   plugin='RcppArmadillo')



myFunR<-function(a,b){
  if(!is.matrix(a)) stop('blah 1')
  if(ncol(a)!=length(b)) stop('blah 2')
  
  r1<-.Call('myFun1',a,b,PACKAGE='testpack')
  r2<-.Call('myFun2',a,b,PACKAGE='testpack')
  
  list(r1=r1,r2=r2)
}


RcppArmadillo.package.skeleton('testpack','myFunR',example_code=FALSE)

setwd('~/testpack/src')
cat(funcs[[1]]@code,'testpack.cpp')
