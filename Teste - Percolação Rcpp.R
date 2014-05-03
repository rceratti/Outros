library(Rcpp)
library(inline)
library(doParallel) 


src <- "
int n = as<int>(n_r),
    m = as<int>(m_r);

double p = as<double>(p_r), 
       f = as<double>(f_r),
       y, pp = 0;

NumericVector result(2);

RNGScope scope;

for(int g = 0; g < m; g++) {
  y = 1.;  
  for(int k = 0; k < n; k++)
    y = Rf_rbinom(y*f, p);
  if(y > 0)
  pp +=  1.;
}

result[0] = p;
result[1] = pp/((double) m);

return wrap(result);
"

sig <- signature(f_r = 'double', n_r = 'int', m_r = 'int', p_r = 'double')

progRcpp <- cxxfunction(sig, src, 'Rcpp')


programa3 <- function(f,n,m) { 
  ## f: número máximo de filhos 
  ## n: número de gerações 
  ## m: número de iterações 
  S <- lapply(seq(0, 1, by = 0.01), function(p) { 
    progRcpp(f, n, m, p)
  })
  S <- do.call(rbind, S) 
  return(S)
} 

system.time(prog3 <- programa3(2, 30, 10000)) 
plot(prog3, xlab = "p", ylab = "Probabilidade de Percolação")





cl <- makeCluster(4)
registerDoParallel(cl)
clusterExport(cl, 'progRcpp')

programa2 <- function(f,n,m) { 
  ## f: número máximo de filhos 
  ## n: número de gerações 
  ## m: número de iterações 
  S <- foreach(p = seq(0, 1, by = 0.01), .combine = rbind) %dopar% { 
    pp <- 0 
    for (g in 1:m) { 
      y <- 1 
      for (k in 1:n) 
        y <- rbinom(1, y*f, p) 
      if (y > 0) pp <- pp + 1 
    } 
    c(p, pp/m) 
  } 
  return(S) 
} 

system.time(prog2 <- programa2(2, 50, 1000)) 
plot(prog2, xlab = "p", ylab = "Probabilidade de Percolação")

stopCluster(cl)