/*
# Codigo R
s <- c(5,1,5,14,3,19,1,1,4,22)
t <- c(94.320,15.720,62.880,125.760,5.240,31.440,1.048,1.048,2.096,10.480)
r <- s/t

a0 <- mean(r)^2/(var(r)-mean(1/t)*mean(r))
b0 <- a0/mean(r)

p <- 11
M <- 11000

theta <- matrix(0, M, p)

theta[1, 11] <- b0

# Hiperparâmetros: a_beta=b_beta=0
for(i in 1:M) {
  theta[i, 1:10] <- rgamma(10, shape = a0 + s, rate = t + theta[i, 11])
  if(i + 1 <= M)
    theta[i + 1, 11] <- rgamma(1, shape = 10 * a0, rate = sum(theta[i, 1:10]))
}

theta <- theta[1001:M, ]

t(t(apply(theta[, 1:10], 2, mean)))
mean(a0/theta[, 11])
*/


library(Rcpp)
library(inline)

sig <- signature(s_r = 'numeric', t_r = 'numeric', M_r = 'int')

body <- "
NumericVector s(s_r), t(t_r), t_inv(t.size()), r(s.size());
double        a0, b0, s_lambda;
int           p = 11, M = as<int>(M_r), i, j, k;
NumericMatrix theta(M, p);

for(j = 0; j < t.size(); j++) {
  r[j] = s[j]/t[j];
  t_inv[j] = 1/t[j];
}

a0 = (mean(r)*mean(r))/(var(r)-mean(t_inv)*mean(r));
b0 = a0/mean(r);

theta(0, 10) = b0;

for(i = 0; i < M; i++) {
  s_lambda = 0;
  for(k = 0; k < 10; k++) {
    theta(i, k) = Rf_rgamma(a0 + s[k], 1/(t[k] + theta(i, 10)));
    s_lambda += theta(i, k);
  }
  if(i < M-1)
    theta(i + 1, 10) = Rf_rgamma(10 * a0, 1/s_lambda);
}

return wrap(theta);
"

gibbs.go <- cxxfunction(sig, body, "Rcpp")

th.rcpp <- gibbs.go(s, t, 11000)
th.rcpp <- th.rcpp[1001:M, ]

t(t(apply(th.rcpp[, 1:10], 2, mean)))
mean(a0/th.rcpp[, 11])