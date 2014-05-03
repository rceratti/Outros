f <- function(n) {
  a <- rbeta(n, 2, 2)
  b <- runif(n, 0, 1-a)
  matrix(c(a, b), ncol = 2)
}

x <- f(1e5)
plot(x)
summary(x)
summary(rowSums(x))

op <- par(mfrow = c(1, 2))
hist(x[,1])
hist(x[,2])
par(op)


MASS::fitdistr(x[,2], 'beta', list(shape1 = .75, shape2 = 2))
y <- seq(.001, .999, l = 100)
hist(x[,2], fr = F)
lines(y, dbeta(y, 0.746560357, 2.205939480))

f <- function(x) 1/(1-x)
integrate(f, 0, 1)
plot(y, f(y), ty = 'l')