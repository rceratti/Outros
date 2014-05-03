f1 <- function(alpha, s) {
  x <- -qnorm(alpha, 0, s)
  f0 <- function(x, s, alpha) x*dnorm(x, 0, s)/alpha
  integrate(f0, x, +Inf, s = s, alpha = alpha)$value
}

f2 <- function(alpha, s) {
  x <- -qnorm(alpha, 0, s)
  s * exp(-x^2/(2*s^2))/(alpha*sqrt(2*pi))
}

f3 <- function(alpha, s) {
  x <- rnorm(1e5, 0, s)
  mean(x[x > -qnorm(alpha, 0, s)])
}


s <- 2
a <- .01

lapply(list(f1, f2, f3), function(x) eval(as.call(list(x, alpha = a, s = s))))





max.streak <- sapply(1:1e4, function(i) {
  y <- rle(rbinom(100, 1, .9))
  max(y$lengths[y$values == 1])
})

hist(max.streak, fr = F, br = 20)
summary(max.streak)
sd(max.streak)

mean(max.streak >= 51)