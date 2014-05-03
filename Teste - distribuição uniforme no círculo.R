rcircle <- function(n, R = 1) {
  r <- R*sqrt(runif(n))
  theta <- 2*pi*runif(n)
  cbind(r*cos(theta), r*sin(theta))
}

area <- function(x, y, u, w) {
  s <- rcircle(1e5)
  s.x <- (s[,1] + x) - u
  s.y <- (s[,2] + y) - w

  circle2 <- (s.x^2 + s.y^2) <= 1
  (2 - mean(circle2)) * pi
}


area(-0.5, 0, 0.5, 0.3)