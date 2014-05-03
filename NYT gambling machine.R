maquina <- function(regra, R) {
  x <- sample(0:100, R, rep = T)
  y <- sample(0:100, R, rep = T)
  
  # Regra de decisão: x > regra: "y < x"; x <= regra: "y > x"
  # Acerto?
  acerto <- (x > regra & y < x) | (x <= regra & y > x)
  retorno <- acerto * y
  
  # Caso x == y
  iguais <- x == y
  retorno[iguais] <- y[iguais]/2
  
  mean(retorno)
}


regra <- 0:100
resultado <- sapply(regra, maquina, R = 1e6)
regra[which.max(resultado)]; max(resultado)

plot(regra, resultado, pch = 20)
abline(h = max(resultado), v = regra[which.max(resultado)], lty = 2)