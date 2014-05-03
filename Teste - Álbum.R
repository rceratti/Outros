library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)


colecao <- function() {
  album <- 150
  pacote <- 5
  minimo_pacotes <- album/pacote
  figuras <- 1:album

  lote <- sample(figuras, pacote * minimo_pacotes, TRUE)
  total_pacotes <- minimo_pacotes

  while(!all(figuras %in% lote)) {
    lote <- c(lote, sample(figuras, pacote, TRUE))
    total_pacotes <- total_pacotes + 1
  }

  total_pacotes
}

B <- 1e5
numero_pacotes <- foreach(i = 1:B, .combine = c) %dopar% colecao()
plot(table(numero_pacotes)/B)
summary(numero_pacotes)
mean(numero_pacotes); var(numero_pacotes)
