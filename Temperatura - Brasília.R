library(ggplot2)
library(TSA)


dat <- read.table("http://academic.udayton.edu/kissock/http/Weather/gsod95-current/BZBRSLIA.txt")
dat <- dat[dat$V4 > -20, ]

temp <- 5*(dat$V4-32)/9

d_m <- paste(dat$V2, dat$V1, sep = "-")
l <- expand.grid(1:31, 1:12)
niveis <- paste(l$Var1, l$Var2, sep = "-")
d_m <- factor(d_m, levels = niveis)

plot(ts(temp))
plot(d_m, temp, pch = 20)




ggplot(data.frame(x = d_m, y = temp), aes(x, y)) + geom_point()


dias <- data.frame(dia_mes = niveis, posicao = 1:length(niveis))
ind <- match(d_m, dias$dia_mes)
dat.1 <- data.frame(dia = dias$posicao[ind], temp = temp)
m0 <- loess(temp ~ dia, dat.1, span = .2)
p <- predict(m0, data.frame(dia = 1:length(niveis)))


plot(dat.1$dia, dat.1$temp, pch = 20)
lines(1:length(niveis), p, col = 2)


ggplot(dat, aes(1:length(V4), V4)) + geom_line()


p <- periodogram(dat$V4)
p$freq[which.max(p$spec)]^-1


