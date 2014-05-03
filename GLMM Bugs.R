library(arm)

n<-nrow(cbpp)
n.herd<-length(unique(cbpp$herd))

model.data<-list(
 "incidence" = cbpp$incidence,
 "size" = cbpp$size,
 "period2" = as.numeric(model.matrix(~cbpp$period - 1)[,2]),
 "period3" = as.numeric(model.matrix(~cbpp$period - 1)[,3]),
 "period4" = as.numeric(model.matrix(~cbpp$period - 1)[,4]),
 "herd" = as.numeric(cbpp$herd),
 "n" = n,
 "n.herd" = n.herd
)

model.inits<-function(){
 list(
   B.0 = rnorm(1),
   B.period2 = rnorm(1),
   B.period3 = rnorm(1),
   B.period4 = rnorm(1),
   b.herd = rnorm(15),
   sigma.b.herd = runif(1)
 )
}

params<-c("B.0", "B.period2", "B.period3", "B.period4","sigma.b.herd") 


model<-function(){

 for(i in 1:n){
   incidence[i] ~ dbin(phi2[i], size[i])
   logit(phi[i]) <- B.0 + B.period2*period2[i] + B.period3*period3[i]
                        + B.period4*period4[i] + b.herd[herd[i]]
   phi2[i] <- max(0.00001, min(phi[i], 0.9999999))
 }

 B.0 ~ dnorm(0,0.001)
 B.period2 ~ dnorm(0, 0.001)
 B.period3 ~ dnorm(0, 0.001)
 B.period4 ~ dnorm(0, 0.001)

 for(j in 1:n.herd){
   b.herd[j] ~ dnorm(0, tau.b.herd)
 }

 tau.b.herd <- pow(sigma.b.herd, -2)
 sigma.b.herd ~ dunif(0, 100)
}


write.model(model, con="mcmcTest.bug")
pasta<-"C:/Users/b2415110/Downloads/WinBUGS14/WinBUGS14"

starT<-Sys.time()
bug3<-bugs(model.data, model.inits, params, "mcmcTest.bug",
 n.iter = 500E3, debug = FALSE, bugs.directory=pasta)
runt<-Sys.time() - starT
runt

bug3$summary[which(rownames(bug3$summary) %in% params), c("mean",
"sd", "2.5%", "97.5%", "Rhat", "n.eff")]