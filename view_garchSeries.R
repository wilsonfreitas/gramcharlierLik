
rm(list=ls(all.names = TRUE))

source('functions.R')

sol_ <- c(omega=1e-6, alpha=0.2, beta=0.7)
rets <- garchSim(garchSpec(as.list(sol_), cond.dist = "norm"), n=100)
rets <- rets - mean(rets)

e2 <- rets^2
e2t <- sol_[1] + sol_[2]*c(mean(e2), e2[-length(e2)])
s2 <- filter(e2t, sol_[3], "recursive", init=mean(e2))

plot(as.numeric(rets), type='l')
lines(sqrt(s2)*qnorm(0.9), type='l', col='red')
lines(-sqrt(s2)*qnorm(0.9), type='l', col='red')
lines(sqrt(s2)*qnorm(0.95), type='l', col='magenta')
lines(-sqrt(s2)*qnorm(0.95), type='l', col='magenta')

# garch-gc ----

sol_ <- c(
  omega = 1e-6,
  alpha = 0.1,
  beta = 0.8,
  skew = -0.1,
  kurt = 5)

skip = 100
size = 100 + skip
et <- qapx_cf(runif(size), moment2cumulant(c(0, 1, sol_['skew'], sol_['kurt'])))

rets <- garch_sim(sol_['omega'], sol_['alpha'], sol_['beta'], et)
rets <- rets - mean(rets)

s2 <- var.garch(rets, sol_['omega'], sol_['alpha'], sol_['beta'])

plot(as.numeric(rets), type='l')
l90 <- qapx_cf(0.9, moment2cumulant(c(0, 1, sol_['skew'], sol_['kurt'])))
lines(sqrt(s2)*l90, type='l', col='red')
lines(-sqrt(s2)*l90, type='l', col='red')
l95 <- qapx_cf(0.95, moment2cumulant(c(0, 1, sol_['skew'], sol_['kurt'])))
lines(sqrt(s2)*l95, type='l', col='magenta')
lines(-sqrt(s2)*l95, type='l', col='magenta')
