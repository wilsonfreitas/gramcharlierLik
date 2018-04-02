
rm(list=ls(all.names = TRUE))

library(fGarch)
library(DEoptim)
source('functions.R')

sol_ <- c(omega=1e-6, alpha=0.2, beta=0.7)

rets <- garchSim(garchSpec(as.list(sol_), cond.dist = "norm"), n=5000)
mu <- mean(rets)
rets <- rets - mu

fit <- garchFit(data=rets, include.mean = FALSE, cond.dist = 'norm', trace = FALSE)

lo <- c(1e-6, 1e-6, 1e-6)
hi <- c(1, 1, 1)
st <- c(var(rets)*0.1, 0.1, 0.8)

garch_likelihood <- .garch_likelihood(rets)
garch_likelihood(sol_)
garch_likelihood(lo)
garch_likelihood(st)
garch_likelihood(hi)

res_nlminb <- nlminb(st, garch_likelihood, lower=lo, upper=hi, control=list(x.tol=1e-8, trace=0))

res_optim <- optim(st, garch_likelihood, lower=lo, upper=hi, method = "L-BFGS-B")

res_deoptim1 <- DEoptim(garch_likelihood, lower=lo, upper=hi, DEoptim.control(trace = FALSE))

res_deoptim2 <- DEoptim(garch_likelihood, lower=lo, upper=hi, DEoptim.control(NP=100, itermax = 100, storepopfrom = 1, storepopfreq = 2, strategy=2, trace=F))

res <- rbind(sol_, res_nlminb$par, res_optim$par, res_deoptim1$optim$bestmem, res_deoptim2$optim$bestmem, fit@fit$par)
res <- cbind(res, -apply(res, 1, garch_likelihood))
colnames(res) <- c('omega', 'alpha', 'beta', 'llh')
rownames(res) <- c('solution', 'nlminb', 'optim', 'DEoptim', 'DEoptim (tuned)', 'fGarch')

print(res)
