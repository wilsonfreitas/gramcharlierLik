
library(PDQutils)
library(nloptr)
library(fGarch)
library(DEoptim)
source('functions.R')

omega = 1e-6
alpha = 0.1
beta = 0.8
skew = -0.1
kurt = 5

skip = 100
size = 1000 + skip
et <- local({
  rnd_ <- runif(size)
  qapx_cf(rnd_, moment2cumulant(c(0,1, skew, kurt)))
})

# rets <- garchSim(garchSpec(list(omega=1e-6, alpha=0.1, beta=0.8), cond.dist = "norm"), n=10000)
rets <- garch_sim(omega, alpha, beta, et)
mu <- mean(rets)
rets <- rets - mu

sol_ <- c(omega=omega, alpha=alpha, beta=beta, skew=skew, kurt=kurt)

lo <- c(1e-6, 1e-6, 1e-6, -1, 3)
hi <- c(1e-3, 1, 1, 1, 7)
st <- c(1e-5, 0.2, 0.7, 0, 3)

garch_likelihood <- .garch_gc_likelihood(rets)
garch_likelihood(sol_)
garch_likelihood(lo)
garch_likelihood(st)
garch_likelihood(hi)

res_nlminb <- nlminb(st, garch_likelihood, lower=lo, upper=hi, control=list(x.tol=1e-8, trace=0))
tr_par(res_nlminb$par)
garch_likelihood(res_nlminb$par)

res_deoptim1 <- DEoptim(garch_likelihood, lower=lo, upper=hi)
tr_par(res_deoptim1$optim$bestmem)
garch_likelihood(res_deoptim1$optim$bestmem)

res_deoptim2 <- DEoptim(garch_likelihood, lower=lo, upper=hi, DEoptim.control(NP=100, itermax = 100, storepopfrom = 1, storepopfreq = 2, strategy=2, trace=F))
tr_par(res_deoptim2$optim$bestmem)
garch_likelihood(res_deoptim2$optim$bestmem)

res_nlopt <- garch_fit_nlopt(st, garch_likelihood, lower=lo, upper=hi)
tr_par(res_nlopt$solution)
garch_likelihood(res_nlopt$solution)

res <- rbind(sol_, res_nlminb$par, res_nlopt$solution, res_deoptim1$optim$bestmem, res_deoptim2$optim$bestmem)
res <- cbind(res, -apply(res, 1, garch_likelihood))
colnames(res) <- c('omega', 'alpha', 'beta', 'skewness', 'kurtosis', 'llh')
rownames(res) <- c('solution', 'nlminb', 'nlopt', 'DEoptim', 'DEoptim (tuned)')
res

