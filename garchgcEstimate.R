
rm(list=ls(all.names = TRUE))

library(nloptr)
library(fGarch)
library(DEoptim)
source('functions.R')

sol_ <- c(omega=1e-6, alpha=0.1, beta=0.8, skew=-Inf, kurt=-Inf)

skip = 100
size = 1000 + skip
et <- local({
  rnd_ <- runif(size)
  parms_ <- uncons_regionD(sol_['skew'], sol_['kurt'])
  qapx_cf(rnd_, moment2cumulant(c(0, 1, parms_[1], parms_[2])))
})

rets <- garch_sim(sol_['omega'], sol_['alpha'], sol_['beta'], et)
rets <- rets - mean(rets)

lo <- c(1e-6, 1e-6, 1e-6, -10, -10)
hi <- c(1e-1, 1, 1, 10, 10)
st <- c(var(rets), 0.2, 0.7, 0, 0)

garch_likelihood <- .garch_gc_likelihood(rets)
garch_likelihood(sol_)
garch_likelihood(lo)
garch_likelihood(st)
garch_likelihood(hi)

res_nlminb <- nlminb(st, garch_likelihood, lower=lo, upper=hi, control=list(x.tol=1e-8, trace=0))

res_deoptim1 <- DEoptim(garch_likelihood, lower=lo, upper=hi, DEoptim.control(trace = FALSE))

res_deoptim2 <- DEoptim(garch_likelihood, lower=lo, upper=hi, DEoptim.control(NP=100, itermax = 100, storepopfrom = 1, storepopfreq = 2, strategy=2, trace=F))

res_nlopt <- garch_fit_nlopt(st, garch_likelihood, lower=lo, upper=hi)

res <- rbind(sol_, res_nlminb$par, res_nlopt$solution, res_deoptim1$optim$bestmem, res_deoptim2$optim$bestmem)
res <- cbind(res, -apply(res, 1, garch_likelihood))
res <- t(apply(res, 1, format_par))
colnames(res) <- c('omega', 'alpha', 'beta', 'skewness', 'kurtosis', 'llh')
rownames(res) <- c('solution', 'nlminb', 'nlopt', 'DEoptim', 'DEoptim (tuned)')
res

print(res)


# another test ----

sol_ <- c(omega=1e-6, alpha=0.1, beta=0.8, skew=-1, kurt=-1)

skip = 100
size = 5000 + skip
et <- local({
  rnd_ <- runif(size)
  parms_ <- uncons_regionD(sol_['skew'], sol_['kurt'])
  qapx_cf(rnd_, moment2cumulant(c(0, 1, parms_[1], parms_[2])))
})

rets <- garch_sim(sol_['omega'], sol_['alpha'], sol_['beta'], et)
rets <- rets - mean(rets)

lo <- c(1e-6, 1e-6, 1e-6, -10, -10)
hi <- c(1e-1, 1, 1, 10, 10)
st <- c(var(rets), 0.2, 0.7, 0, 0)

garch_likelihood <- .garch_gc_likelihood(rets)
garch_likelihood(sol_)
garch_likelihood(lo)
garch_likelihood(st)
garch_likelihood(hi)

res_nlminb <- nlminb(st, garch_likelihood, lower=lo, upper=hi, control=list(x.tol=1e-8, trace=0))

res_deoptim1 <- DEoptim(garch_likelihood, lower=lo, upper=hi, DEoptim.control(trace = FALSE))

# res_deoptim2 <- DEoptim(garch_likelihood, lower=lo, upper=hi, DEoptim.control(NP=100, itermax = 100, storepopfrom = 1, storepopfreq = 2, strategy=2, trace=F))

res_nlopt <- garch_fit_nlopt(st, garch_likelihood, lower=lo, upper=hi)

res <- rbind(sol_,
             res_nlminb$par,
             res_nlopt$solution,
             res_deoptim1$optim$bestmem # , res_deoptim2$optim$bestmem
)
res <- cbind(res, -apply(res, 1, garch_likelihood))
res <- t(apply(res, 1, format_par))
colnames(res) <- c('omega', 'alpha', 'beta', 'skewness', 'kurtosis', 'llh')
rownames(res) <- c('solution', 'nlminb', 'nlopt', 'DEoptim' # , 'DEoptim (tuned)'
)
res

print(res)

# real asset ----

rets <- read.csv('https://www.quandl.com/api/v3/datasets/GOOG/BVMF_VALE5.csv?&trim_start=2003-06-02&trim_end=2016-06-02&sort_order=desc', colClasses=c('Date'='Date'))
rets <- diff(log(rets$Close))
rets <- rets - mean(rets)

lo <- c(1e-4, 1e-5, 1e-5, -10, -10)
hi <- c(1e-1, 1, 1, 10, 10)
st <- c(var(rets), 0.2, 0.7, 0, 0)

garch_likelihood <- .garch_gc_likelihood(rets)
garch_likelihood(lo)
garch_likelihood(st)
garch_likelihood(hi)

res_nlminb <- nlminb(st, garch_likelihood, lower=lo, upper=hi, control=list(x.tol=1e-8, trace=0))
tr_par(res_nlminb$par)
res_deoptim1 <- DEoptim(garch_likelihood, lower=lo, upper=hi, DEoptim.control(trace = FALSE))
