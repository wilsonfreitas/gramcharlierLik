
library(PDQutils)
library(nloptr)
library(fGarch)
library(DEoptim)
source('functions.R')

# ----

omega = 1e-6
alpha = 0.1
beta = 0.8
skew = -0.1
kurt = 4

skip = 100
size = 1000 + skip
et <- local({
  rnd_ <- runif(size)
  qapx_cf(rnd_, moment2cumulant(c(0,1, skew, kurt)))
})

logLik <- gcUnconstrainedlogLik(et)
res <- optim(c(0, 3), logLik, method='Nelder-Mead')
uncons_regionD(res$par[1], res$par[2])
