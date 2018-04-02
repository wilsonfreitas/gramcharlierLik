
source('functions.R')

x <- garchSim(garchSpec(list(omega=1e-6, alpha=0.1, beta=0.8), cond.dist = "norm"), n=2500)
# x <- read.csv('https://www.quandl.com/api/v1/datasets/YAHOO/INDEX_BVSP.csv?&trim_start=1997-03-12&trim_end=2014-03-31&sort_order=desc', colClasses=c('Date'='Date'))
# https://www.quandl.com/api/v3/datasets/YAHOO/SA_PETR3.csv?api_key=nJ1NhTYdEs2p3MsS4CVd
# x <- read.csv('https://www.quandl.com/api/v3/datasets/YAHOO/SA_PETR3.csv?&trim_start=1997-03-12&trim_end=2014-03-31&sort_order=desc', colClasses=c('Date'='Date'))
# x <- diff(log(x$Adjusted.Close))
# x <- scale(x)

library(fGarch)
fit <- garchFit(data=x, include.mean = FALSE, cond.dist = 'norm')

# res <- garch_fit(x)

# var.garch(x, coef(fit)['omega'], coef(fit)['alpha1'], coef(fit)['beta1'])

# var.garch <- function(rets, omega, alpha, beta) {
#     sig.p <- rets[1]*rets[1]
#     sigs <- vapply(rets[c(-1,-length(rets))],
#         function(r) sig.p <<- omega + alpha*r*r + beta*sig.p, 0)
#     c(rets[1]*rets[1], sigs)
# }

rets <- as.numeric(x)

omega <- 1e-6
alpha <- 0.1
beta <- 0.8
mu <- mean(rets)
skew <- 0
kurt <- 3
# scaling and optimization bounds
small <- 1e-6
lo <- c(small, small, small, -10, -10)
hi <- c(100*abs(mu), 1-small, 1-small, 10, 10)
# setting up likelihood function
garch_likelihood <- .garch_likelihood(rets)
# optimization
# res <- optim(c(omega, alpha, beta), garch_likelihood, lower=lo, upper=hi, method='L-BFGS-B')
res <- nlminb(c(omega, alpha, beta, skew, kurt), garch_likelihood, lower=lo, upper=hi, control=list(x.tol=1e-8, trace=0))

garch_likelihood <- .garch_likelihood(rets - mu)
small <- 1e-8
lo <- c(small, small, small)
hi <- c(1, 1, 1)
res <- nlminb(c(omega, alpha, beta), garch_likelihood, lower=lo, upper=hi, control=list(x.tol=1e-8, trace=0))
res <- optim(res$par, garch_likelihood, lower=lo, upper=hi, method = "L-BFGS-B")
res <- DEoptim(garch_likelihood, lower=lo, upper=hi)
summary(res)
res <- DEoptim(garch_likelihood, lower=lo, upper=hi, DEoptim.control(NP=100, itermax = 100, storepopfrom = 1, storepopfreq = 2, strategy=2, trace=F))
summary(res)
plot(res, plot.type="bestvalit", type='l')
plot(res, plot.type="bestmemit", type='l')
parms <- res$optim$bestmem

rets_ <- rets - mu
garch_ <- var.garch(rets_, parms[1], parms[2], parms[3])
llk1_ <- - sum( 0.5*log(2*pi) + 0.5*log(garch_) + 0.5*(rets_[-1]^2)/garch_ )
dens_ <- dnorm(rets_[-1], sd=sqrt(garch_))
llk2_ <- sum( log(dens_) )
dens_ <- dgramcharlier(rets_[-1]/sqrt(garch_), mu3=skew, mu4=kurt)
sum( log( dens_ ) )
dens_ <- dt(rets_[-1]/sqrt(garch_), df = 4)
sum( log( dens_ ) )

garch_likelihood <- .garch_gc_likelihood(rets - mu)
small <- 1e-8
lo <- c(small, small, small, -10, -10)
hi <- c(1, 1, 1, 10, 10)
res <- nlminb(c(omega, alpha, beta, 0, -10), garch_likelihood, lower=lo, upper=hi, control=list(x.tol=1e-8, trace=0))
res <- optim(c(omega, alpha, beta, skew, kurt), garch_likelihood, lower=lo, upper=hi, method = "L-BFGS-B")
res <- DEoptim(garch_likelihood, lower=lo, upper=hi)
summary(res)
res <- DEoptim(garch_likelihood, lower=lo, upper=hi, DEoptim.control(NP=100, itermax = 200, storepopfrom = 1, storepopfreq = 2, strategy=2, trace=F))
summary(res)
plot(res, plot.type="bestvalit", type='l')
plot(res, plot.type="bestmemit", type='l')
parms <- res$par

garch_likelihood(c(1e-6, 0.1, 0.8, skewness(x), kurtosis(x, method = 'moment')))

# omega         alpha1        beta1 
# 0.0000083917  0.1087357     0.8735381
# 0.0000010     0.2556101     0.8765230

uncons_regionD(parms[4], parms[5])

# setting up returning values
par <- res$par
parms <- list(omega=par[1], alpha=par[2], beta=par[3], skewness=par[4], kurtosis=par[5])
parms$gamma <- 1 - parms$alpha - parms$beta
parms$long_term_variance <- parms$omega/parms$gamma
parms$long_term_volatility <- sqrt(parms$long_term_variance)
parms$long_term_volatility_annu <- parms$long_term_volatility*sqrt(252)
parms


# deoptim ----

library(DEoptim)

rastrigin <- function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))

est.ras <- DEoptim(rastrigin,lower=c(-5,-5),upper=c(5,5), control=list(storepopfrom=1, trace=FALSE))

shaffer <- function(parms) {
  x <- parms[1]
  y <- parms[2]
  z <- 0.5 - (sin(sqrt(x^2 + y^2))^2 - 0.5)/(1 + 0.001*(x^2 + y^2))^2
  -z
}

est.ras <- DEoptim(shaffer, lower=c(-5, -5), upper=c(5, 5))
summary(est.ras)
plot(est.ras, plot.type="bestvalit", type='l')
plot(est.ras, plot.type="bestmemit", type='b')

est.ras <- DEoptim(shaffer, lower=c(-5, -5), upper=c(5, 5), DEoptim.control(itermax = 500, storepopfrom = 1, storepopfreq = 2))
summary(est.ras)
plot(est.ras, plot.type="bestvalit", type='l')
plot(est.ras, plot.type="bestmemit", type='l')

est.ras <- DEoptim(shaffer, lower=c(-5, -5), upper=c(5, 5), DEoptim.control(itermax = 500, storepopfrom = 1, storepopfreq = 2, strategy=2, trace=F))
summary(est.ras)
plot(est.ras, plot.type="bestvalit", type='l')
plot(est.ras, plot.type="bestmemit", type='l')
