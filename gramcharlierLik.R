
library(ggplot2)

# functions ----

dgramcharlier <- function(x, mu3=0, mu4=3) {
  psi <- function(x) ( 1 + mu3*(x^3 - 3*x)/6 + (mu4 - 3)*(x^4 - 6*x^2 + 3)/24 )
  dnorm(x)*psi(x)
}

dgramcharlier_adj <- function(x, mu3=0, mu4=3) {
  psi <- function(x) ( 1 + mu3*(x^3 - 3*x)/6 + (mu4 - 3)*(x^4 - 6*x^2 + 3)/24 )
  .gamma <- 1 + (mu3^2)/6 + ((mu4 - 3)^2)/24
  dnorm(x)*(psi(x)^2)/.gamma
}

gc_check <- function(x) {
  function(parms) {
    gc <- dgramcharlier(x, mu3=parms[1], mu4=parms[2])
    if (any( gc < 0 ))
      FALSE
    else
      TRUE
  }
}

gclogLik <- function(x) {
  function(parms) {
    gc <- dgramcharlier(x, mu3=parms[1], mu4=parms[2])
    if (any( gc < 0 ))
      NA
    else
      -sum(log(gc))
  }
}

gcUnconstrainedlogLik <- function(x) {
  function(parms) {
    parms <- uncons_regionD(parms[1], parms[2])
    gc <- dgramcharlier(x, mu3=parms[1], mu4=parms[2])
    gc <- abs(gc)
    -sum(log(gc))
  }
}

# gc check ----

x <- rnorm(1000) # seq(-5, 5, length.out = 1000) # rnorm(1000)

comb <- expand.grid(mu3=seq(-2, 2, length.out = 100),
                    mu4=seq(0, 10, length.out = 100))
logLik <- gc_check(x)
z <- sapply(seq_len(dim(comb)[1]), function(i) {
  parms <- as.numeric(comb[i,])
  logLik(parms)
})

comb$check <- z

ggplot(comb, aes(x=mu4, y=mu3, colour=check)) +
  geom_jitter()

# gc loglik ----

x <- read.csv('https://www.quandl.com/api/v1/datasets/YAHOO/INDEX_BVSP.csv?&trim_start=1997-03-12&trim_end=2014-03-31&sort_order=desc', colClasses=c('Date'='Date'))
x <- diff(log(x$Adjusted.Close))
x <- scale(x)

comb <- expand.grid(mu3=seq(-2, 2, length.out = 100),
                    mu4=seq(0, 20, length.out = 100))

logLik <- gclogLik(x)
z <- sapply(seq_len(dim(comb)[1]), function(i) {
  parms <- as.numeric(comb[i,])
  logLik(parms)
})

comb$ll <- z
h <- hist(z, plot = FALSE)
comb$llgroups <- cut(z, h$breaks)

ggplot(comb, aes(x=mu4, y=mu3, colour=llgroups)) +
  geom_jitter() +
  annotate('text', label='X',
           y=timeDate::skewness(x), x=timeDate::kurtosis(x, method='moment'))

ggplot(comb, aes(x=mu4, y=mu3, colour=ll)) +
  geom_jitter() +
  annotate('text', label='X',
           y=timeDate::skewness(x), x=timeDate::kurtosis(x, method='moment'))

# ggplot(comb, aes(x=mu4, y=mu3, colour=ll)) +
#   geom_tile(aes(fill=ll)) +
#   annotate('text', label='X',
#            y=timeDate::skewness(x), x=timeDate::kurtosis(x, method='moment'))

# gc unconstrained loglik ----

x <- rnorm(1000)
# x <- read.csv('https://www.quandl.com/api/v1/datasets/YAHOO/INDEX_BVSP.csv?&trim_start=1997-03-12&trim_end=2014-03-31&sort_order=desc', colClasses=c('Date'='Date'))
# x <- diff(log(x$Adjusted.Close))
# x <- scale(x)

comb <- expand.grid(mu3=seq(-10, 10, length.out=100),
                    mu4=seq(-10, 10, length.out=100))

logLik <- gcUnconstrainedlogLik(x)
z <- sapply(seq_len(dim(comb)[1]), function(i) {
  parms <- as.numeric(comb[i,])
  # p <- uncons_regionD(parms[1], parms[2])
  # l <- logLik(parms)
  # c(p, l)
  logLik(parms)
})

comb$ll <- z
h <- hist(z, plot = FALSE)
comb$llgroups <- cut(z, h$breaks)

ggplot(comb, aes(x=mu4, y=mu3, colour=llgroups)) +
  geom_jitter() +
  annotate('text', label='X',
           y=timeDate::skewness(x), x=timeDate::kurtosis(x, method='moment'))

ggplot(comb, aes(x=mu4, y=mu3, colour=ll)) +
  geom_jitter() +
  annotate('text', label='X',
           y=timeDate::skewness(x), x=timeDate::kurtosis(x, method='moment'))

# ggplot(comb, aes(x=mu4, y=mu3, colour=ll)) +
#   geom_tile(aes(fill=ll)) +
#   annotate('text', label='X',
#            y=timeDate::skewness(x), x=timeDate::kurtosis(x, method='moment'))

# compare logLik ----

x <- read.csv('https://www.quandl.com/api/v1/datasets/YAHOO/INDEX_BVSP.csv?&trim_start=1997-03-12&trim_end=2014-03-31&sort_order=desc', colClasses=c('Date'='Date'))
x <- diff(log(x$Adjusted.Close))
x <- scale(x)

comb <- expand.grid(mu3=seq(-1.5, 1.5, length.out = 100),
                    mu4=seq(2, 8, length.out = 100))

logLik <- gclogLik(x)
z <- sapply(seq_len(dim(comb)[1]), function(i) {
  parms <- as.numeric(comb[i,])
  logLik(parms)
})

comb$ll <- z
h <- hist(z, 10, plot = FALSE)
comb$llgroups <- cut(z, h$breaks)

ggplot(comb, aes(x=mu4, y=mu3, colour=llgroups)) +
  geom_jitter()

x <- rnorm(1000)
logLik <- gclogLik(x)
z <- sapply(seq_len(dim(comb)[1]), function(i) {
  parms <- as.numeric(comb[i,])
  logLik(parms)
})

comb$ll <- z
h <- hist(z, 10, plot = FALSE)
comb$llgroups <- cut(z, h$breaks)

ggplot(comb, aes(x=mu4, y=mu3, colour=llgroups)) +
  geom_jitter()

# opt <- optim(c(0, 3), logLik, method='L-BFGS-B', lower=c(-0.5, 0), upper=c(0.5, 8))
# 
# ggplot(comb, aes(x=mu4, y=mu3)) +
#   geom_tile(aes(fill=ll)) +
#   annotate('text', label='X',
#            y=timeDate::skewness(x), x=timeDate::kurtosis(x, method='moment')) +
#   annotate('text', label='X',
#            y=opt$par[1], x=opt$par[2])

# optim ----

x <- read.csv('https://www.quandl.com/api/v1/datasets/YAHOO/INDEX_BVSP.csv?&trim_start=1997-03-12&trim_end=2014-03-31&sort_order=desc', colClasses=c('Date'='Date'))
x <- diff(log(x$Adjusted.Close))
x <- scale(x)

logLik <- gcUnconstrainedlogLik(x)

res <- optim(c(0, 3), logLik, method='BFGS') # , lower=c(-5, 3), upper=c(5, 7)
uncons_regionD(res$par[1], res$par[2])
optim(c(0, 3), logLik, method='L-BFGS-B', lower=c(-1, 3), upper=c(1, 7))

# plot ----

x <- seq(-5, 5, length.out = 100)
plot(x, dgramcharlier(x, mu3=1, mu4=3))
