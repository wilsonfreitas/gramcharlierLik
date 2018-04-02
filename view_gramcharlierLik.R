
library(ggplot2)
library(PDQutils)

# functions ----

source('functions.R')

# gc check ----

skewness <- -0.1
kurtosis <- 4.5
rnd_ <- runif(1000)
x <- qapx_cf(rnd_, moment2cumulant(c(0,1, skewness, kurtosis)))

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
           y=skewness, x=kurtosis)

logLik <- gcUnconstrainedlogLik(x)

res <- nlminb(c(0, 0), logLik, lower=c(-10, -10), upper=c(10, 10))
uncons_regionD(res$par[1], res$par[2])
res$objective
-sum(log(dgramcharlier(x, mu3=skewness, mu4=kurtosis)))

res <- DEoptim(logLik, lower=c(-10, -10), upper=c(10, 10), DEoptim.control(NP=100, itermax = 100, storepopfrom = 1, storepopfreq = 2, strategy=2, trace=F))
uncons_regionD(res$optim$bestmem[1], res$optim$bestmem[2])
res$optim$bestval
-sum(log(dgramcharlier(x, mu3=skewness, mu4=kurtosis)))

