
library(xts)


x <- Quandl::Quandl('YAHOO/INDEX_BVSP', type = 'xts')

x.s <- x['2003/']
x.r <- PerformanceAnalytics::Return.calculate(x.s[,'Close'], 'log')

x.r <- scale(x.r)
plot(x.r)

x.r.s <- rollapply(x.r, 756L, timeDate::skewness)
plot(x.r.s)

x.r.k <- rollapply(x.r, 756L, timeDate::kurtosis)
plot(x.r.k)

plot(cbind(coredata(x.r.s), coredata(x.r.k)+3))

sum(x^3)/n

sum(x^3)*n/((n-1)*(n-2))

sum(x^4)/n

sum(x^4)*n*(n-1)/((n-1)*(n-2)*(n-3))

boot.mu4 <- boot::boot(x, function(d, w) {
  n = length(d[w])
  sum(d[w]^4)*n*(n-1)/((n-1)*(n-2)*(n-3))
}, R=9999)

hist(boot.mu4$t, n=50)
