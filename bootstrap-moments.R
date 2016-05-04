
x <- read.csv('https://www.quandl.com/api/v1/datasets/YAHOO/INDEX_BVSP.csv?&trim_start=1997-03-12&trim_end=2014-03-31&sort_order=desc', colClasses=c('Date'='Date'))
x <- diff(log(x$Adjusted.Close))
x <- scale(x)

mu = mean(x)
sigma = sd(x)
n = length(x)

sum(x^3)/n

sum(x^3)*n/((n-1)*(n-2))

sum(x^4)/n

sum(x^4)*n*(n-1)/((n-1)*(n-2)*(n-3))

boot.mu4 <- boot::boot(x, function(d, w) {
  n = length(d[w])
  sum(d[w]^4)*n*(n-1)/((n-1)*(n-2)*(n-3))
}, R=9999)

hist(boot.mu4$t, n=50)
