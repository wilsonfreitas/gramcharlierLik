
library(xts)


x = rbcb::get_series(c(IBOVESPA = 7),
                        start_date = "2000-01-01",
                        end_date = "2017-12-31", as = "xts")
x = na.omit(x)

x.s <- x['2003/']
x.r <- PerformanceAnalytics::Return.calculate(x.s, 'log')

x.r <- scale(x.r)
plot(x.r)

x.r.s <- rollapply(x.r, 756L, timeDate::skewness)
plot(x.r.s)

x.r.k.m <- rollapply(x.r, 756L, timeDate::kurtosis, method = "moment")
plot(x.r.k.m)

x.r.k.f <- rollapply(x.r, 756L, timeDate::kurtosis, method = "fisher")
plot(x.r.k.f)

# bootstrap mu4 ----

boot.mu4_moment <- boot::boot(x.r, function(d, w) {
  # n = length(d[w])
  # sum(d[w]^4)*n*(n-1)/((n-1)*(n-2)*(n-3))
  timeDate::kurtosis(d[w], method = "moment")
}, R=9999)

hist(boot.mu4_moment$t, n=50)

boot.mu4_fisher <- boot::boot(x.r, function(d, w) {
  # n = length(d[w])
  # sum(d[w]^4)*n*(n-1)/((n-1)*(n-2)*(n-3))
  timeDate::kurtosis(d[w], method = "fisher")
}, R=9999)

hist(boot.mu4_fisher$t, n=50)
