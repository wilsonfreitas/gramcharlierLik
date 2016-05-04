
obj <- function(x, mu3=0, mu4=3) {
  f <- function(x) 1 + (mu3/6)*(x^3 - 3*x) + ((mu4 - 3)/24)*(x^4 - 6*x^2 + 3) 
  -2*x*f(x) + (mu3/2)*(x^2 - 1) + ((mu4 - 3)/6)*(x^3 - 3*x)
}

# plot ----

x <- seq(-3, 3, length.out = 100)
plot(x=x, y=obj(x, 0, 7))
abline(h=0)

obj(x, 0, 7)

plot(x=x, y=dgramcharlier(x, 0, 3), type='l', col='red')
lines(x=x, y=dgramcharlier(x, 0, 1/3), col='blue')

# polycoefs ----

polycoefs <- function(mu3, mu4) {
  c5 <- -(mu4 - 3)/12
  c4 <- -mu3/3
  c3 <- (2/3) * (mu4 - 3)
  c2 <- (3/2) * mu3
  c1 <- - ((3/4)*(mu4 - 3) + 2)
  c0 <- -mu3/2
  c(c0, c1, c2, c3, c4, c5)
}

valid_dgramcharlier <-function(parms) {
  coefs <- polycoefs(parms[1], parms[2])
  roots <- polyroot(coefs)
  length(unique(Re(roots)))
}

x <- seq(-10, 10, length.out = 500)
comb <- expand.grid(mu3=seq(-0.5, 0.5, length.out = 50),
                    mu4=seq(2, 8, length.out = 50))

z <- lapply(seq_len(dim(comb)[1]), function(i) {
  parms <- as.numeric(comb[i,])
  data.frame(mu3=parms[1], mu4=parms[2], ncoefs=valid_dgramcharlier(parms))
})

k <- do.call(rbind, z)

ggplot(k, aes(x=mu4, y=mu3)) +
  geom_tile(aes(fill=factor(ncoefs)))
