library(ggplot2)

source('functions.R')

# valid 1 ----

valid_dgramcharlier <- function(x) {
  function(parms) {
    y <- dgramcharlier(x, mu3=parms[1], mu4=parms[2])
    # as.numeric(y < 0)
    factor(y < 0, levels=c(FALSE, TRUE))
  }
}

x <- seq(-10, 10, length.out = 500)
comb <- expand.grid(mu3=seq(-0.5, 0.5, length.out = 50),
                    mu4=8)

valid <- valid_dgramcharlier(x)
z <- lapply(seq_len(dim(comb)[1]), function(i) {
  parms <- as.numeric(comb[i,])
  v <- valid(parms)
  data.frame(check=v, x=seq_along(v), y=parms[1])
})

k <- do.call(rbind, z)

ggplot(k, aes(x=x, y=y)) +
  geom_tile(aes(fill=check))

# valid 2 ----

polycoefs <- function(mu3, mu4) {
  c5 <- -(mu4 - 3)/12
}

valid_dgramcharlier <- function(x) {
  function(parms) {
    y <- dgramcharlier(x, mu3=parms[1], mu4=parms[2])
    # as.numeric(y < 0)
    any(y < 0)
  }
}

x <- seq(-10, 10, length.out = 500)
comb <- expand.grid(mu3=seq(-0.5, 0.5, length.out = 50),
                    mu4=seq(2, 8, length.out = 50))

valid <- valid_dgramcharlier(x)
z <- lapply(seq_len(dim(comb)[1]), function(i) {
  parms <- as.numeric(comb[i,])
  v <- valid(parms)
  data.frame(check=v, x=seq_along(v), y=parms[1])
})

k <- do.call(rbind, z)

ggplot(k, aes(x=x, y=y)) +
  geom_tile(aes(fill=check))
