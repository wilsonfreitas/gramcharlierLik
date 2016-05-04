
d_poly <- function(x) {
  2*(x^3 - 3*x)^2 - 1*(x^2 - 1)*(x^4 - 6*x^2 + 3)
}

mu3 <- function(x) {
  -12 * (x^3 - 3*x)/d_poly(x)
}

mu4 <-function(x) {
  24 * (x^2 - 1)/d_poly(x) + 3
}

regionD <- function(x) {
  dm <- cbind(x=x, mu3=mu3(x), mu4=mu4(x))
  idx <- order(dm[,'mu4'])
  dm[idx,]
}

adj_seq <- function(start, end, a=-3, length.out=100) {
  x <- seq(start^a, end^a, length.out=length.out)
  x^(1/a)
}

x <- c(seq(-10, -sqrt(3), length.out=100), seq(sqrt(3), 10, length.out=100))
plot(data=cbind(mu3=mu3(x), mu4=mu4(x)), mu3 ~ mu4, type='l')

create_uncons_regionD <- function() {
  x <- adj_seq(sqrt(3), 100)
  rd <- regionD(x)
  rd_curve <- approxfun(rd[,'mu4'], rd[,'mu3'])
  ff <- function(x, a, b) a + (b - a)/(1 + exp(-x))
  function(mu3p, mu4p) {
    mu4 <- ff(mu4p, 3, 7)
    mu3_l <- rd_curve(mu4)
    mu3_u <- -mu3_l
    mu3 <- ff(mu3p, mu3_l, mu3_u)
    c(mu3, mu4)
  }
}

uncons_regionD <- create_uncons_regionD()
