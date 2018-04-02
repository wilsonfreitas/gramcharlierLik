
source('functions.R')

x <- c(seq(-10, -sqrt(3), length.out=100), seq(sqrt(3), 10, length.out=100))
plot(data=cbind(mu3=mu3(x), mu4=mu4(x)), mu3 ~ mu4, type='l')
