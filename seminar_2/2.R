set.seed(1)

x = runif(1, -5, 5)
sin(x)


k = 0:10
mysin = (-1) ^ k * x ^ (2*k + 1) / factorial(2 * k + 1)
mysin = cumsum(mysin)

plot(mysin, type = 'l')
abline(h = sin(x), col = 'red')
