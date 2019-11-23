library(MASS)

#help for mvrnorm()

#need to create variance-covariance matrix

varcov <- matrix(c(411.23, 6.906, 6.906, 72.49), nrow = 2, ncol = 2, byrow = T)

x <- mvrnorm(n = 500, mu = c(0,0), Sigma = varcov)

plot(x, cex = 0.2, xlim = c(-50,50), ylim = c(-50,50), xlab = "u0j", ylab = "u1j")


#this is wrong, try using lapply across list
lik <- function (x, z) {
  return(sum(log(dnorm(x, z, 1.92))))
}

y <- c(2, 4, 1, 6, 3)
mu.val <- seq(1, 5, 0.2)

lik(y, mu.val)


#options(scipen = 99) change threshold for scientific notation

#plot(var1 = list of mu values, var2 = Likelihoods based on those mu values)


#two pipes in lmer random effects syntax sets all covariances to zero
