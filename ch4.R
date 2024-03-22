## To save the hassle of installing the pacakge, I've copied out the
## data as 'ch4.csv'
##
## library(rethinking)
## data(Howell1)
## write.csv(Howell1, "ch4.csv")

d <- subset(read.csv("ch4.csv"), age >= 18)

likelihood <- function(theta, data) {
  mu <- theta[[1]]
  sigma <- theta[[2]]
  if (sigma < 0) {
    return(-Inf)
  }
  sum(dnorm(data$height, mu, sigma, log = TRUE))
}

prior <- function(theta) {
  mu <- theta[[1]]
  sigma <- theta[[2]]
  dnorm(mu, 178, 20, log = TRUE) + dunif(sigma, 0, 50, log = TRUE)
}

posterior <- function(theta, data) {
  likelihood(theta, data) + prior(theta)
}

## Find the "maximum a posteriori" point (parameter set with the
## highest posterior probability).  Start from mu = 178, sigma = 10
## (because why not).  The control/fnscale argument here just tells
## optim to maximise rather than minimise.  I've also asked for a
## Hessian at the end which is a matrix of second derivatives at the
## optimum.
res <- optim(c(178, 10), posterior, data = d, hessian = TRUE,
             control = list(fnscale = -1))

## The $par elemenbt holds our estimate of the best mean and standard
## deviation. These compare well to 154.61 and 7.73 given in his code
## block 4.29
mu <- res$par[[1]]
sigma <- res$par[[2]]

## We can recover the variance covariance matrix by inverting the
## negative Hessian, and recover the same parameters in 4.32
vcv <- solve(-res$hessian)

## At which point we can draw samples from the approximated version of
## the posterior:
samples <- MASS::mvrnorm(10000,
                         mu = setNames(res$par, c("mu", "sigma")),
                         Sigma = vcv)
plot(samples, col = "#00000022", pch = 19)

## Some disgusting base plotting for your enjoyment
panel.dens <- function(x, ...) {
  dens <- density(x)
  par(usr = c(par("usr")[1:2], 0, max(dens$y) * 1.2))
  lines(dens)
}
pairs(samples, diag.panel = panel.dens, pch = 19, col = "#00000022")
