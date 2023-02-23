#importance sampling to reduce variance from Monte Carlo
#mean = 10,000 & var = 1,000,000


#expected profit from selling n newspapers: E[profit] ~ 1*E[demand]-(11000-E[demand])*0.25
#shape = alpha, rate = beta
#beta = .01
#alpha = 100

#crude monte carlo estimate
x <- rgamma(500,shape=100, rate=.01)
p <- function(x) x - (0.25*(11000-x))
Y<-p(x)
se = sd(Y)/sqrt(length(Y))
CI = 1.96*sd(Y)/sqrt(length(Y))

#importance sampling using gamma distribution with rate of .001
x <- rgamma(500,shape=100, rate=.01)
p <- function(x) x - (0.25*(11000-x))*(exp(-x))
Y<-p(x)
se = sd(Y)/sqrt(length(Y))
CI = 1.96*sd(Y)/sqrt(length(Y))



