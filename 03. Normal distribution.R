# Chapter 6 Normal Distribution

# plot the normal distribution
sd <- c(-3, -2, -1, 0, 1, 2, 3)
density <- dnorm(sd)
plot(sd, density, type = 'o')

# to be specific 
sd <- seq(-3, 3, length.out = 100)
density <- dnorm(sd)
plot(sd, density, type = 'l')

# the probability between -2SD and 2SD
pnorm(2) - pnorm(-2)      # it turns out around 95%

# make a random sample with a size of 1000 from normal distribution with mean = 50 and sd = 10
data <- rnorm(n=1000, mean = 50, sd = 10)
zscore <- (data - 50) / 10
zscore
