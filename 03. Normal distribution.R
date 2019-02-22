# Chapter 6 Normal Distribution

# plot the normal distribution
sd <- c(-3, -2, -1, 0, 1, 2, 3)
density <- dnorm(sd)
plot(sd, density, type = 'o')

# to be specific 
sd <- seq(-3, 3, length.out = 100)
density <- dnorm(sd)
plot(sd, density, type = 'l')
