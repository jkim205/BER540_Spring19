# Chapter 6 Normal Distribution and Chapter 7 Probability and Sampling distributions

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
mu <- 50
sigma <- 10
data <- rnorm(n=1000, mean = mu, sd = sigma)      # generate 1000 samples on the normal distribution
zscore <- (data - mu) / sigma                     # calculate z score
head(cbind(data,zscore))                          # show first 6 entries of data and z score

# histogram of 1000 samples
hist(data, breaks=30)
hist(zscore, breaks=30)

# table 6.2 in p185.

sat_data <- c(500, 750, 600, 900, 950, 880, 990, 560, 780, 800, 800, 450, 800, 680, 550, 600)

length(sat_data)              # sample size
range(sat_data)               # min and max of the sample
mu <- mean(sat_data)          # sample mean
summary(sat_data)             # or use a summary function
sigma <- sd(sat_data)         # standard deviation

sat_zscore <- (sat_data - mu) / sigma                     # calculate z score
cbind(sat_data, sat_zscore)

zscore <- scale(sat_data)               # scale function automatically calculate zscore
attr(zscore, "scaled:center")           # the scaled data stores the mean and the standard deviation of the original data.
attr(zscore, "scaled:scale")
