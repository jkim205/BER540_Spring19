# Session 1. Sample script for the tutorial session Mar. 21st

# what is the meaning of the code below? 
n <- 100
x <- seq(1, n)
sum(x)

array <- c("Youn", "Jeng", "Choi")
array
array <- c(array, "Jongjin", "Kim")
array

# table 6.2 in p185.
sat_data <- c(500, 750, 600, 900, 950, 880, 990, 560, 780, 800, 800, 450, 800, 680, 550, 600)

length(sat_data)              # sample size
range(sat_data)               # min and max of the sample
mu <- mean(sat_data)          # sample mean
summary(sat_data)             # or use a summary function
sigma <- sd(sat_data)         # standard deviation

sat_zscore <- (sat_data - mu) / sigma                     # calculate z score
zscore <- scale(sat_data)[,1]
identical(sat_zscore, scale(sat_data)[,1])

# normal distribution
pnorm(90, mean=100, sd=10)
qnorm(0.975)
rnorm(10, mean=5)

# t distribution 
pt(2, df=10)
qt(0.975, df=10)
rt(10, df=10)

# Exercise  1. One sample t-test
# one sample t-test
# input data
ex1 <- c(-3, 0, -2, 0, -2, -2, -3, -1, -1, -1, 0, 0, -2, -1, -1, 0, 0, -1, -1, -2, 0, -2, -3, -3, -3, -2, 2, -3, -3, 0, -2, 0, 0, -3, -3, -2)

# summary of data
length(ex1)     # sample size
mean(ex1)       # sample mean
var(ex1)        # sample variance
sd(ex1)         # standard deviation
sem <- sd(ex1) / sqrt(length(ex1))    # standard error

# population estimator
pop_mean <- 0

# t-test 
t.test(ex1)  # it shows the t statistics, df, and p-value
t.test(ex1, mu=-1) # when the null hypothesis is u = -1 
t_result <- t.test(ex1, alternative = "two.sided")

# effect size
cohens_d <- (mean(ex1)-pop_mean) /sd(ex1)
eta_squared <- (t_result$statistic ^2) / (t_result$statistic^2 + t_result$parameter) 
omega_squared <- (t_result$statistic ^2 - 1) / (t_result$statistic^2 + t_result$parameter) 

# Exercise 2. Two independent sample t-test
# two sample t-test
# input data
group1 <- c(1.3, 2.2, 3.5, 0.7, 2.3, 2.1, 4.0, 6.0, 2.3, 5.8, 6.8, 5.3, 8.4, 3.5, 0.4, 7.9, 8.2, 1.6)
group2 <- c(6.8, 5.7, 4.9, 8.5, 9.2, 8.4, 6.7, 4.3, 1.3, 6.3, 8.8, 9.2, 5.7, 7.3, 2.6, 2.1, 6.0, 3.4)

# summary of data
length(group1)     # sample size
mean(group1)       # sample mean
var(group1)        # sample variance
sd(group1)         # standard deviation
sem <- sd(group1) / sqrt(length(group1))    # standard error

# summary of data
length(group2)     # sample size
mean(group2)       # sample mean
var(group2)        # sample variance
sd(group2)         # standard deviation
sem <- sd(group2) / sqrt(length(group2))    # standard error

# two independent sample t-test 
t_result <- t.test(group1, group2, var.equal = TRUE)  # it shows the t statistics, df, and p-value
t_result

# effect size
sp <- sqrt((sd(group1)^2 + sd(group2)^2)/2)
cohens_d <- (mean(group1)-mean(group2)) / sp
eta_squared <- (t_result$statistic ^2) / (t_result$statistic^2 + t_result$parameter) 
omega_squared <- (t_result$statistic ^2 - 1) / (t_result$statistic^2 + t_result$parameter) 

# Exercise  3. Paired t-test
# Paired t-test
color <- c(7,8,4,6,4,8,6,7,5,6,4,6,5,8,9,10,6,4,2,8,6,4)
black <- c(4,4,6,4,6,2,3,3,6,9,5,7,5,3,0,0,8,6,6,4,2,1)

# summary of data in color condition
length(color)     # sample size
mean(color)       # sample mean
sd(color)         # standard deviation
sem <- sd(color) / sqrt(length(color))    # standard error

# summary of data in black condition
length(black)     # sample size
mean(black)       # sample mean
sd(black)         # standard deviation
sem <- sd(black) / sqrt(length(black))    # standard error

# use the difference between two conditions because it is related.
diff <- color - black

mean(diff)       # sample mean
sd(diff)         # standard deviation
sem <- sd(diff) / sqrt(length(diff))    # standard error
sem

# t test
t.test(diff)

# or you can use t.test option of paired test
t.test(color, black, paired = TRUE, val.equal = TRUE)
