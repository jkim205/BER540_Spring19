# T distribution vs. Normal distribution

x <- seq(-3, 3, length.out = 1000)
ndata <- dnorm(x, mean = 0, sd = 1)
tdata_5 <- dt(x, df = 5)
tdata_20 <- dt(x, df = 20)

plot(c(x,x,x), c(ndata, tdata_5, tdata_20), type = 'l', xlab = "standard deviation", ylab="density point")

# t table
crit_val = c(.25, .1, .05, .025, .01, .005)
for (i in seq(1,10)){
  a <- qt(crit_val, df=i, lower.tail = FALSE)
  tab <- rbind(tab, qt(crit_val, df=i, lower.tail = FALSE))
}
tab  

# table 9.3 
score <- c(20, 60, 48, 92, 50, 82, 48, 90, 30, 68, 43, 54, 60, 62, 94, 67, 63, 85)
t.test(score, mu= 77.43)

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
t_result <- t.test(ex1)

# effect size
cohens_d <- (mean(ex1)-pop_mean) /sd(ex1)
eta_squared <- (t_result$statistic ^2) / (t_result$statistic^2 + t_result$parameter) 
omega_squared <- (t_result$statistic ^2 - 1) / (t_result$statistic^2 + t_result$parameter) 

# to calculate cohensD you can use the library lsr
# install.packages("lsr") # install the package lsr if you have not installed yet

library(lsr)
cohensD(ex1, mu=0)

# two sample t-test
# input data
groups <- rep(c(1,2), each = 18)
nums <- c(1.3, 2.2, 3.5, 0.7, 2.3, 2.1, 4.0, 6.0, 2.3, 5.8, 6.8, 5.3, 8.4, 3.5, 0.4, 7.9, 8.2, 1.6, 6.8, 5.7, 4.9, 8.5, 9.2, 8.4, 6.7, 4.3, 1.3, 6.3, 8.8, 9.2, 5.7, 7.3, 2.6, 2.1, 6.0, 3.4)
ex2 <- data.frame(cbind(groups, nums))
str(ex2)
which(ex2$groups==1)
group1 <- ex2$nums[which(ex2$groups==1)]
group2 <- ex2$nums[which(ex2$groups==2)]

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
t_result <- t.test(group1, group2)  # it shows the t statistics, df, and p-value
t_result
str(t_result)

# effect size
eta_squared <- (t_result$statistic ^2) / (t_result$statistic^2 + t_result$parameter) 
omega_squared <- (t_result$statistic ^2 - 1) / (t_result$statistic^2 + t_result$parameter) 

# to calculate cohensD you can use the library lsr
# install.packages("lsr") # install the package lsr if you have not installed yet

library(lsr)
cohensD(group1, group2)

# The related-samples t Test or Paired t-test
# Table 10.7
present <- c(220, 245, 215, 260, 300, 280, 250, 310)
absent <- c(210, 220, 195, 265, 275, 290, 220, 285)

# summary of data in present condition
length(present)     # sample size
mean(present)       # sample mean
sd(present)         # standard deviation
sem <- sd(present) / sqrt(length(present))    # standard error
sem

# summary of data in absent condition
length(absent)     # sample size
mean(absent)       # sample mean
sd(absent)         # standard deviation
sem <- sd(absent) / sqrt(length(absent))    # standard error
sem

# use the difference between two conditions because it is related.
diff <- present - absent

mean(diff)       # sample mean
sd(diff)         # standard deviation
sem <- sd(diff) / sqrt(length(diff))    # standard error
sem


# t test
t.test(diff)

# Exercise 10.1
color <- c(7,8,4,6,4,8,6,7,5,6,4,6,5,8,9,10,6,4,2,8,6,4)
black <- c(4,4,6,4,6,2,3,3,6,9,5,7,5,3,0,0,8,6,6,4,2,1)

# summary of data in color condition
length(color)     # sample size
mean(color)       # sample mean
sd(color)         # standard deviation
sem <- sd(color) / sqrt(length(color))    # standard error
sem

# summary of data in black condition
length(black)     # sample size
mean(black)       # sample mean
sd(black)         # standard deviation
sem <- sd(black) / sqrt(length(black))    # standard error
sem

# use the difference between two conditions because it is related.
diff <- color - black

mean(diff)       # sample mean
sd(diff)         # standard deviation
sem <- sd(diff) / sqrt(length(diff))    # standard error
sem

# t test
t.test(diff)



