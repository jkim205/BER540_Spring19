# Chapter 3 - Central tendency and Chapter 4 - Variability
# made by JJ Kim Feb. 21st 2019
#

# Table 3.1 
df <- data.frame(cbind(c("Lean", "Overweight", "Obese"), c(46, 58, 73), c(12, 14, 30)))
colnames(df) <- c("Sample", "M", "n")
# show the data frame
df
# show the structure of the data frame. each column is a factor type.
str(df)

# change the type of two columns from factor to numeric
df$M <- as.numeric(as.character(df$M))
df$n <- as.numeric(as.character(df$n))

# structure of df changed
str(df)

# it shows the correct weighted mean. If the type of each colum is a factor, then 46 are considered as 1, the factor number of an entry.
wtd_mean <- weighted.mean(as.numeric(df$M), as.numeric(df$n))
wtd_mean

# table 3.3
data3 <- c(2, 3, 4, 4, 5, 7, 8, 11, 12, 14, 18, 21, 21, 21, 26, 29, 29, 34, 34, 37, 44)
# mean, median, and mode
m <- mean(data3)
median(data3)
# mode is a little difficult to write a code, the following code means finding the index of a maximum point in the frequency table, and finding a correspondence variable.
unique(data3)[which.max(table(data3))]
# mode(data) does not show the mode of data
mode(data3)
# summary(data3) shows 1st, 3rd Quartile
summary(data3)

# When we consider the data as a sample, then we use n-1 to calculate the sample variance.
# But, if we consider the data as a population, then we use n to calculate the population variance.

# sum of squares in data3
dev <- data3 - m              # deviation of data
SS <- sum(dev^2) 
n <- length(data3)
# sample variance
SS/(n-1)
# sample variance of data
var(data3)

# population  
pop <- c(2, 5, 8)
len_pop <- 3
pop_var <- mean((pop-mean(pop))^2)

UsingN <- c()
UsingN_1 <- c()

# building the every sample possibilities to compare the case of using n and n-1 as a degree of freedom  
for (i in pop){
  for (j in pop){  
    sample <- c(i,j)
    N <- length(sample)
    sample_mean <- mean(sample)
    dev <- sample - sample_mean
    SS <- sum(dev^2)
    UsingN <- c(UsingN, SS/N)
    UsingN_1 <- c(UsingN_1, SS/(N-1))
  }
}

# estimated value of mean differ and using N-1 of a degree of freedom estimates the population variance correctly, which means an unbiased estimator.
mean(UsingN)
mean(UsingN_1)
identical(pop_var, mean(UsingN))
identical(pop_var, mean(UsingN_1))

# probabiity from -inf to 0 in normal distribution with m=0 and sd=1  
pnorm(0, mean=0, sd=1)
# this shows the percent from -1sd to 1sd in a standard normal distrubition
pnorm(1)- pnorm(-1)

rnorm(n=2) # random two numbers from normal distribution