# Chapter 3 - Central tendency
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
data <- c(2, 3, 4, 4, 5, 7, 8, 11, 12, 14, 18, 21, 21, 21, 26, 29, 29, 34, 34, 37, 44)
# mean, median, and mode
mean(data)
median(data)
# mode is a little difficult to write a code, the following code means finding the index of a maximum point in the frequency table, and finding a correspondence variable.
unique(data)[which.max(table(data))]
# mode(data) does not show the mode of data
mode(data)
