# if you did not install the following package (ggplot2, dplyr), please install. 
# install.packages("ggplot2")
# install.packages("dplyr")
library(ggplot2)
library(dplyr)

# correlation
x <- c(1, 2, 3, 4, 5)
y <- c(1, 2, 3, 4, 5)

df <- data.frame(x, y)
ggplot(df, aes(x, y)) + 
  geom_point()

cor.test(x,y)

x <- c(1, 4, 8, 5)
y <- c(2, 3, 9, 8)

df <- data.frame(x, y)
ggplot(df, aes(x, y)) + 
  geom_point()

cor.test(x,y)

################ Pearson Coefficient ###################
# SPSS in focus 15.4 
mood <- c(6, 4, 7, 4, 2, 5, 3, 1)
eating <- c(480, 490, 500, 590, 600, 400, 545, 650)

df <- data.frame(mood, eating)
mx <- mean(df$mood)
my <- mean(df$eating)

# create a table 15.1 
df <- df %>% mutate(x_mx = mood - mx, y_my = eating - my, xy = x_mx*y_my, x2 = x_mx^2, y2 = y_my^2)

# manually calculate r value
ssxy <- sum(df$xy)
ssx <- sum(df$x2)
ssy <- sum(df$y2)
r <- ssxy / sqrt(ssx * ssy)

# use correlation test function
cor.test(df$mood, df$eating)
cor(df$mood, df$eating)

# linear regression prediction model
coef(lm(eating ~ mood, data = df))

# plot 
p <- ggplot(df, aes(x=mood, y=eating)) +
  geom_point() +
  geom_abline(intercept = 651.16071, slope = -29.82143)

################ Spearman Coefficient ###################
# example 15.2 
food <- c(1.5, 1.5, 3, 4, 5, 6, 7, 8)
water <- c(1, 3, 2, 6, 4, 7, 8, 5)
df <- data.frame(food, water, D = food-water)
df <- df %>% mutate(D2 = D^2)

ssD2 <- sum(df$D2)
n <- length(df[,1])
rs <- 1- 6 * ssD2 / n / (n^2 -1)

# methods
cor.test(df$food, df$water, method = "spearman")

################ Point-biserial correlation coefficient ###################
# input the data
sex <- c(rep(1, 5), rep(2, 7))
dur_laugh <- c(23, 9, 12, 12, 29, 32, 10, 8, 20, 12, 24, 34)

# build a data frame
df <- data.frame(sex, dur_laugh)

# select male only
male <- df$sex == 1
My1 <- mean(df[male,]$dur_laugh)

# select female only
female <- df$sex == 2
My2 <- mean(df[female,]$dur_laugh)

# calculate a total mean
My <- mean(df$dur_laugh)

# create table 15.4
df <- df %>% mutate( y_my = dur_laugh-My, y_my2 = y_my^2)

# manually calculate point-biserial coefficient
ssy <- sum(df$y_my2)
n <- length(df$dur_laugh)
n1 <- length(df[male,]$dur_laugh)
n2 <- length(df[female,]$dur_laugh)
  
sy <- sqrt(ssy/n)
rpb <- (My1-My2)/sy * sqrt(n1/n * n2/n)
rpb

# point-biserial coefficient function
install.packages("ltm")
library(ltm)
biserial.cor(df$dur_laugh, df$sex)

#### Phi correlation coefficient ####
install.packages("psych")
library(psych)
empl <- c(rep(0, 14), rep(1, 6), rep(0, 6), rep(1, 14))
happ <- c(rep(0, 14), rep(0, 6), rep(1, 6), rep(1, 14)) 

df <- data.frame(empl, happ)
table(df)

phi(table(df))
