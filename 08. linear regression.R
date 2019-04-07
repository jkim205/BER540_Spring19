# linear regression

# table 16.5
x <- c(9, 5, 8, 2, 6, 3, 5, 4)
y <- c(0, 3, 2, 5, 3, 4, 2, 3)

plot(x,y, xlab="number of sessions", ylab = "number of symptoms")
scatter.smooth(x,y)

model <- lm(y~x)
summary(model)

# multiple regression Example 16.4 
age <- c(19, 21, 26, 28, 32, 30)
education <- c(12, 14, 13, 18, 17, 16)
sales <- c(20, 40, 30, 68, 70, 60)

df <- data.frame(age, education, sales)

model <- lm(sales~ age + education, data = df)
summary(model)

b <- summary(model)$coef[2:3,1]

sy <- apply(model$model[1], 2, sd)
sx <- apply(model$model[2:3], 2, sd)

betas <- b * (sx/sy)
