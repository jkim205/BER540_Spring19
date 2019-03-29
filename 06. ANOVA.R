# ANOVA : ANalysis Of Variance
# 

# F distribution 
sample <- seq(0, 10, length.out = 100)
density <- df(sample, df1 = 4, df2= 10)
plot(sample, density, type = 'l')

# ANOVA
length <- c(3.4, 3.2, 3.0, 3.0, 3.5, 3.8, 3.6, 4.0, 3.9, 2.9, 3.5, 3.6, 2.7, 3.5, 3.8, 2.9, 3.4, 3.2, 3.3, 3.1, 2.9, 3.0, 2.6, 3.3, 3.7, 2.7, 2.4, 2.5, 3.3, 3.4)
group <- c(rep("Low", 10), rep("Medium", 10), rep("High", 10))

ex12_1 <- data.frame(length = length, group = group)

res.aov <- aov(length ~ group, data = ex12_1)
summary(res.aov)
boxplot(length ~ group, data = ex12_1)

# Tukey's test
TukeyHSD(res.aov)

# ANOVA Exercise

Groups <- c(55, 43, 100, 105, 95, 93, 50, 83, 86, 59, 60, 110, 100, 92, 108, 84, 75, 60)
Indivs <- c(87, 95, 48, 32, 103, 99, 108, 110, 112, 100, 66, 60, 76, 60, 65, 85, 97, 91)
Nones  <- c(76, 59, 43, 46, 60, 88, 90, 30, 65, 92, 34, 56, 45, 82, 94, 80, 50, 40)

HW_data <- data.frame(time = c(Groups, Indivs, Nones), Type = c(rep("Group",18), rep("Individual", 18), rep("None", 18)))

res.aov <- aov(time ~ Type, data = HW_data)
summary(res.aov)
boxplot(time ~ Type, data = HW_data)

# Tukey's test
TukeyHSD(res.aov)

