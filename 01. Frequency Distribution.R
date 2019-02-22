# File 01. Frequency distributions in Tables and Graphs
# Chapter 2 of Statistics for the Behavioral Sciences 2nd edition by Privitera
# made by JJ Kim Feb 15th 2019

# table 2.1 data p. 32
data = c(30, 70, 7, 47, 13, 60, 0, 91, 33, 44, 40, 9, 67, 55, 65, 12, 140, 77, 49, 77, 110, 98, 21, 22, 44, 33, 44, 18, 10, 33, 30, 20, 110, 109, 54, 17, 55, 16, 90, 12, 175, 44, 33, 33, 7, 82)

# range(data) will show the range of the data: the minimum and maximum value.
range(data)

# Frequency distribution into grouped data 
n <- 7                                  # the number of intervals
grouped_data <- cut(data, breaks=n)     # cut data into n groups 
freq <- table(grouped_data)             # show the frequency distribution
rownames(freq) <- c(25, 50, 75, 100, 125, 150, 175) # set the column name
cum <- cumsum(freq)                     # cumulative frequency distribution
rel <- freq/sum(freq)                   # relative frequency
rel_per <- rel * 100                    # relative percent
cum_rel <- cum/sum(freq)                # cumulative relative frequency
cum_rel_per <- cum_rel * 100            # cumulative relative percent

# shows the frequency table by binding each columns  
f_table <- cbind(freq, cum, rel, rel_per, cum_rel, cum_rel_per)

# change column names of the table
colnames(f_table) <- c("Frequency", "Cumulative Frequency", "Relative Frequency", "Relative Percent", "Cumulative Relative Frequency", "Cumulative Relative Percent")
# show the frequency table
f_table

# histogram of table
hist(data)
hist(data, breaks = 5)
hist(data, breaks = 10, freq = FALSE) # it shows the density
hist(data, breaks = 10, col = "red") # setting a color

# Frequency polygons
plot(freq, type = 'o', col = 'blue', xlab = "Group", ylab = "Frequency")
# Ogive
plot(cum_rel_per, type = 'l', col = 'green', xlab = "Group", ylab = "Cumulative %")
# Stem-and-leaf display
stem(data)

# Bar chart
barplot(freq)
# Pie chart
pie(freq, clockwise = TRUE)

# Optional - Data Frame in R
# if you make this table in the form of data frame, then you can easily access to each column
df <- data.frame(f_table)
# access to the column using the column name.
df$Frequency
df$Cumulative.Frequency
# row.names show the row name of data frame
row.names(df)
