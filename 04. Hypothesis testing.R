# Chapter 8. Hypothesis testing 

# the level of significance is 0.05
# in the two-tailed test, each rejection region becomes 0.025
# in the one-tailed test, each rejection region becomes 0.05 

# assume the population follows the normal distribution of mean = 10, and sd = 3
# this is the null hypothesis to retain or reject the assumption
# If you want to show your intervention or treatment made a difference, your null hypothesis would be there is no difference between control and treatment groups.
# In this case, you would want to reject the null hypothesis.

# Null Hypothesis H0 = my sample follows the assumption of the original population
# the assumed population distribution is normal distribution with m=10 and sd= 3

mu <- 10
sd <- 2
s_size <- 16

f <- 0        # f = 0 means the sample follows thd distribution of the null hypothesis,
              # f = 1 means the sample does not follow the distribution of the null hypothesis

if (f==0){    # if it follows the null 

  # data are randomly selected from the distribution
  s_data <- rnorm(n=s_size, mean = mu, sd = sd)
  
  # critical points in a two-tailed test with a level of significance 0.05
  left <- qnorm(0.025, mean = mu, sd = sd/sqrt(s_size))
  right <- qnorm(0.975, mean = mu, sd = sd/sqrt(s_size))
  
  # sample mean
  s_mean <- mean(s_data)
  
  if (s_mean > left && s_mean < right){
      print("Retain null, null was correct")
  }else{
      print("Even though data comes from the null hypothesis, we reject null")
      print("Type I error: your data was so unlucky to represent the truth")
  }
  
  
}else{
  
  # data are randomly selected from the other distribution. I assumed the real distribution for the sample has mean of 12, not 10
  s_data <- rnorm(n=s_size, mean = mu + 1, sd = sd)

  # critical points in a two-tailed test with a level of significance 0.05
  left <- qnorm(0.025, mean = mu, sd = sd/sqrt(s_size))
  right <- qnorm(0.975, mean = mu, sd = sd/sqrt(s_size))
  
  # sample mean
  s_mean <- mean(s_data)
  
  if (s_mean > left && s_mean < right){
    print("Type II Error, even though null was incorrect, you cannot reject the null hypothesis")
  }else{
    print("Reject the null hypothesis, and take the alternative hypothesis, and you are good to write your paper.")
  }
  
}

