#Today we will play around with sampling distributions

#1) Comparing sample versus population proportions

#In the last Pakistani federal elections, exit polls showed that the probability of PTI winning office was 70%. Suppose you sampled 30 voters from different polls (as they were leaving) across the country. 
#Using the rbinom function that you learned in your last R script, please answer the following questions: 

##Note: I am setting the seeds so that the results are reproduceable

#1a) What was the sample proportion that you obtained if you draw one sample using the rbinom function? 
set.seed(100)
c <- rbinom(30,1,0.7)
c
sum(c)/30
#The sample proportion is 0.7666667

#1b) Draw 300 sample proportions using the rbinom function. Make a histogram of these sample proportions. 
#Based on our lecture yesterday, what is this histogram called?
set.seed(100)
val <- rbinom(300,30,0.7)
val
p <- val/30
hist(p, probability = TRUE, col = 'cyan', main='Histogram of 300 samples',xlab='polls')
#The histogram is called sampling distribution of a sample proportion.
#The distribution looks symmetric.

#1c) Based on the histogram above, describe the behaviour of the sampling distribution (hint: three features that we covered in our lecture yesterday)

#The distribution looks symmetric.

mean(p)
#The center is near 0.697

#We calculate the spread of the distribution by using the proportion spread formula.
#The population proportion is 0.7 and the sample size is 30
sqrt(0.7*(1-0.7)/30)
#The spread is   0.083666

#1d) What happens to the distribution if you change the sample size from 30 to 100 voters per sample? Make sure to discuss all three features
set.seed(200)
val1 <- rbinom(300,100,0.7)
val1
p1 <- val1/100
hist(p1, probability = TRUE, col = 'cyan', main='Histogram of 300 samples',xlab='polls')

#The shape is normal

mean(p1)
#The center is near 0.697

#We calculate the spread of the distribution by using the proportion spread formula.
#The population proportion is 0.7 and the sample size is 100
sqrt(0.7*(1-0.7)/100)
#The spread is  0.04582576
#For larger sample size the spread is lower

#1e) What happens to the distribution if you change the sample size from 100 to 10 voters per sample? Make sure to discusss all three features
set.seed(100)
val2 <- rbinom(300,10,0.7)
val2
p2 <- val2/10
p2
hist(p2, probability = TRUE, col = 'cyan', main='Histogram of 300 samples',xlab='pols',breaks=10)

#The shape still seems symmetric but the distribution has gaps in it. 
mean(p2)
#The center is near 0.697

#We calculate the spread of the distribution by using the proportion spread formula.
#The population proportion is 0.7 and the sample size is 10
sqrt(0.7*(1-0.7)/10)
#The spread is  0.1449138
#The spread seems to be larger which is right because as 
#sample size decreases the spread increases.

#1f) What happens to the distribution if you change the sample size from 10 to 5 voters per sample? Make sure to discuss all three features
set.seed(100)
val3 <- rbinom(300,5,0.7)
val3
p3 <- val3/5
p3
hist(p3, probability = TRUE, col = 'cyan', main='Histogram of 300 samples',xlab='pols',breaks=10)

#The shape is symmetric but there are gaps in the histogram. 
mean(p3)
#The center is near 0.6946667.

#We calculate the spread of the distribution by using the proportion spread formula.
#The population proportion is 0.7 and the sample size is 5
sqrt(0.7*(1-0.7)/5)
#The spread is  0.204939
#The spread seems to be larger which is right because as 
#sample size decreases the spread increases.


#Moreover in all the parts we saw the center more or less remained the same

#2)Suppose you are a dendochronologist (google is your friend), who is measuring the distance between tree rings in order to determine what the moisture availability trees in the coniferous forests of Gilgit Baltistan receive. From previous studies you know that the mean tree ring distance for a coniferous tree is 5cm, with a standard deviation of 0.5cm. Using the rnorm function from your last script, please answer the following questions: 

#2a) What is your sampling mean if you only sample a forest stand once and take cores from 45 trees
set.seed(100)
val4 <- rnorm(45, mean = 5, sd = 0.5)
mean(val4)

#The sample mean is 5.040361

#2b) What if you sample your forest stand 100 times, taking cores from 45 trees for each sample? Make a histogram of these sample means?
set.seed(100)
lst <- NULL
vec <- 1:100
for (i in vec){
  lst[i] <- mean(rnorm(45, mean = 5, sd = 0.5))
}
lst
hist(lst, col = 'orange', main='Histogram of 100 samples from 45 trees',xlab='means',breaks=10)


#2c)  Based on the histogram above, describe the behaviour of the sampling distribution (hint: three features that we covered in our lecture yesterday). 

#The shape of the distribution looks symmetric. It seems like a normal distribution is being formed.
#which is true because the the population from which we are sampling is normal.

mean(lst)
#The center of the distribution is at 4.99.
sd(lst)/45
#To measure the spread we calculate the sample standard deviation and
#divide by the sample size
#The spread of the distribution is 0.001581983

#2d) What happens to the distribution if you change the sample size from coring 45 trees to 70 trees per sample (and you sample the forest stand 100 times). Make sure to describe all three features.
set.seed(100)
lst1 <- NULL
for (i in vec){
  lst1[i] <- mean(rnorm(70, mean = 5, sd = 0.5))
}
lst1
hist(lst1, col = 'red', main='Histogram of 100 samples of 70 trees',xlab='means')

#The shape of the distribution looks symmetric. It seems a normal distribution is being formed.
#which is true because the population from which we are sampling is normal.

mean(lst1)
#The center of the distribution is at 5.001
sd(lst1)/70
#To measure the spread we calculate the sample standard deviation and
#divide by the sample size
#The spread of the distribution is 0.0008704107
#The spread is even lower because larger the sample size 
#the lower will be the spread.

#2e) What happens to the distribution if you change the sample size from coring 70 trees to 30 trees per sample (and you sample the forest stand 100 times). Make sure to describe all three features.
set.seed(100)
lst2 <- NULL
for (i in vec){
  lst2[i] <- mean(rnorm(30, mean = 5, sd = 0.5))
}
lst2
hist(lst2, col = 'orange', main='Histogram of 100 samples from 30 trees',xlab='means')

#The shape of the distribution looks symmetric. It seems a normal distribution is being formed.
#Which is true because the population from which we are sampling is normal.
mean(lst2)
#The center of the distribution is at 5.001

sd(lst2)/30
#To measure the spread we calculate the sample standard deviation and
#divide by the sample size
#The spread of the distribution is  0.002790068
#The spread is slightly larger


#2f) What happens to the distribution if you change the sample size from coring 30 trees to 10 trees per sample (and you sample the forest stand 100 times). Make sure to describe all three features. 
set.seed(100)
lst3 <- NULL
for (i in vec){
  lst3[i] <- mean(rnorm(10, mean = 5, sd = 0.5))
}
lst3
hist(lst3, col = 'orange', main='Histogram of 100 samples of 10 trees',xlab='means')
#The shape of the distribution looks symmetric. It seems a normal distribution is being formed.
#Which is true because the population from which we are sampling is normal.


#The center of the distribution is at 5
sd(lst3)/10
#To measure the spread we calculate the sample standard deviation and
#divide by the sample size
#The spread of the distribution is  0.01550903
#The spread is larger

#3) From your sampling survey, pick data that is either normally or binomially distributed. If you do not have normally or binomially distributed data, convert your data of interest so that they fit these distributions. For e.g. instead of number of orders per type, you can do proportions of total orders that were type 1. Assume that the mean/proportion for your sampling survey is the same as the population mean/proportion. Using the rbinom or rnorm function, please create a sampling distribution for the following sample sizes: 

setwd("D:/bilal's books 8/Lie Detector/Lab assignments/Lab_11")
data <- read.csv('datawithcategories.csv')
data

bothCategory <- nrow(data[data$Category=="WithSpiceSauce",])
bothCategory
totalOrders <- nrow(data)
totalOrders
prop <- bothCategory/totalOrders
prop
#the proportion of orders of both category were 0.6
#We make a sampling distribution of proportions

#a) Three times the sample size that you collected
set.seed(100)
h <- 3*totalOrders
h 
out <- rbinom(100,h,prop)
out
output <- out/(h)
output
hist(output, probability = TRUE, col = 'brown', main='Histogram of 100 samples',xlab='proportions')

#b) Half the sample sizes you collected
set.seed(100)
h1<- floor(totalOrders/2)
h1
out1 <- rbinom(100,h1,prop)
out1
output1 <- out1/h1
output1
hist(output1, probability = TRUE, col = 'cyan', main='Histogram of 100 samples',xlab='proportions')
#Please describe the behaviour of the sampling distribution in each scenario. 

#Part a:
# The shape of the sampling distribution of proportions looks symmetric 
mean(output)
#The center is at 0.599 
#The spread of the distribution is 0.02760262
sqrt(prop*(1-prop)/(h))

#Part b:
# The shape of the sampling distribution of proportions looks symmetric
mean(output1)
#The center is at  0.6013462 
#The spread of the distribution is 0.06793662
#The spread is larger as the sample size is smaller
sqrt(prop*(1-prop)/(h1))

