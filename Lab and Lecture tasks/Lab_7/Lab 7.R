#In this session, we will explore different probability distributions
#This tutorial was adapted on R-bloggers session on probability distributions
#Please note that for this week's assignment, you are expected to answer questions in this script
#Please submit your answers as a .doc or .pdf file with the corresponding answers or plots. 
#Please make sure to submit your R file as well! 
#Those who do not submit an R file will receive a grade of 0 in the assignment

#Today we will cover three distributions in R
#Binomial
#Poisson 
#Normal

#Functions dealing with probability distributions in R 
#have a single-letter prefix that defines the type of function we want to use
#pre-fixes are d, p, q, and r
#d refers to density or mass  
#c refers to cumulative (graphical depiction of probability for each outcome, cumulatively added to the next outcome)
#q refers to quantile distribution funcion. Calculates the inverse cumulative density function
#r refers to random sampling

#We will combine these prefixes with the names of the distributions we are interested in, # These are binom (Bernoulli and Binomial), pois (Poisson) and norm (Normal).

#Let's start with the bernoulli distribution
#20 coin flips example using p = 0.7 (probability of heads )
#let's plot the mass function of X.
#How do we do this? 
#We first generate a vector with the sequence of numbers 1,2,…20 and iterate the function over these values.


n <- 1:20 #setting a vector with numbers 1 to 20
den <- dbinom(n, 20, 0.7) #assigning a new variable called den
den

#?dbinom to get details on the syntax for this function
plot(den, ylab = "Density", xlab = "Number of successes")
sum(den) # should be equal to 1 (one of the three axioms of probability theory)

#looking at the plot, determine what the probaility maximum is? 
#around 0.19

#Let's try another example
#Suppose widgits produced at Acme Widgit Works have probability 0.005 of being defective. #Suppose widgits are shipped in cartons containing 25 widgits. What is the probability that a randomly chosen carton contains exactly one defective widgit? 
#rephrased: What is the probability of one defective widget given that there are 25 widgits in a carton and the failure rates is 0.005
#Answer
dbinom(1, 25, 0.005)

#Recall that pbinom is the R function that calculates the cumulative density function of the binomial distribution.
vec <- 1:100
d <- pbinom(vec, size=100, prob=0.25)
plot(d)
#what is this function looking up?
#The probability of 27th value being a success

#Question 1: plot a vector of 100 numbers that are drawn from a probability mass function of a binomial distribution with a 0.5 probability of success
n1 <- 1:100
first <- dbinom(n1,100,0.5)
plot(first, ylab = "Density", xlab = "Number of successes")

#The plot is symmetric so we can say the probability of 
#50 successes in 100 trials is highest


#1a)what happens when the probability is 0.01, 0.1, and 0.89?
second <-dbinom(n1,100,0.01)
plot(second, ylab = "Density", xlab = "Number of trials")
#The probability of getting 1-3 successes would be higher where as getting 10 or more successes would be 0

third <- dbinom(n1,100,0.1)
plot(third, ylab = "Density", xlab = "Number of trials")
#The probability of getting 1-20 successes would be higher where as getting more than 22 successes would be 0

fourth <- dbinom(n1,100,0.89)
plot(fourth, ylab = "Density", xlab = "Number of trials")
#The probability of getting 80-90 successes would be higher in a 100 trials 

#As the probability increases the number of successes also increases

#1b)what happens when the size is increased to 1000, 10000, 100000?
one <- dbinom(1:1000,1000,0.5)
plot(one, ylab = "Density", xlab = "Number of trials")


two <- dbinom(1:10000,10000,0.5)
plot(two, ylab = "Density", xlab = "Number of trials")


three <- dbinom(1:100000,100000,0.5)
plot(three, ylab = "Density", xlab = "Number of trials")


#As the size increases the distributions peak at the same fractional distance 
#from the origin, N/2. The peak in the distribution gets sharper and the width 
#of the curve also reduces in other words the standard deviation reduces. 
#Reference: http://www.pas.rochester.edu/~stte/phy104-F00/notes-5.html

# @@ RGEDIT LANDMARK @@: Normal distribution
#example

#The daily revenue of a local store follows a normal distribution with a mean of $1000 and variation of $200
#what is the probability that the revenue today will be at least $1200?
pnorm(1200,1000,200) # this gives us prob x smaller than $1200
1-pnorm(1200,1000,200) # this is the one, x greater than $1200

#given a mean of $1000 and the variation of $200,

#Question 2
#Suppose widgit weights produced at Acme Widgit Works have weights that are normally distributed with mean 17.46 grams and variation 375.67 grams. 

#2a)What is the probability that a randomly chosen widgit weighs more then 19 grams?
# Hint: What is P(X > 19) when X has the N(17.46, 375.67) distribution? 
# Note: R wants the s. d. as the parameter, not the variance.
pnorm(19,17.46,375.67) # this gives us prob x smaller than 19
1-pnorm(19,17.46,375.67) # this is the one, x greater than 19
#The probability is 0.4983646
#2b) Please plot the probabilities of outcomes for 100 values between 2 and 200
val <- 1:101
prob <- dnorm(val,17.46,375.67)
plot(prob, ylab = "Density", xlab = "Number of trials between 100 and 200" )

#we can use the rnorm function to randomly sample a set of 100 values from a normal distribution with a specified mean and variance (sd) 
z<-rnorm(1000, mean = 10, sd = 4) #assigning the random draws to a variable z

#let's plot this
hist(z, probability = TRUE, col = 'cyan', main='Histogram of 100 draws',xlab='Weights')

#2c) Increase the number of random draws from this distribution to 10000 and 100000.
#What does this distribution look like? In which interval does most of the data lie in?

z1<-rnorm(10000, mean = 10, sd = 4) #assigning the random draws to a variable z1
hist(z1, probability = TRUE, col = "red",main='Histogram of 10000 draws',xlab='weights')


z2<-rnorm(100000, mean = 10, sd = 4) #assigning the random draws to a variable z1
hist(z2, probability = TRUE, col = "orange",main='Histogram of 100000 draws',xlab='Weights')

#The width of the bars or bin size has decreased in 10000 and the width of 
#the bars in case of 100000 has further decreased 
#while the standard deviation is fixed. This is due to the fact as the number of 
#draws increase more and more data gets packed into each interval. The distribution is a bell curve. 
#Most of the data lies in the 8-12 interval.

#let's pick 100 outcomes from 1000 random draws
xx <- seq(min(z), max(z), length=100)
lines(xx, dnorm(xx, mean=10, sd=4)) #this draws a line on the histogram


#Poisson distribution
#let's start with a probability mass function with a rate parameter of 3
n <- 1:100 #setting a vector with numbers 1-100 in ascending order
den <- dpois(n, 3) #using the dpois function to draw 100 values from a poisson distribution with a rate parameter of 3
plot(den, xlab = "Outcome", ylab = "Density",col='red')

#Question 3: 
#3a)What happens to the shape of the distribution when the rate parameter is 0.3?
den <- dpois(n, 0.3) #using the dpois function to draw 100 values from a poisson distribution with a rate parameter of 3
plot(den, xlab = "Outcome", ylab = "Density",col='blue')

#The probability of getting 0.3 - 1 event in an interval would be the greatest and 
#the probability of getting events greater than 5 would be close to 0  

#3b) What happens to the shape of the distribution when the rate parameter is 10?
den <- dpois(n, 10) #using the dpois function to draw 100 values from a poisson distribution with a rate parameter of 3
plot(den, xlab = "Outcome", ylab = "Density",col='orange')

#The probability of getting 0 - 20 events in an interval would be the greatest and 
#the probability of getting events greater than 20 would be close to 0

#3c) What happens to the shape of the distribution when the rate parameter is 100?
den <- dpois(n, 100) #using the dpois function to draw 100 values from a poisson distribution with a rate parameter of 3
plot(den, xlab = "Outcome", ylab = "Density",col='blue')

#The probability of getting more than 80 events in an interval would be the greatest and 
#the probability of getting events less than 70 would be close to 0

#When we are changing the rate parameter the mean and variance of the distribution is changing  
#So as we increase the rate parameter the probability of observing more events would increase 
#and if we decrease the rate parameter then the probability of observing less events is higher

#3d) Where are the mass of all points on the distribution?
#make sure to include plots for all three questions

#The mass would be near the rate parameter:

#part (a)
#The probability of getting 0.3 - 1 event in an interval would be the greatest hence most of our mass is concentrated in this region

#part (b)
#The probability of getting 0 - 20 events in an interval would be the greatest hence most of our mass is concentrated in this region

#part (c)
#The probability of getting 80 events or more in an interval would be the greatest hence most of our mass is concentrated in this region

#Question 4
#using the rpois function, extract 1000 random vales from a poisson distribution with a rate parameter of 5
#produce a histogram of these values
#make sure to add a line showing the shape of the distribution
val <- 1:1000
rp<-rpois(val, 5) #assigning the random draws to a variable rp
hist(rp, probability = TRUE, col = "orange")

xx <- seq(min(rp), max(rp), length=100)
xx <- round(xx)
lines(xx, dpois(xx,5)) #this draws a line on the histogram

#Let's generate means from a poisson distribution
myMeans <- vector()
for(i in 1:100){
   set.seed(i)
   myMeans <- c(myMeans, mean(rpois(10,3)))
}

#creating a histogram of the means
hist(myMeans, main = NULL, xlab = expression(bar(x)),col='green')
hist(myMeans, main = NULL, xlab = expression(bar(x)),col='green',breaks=10)
#Question 4
#4a)What does this distribution look like?
#The distribution looks like a poisson distribution

#4b) What happens to the shape of the distribution if we draw 10000 points instead of 100?
myMeans1 <- vector()
for(i in 1:10000){
  set.seed(i)
  myMeans1 <- c(myMeans1, mean(rpois(10,3)))
}
hist(myMeans1, main = NULL, xlab = expression(bar(x)),col='grey')

#The shape of the distribution becomes bell shaped in other words in this case we get a normal distribution 

#Question 5
#5a)Based on your sampling survey, which of the distributions that we learned in class are relevant to the kind of data that you have collected?
setwd("D:/bilal's books 8/Lie Detector/Lab assignments/Lab_7")
data<-read.csv("datawithcategories.csv", header = TRUE)
data
library(plyr)
tot <- ddply(data,.(Day,Category),nrow)
tot
hist(tot$V1, main="", probability = TRUE, col = "orange", breaks = 10)


#5b)Determine the mean, sd, probability of successes and/or rate parameter (depending on the type of data collected) for your data. Use this information to draw 10000 random variables from the relevant probability distribution. Plot these random draws as a histogram
mean(tot$V1)
sd(tot$V1)

#install.packages("fitdistrplus")

library(fitdistrplus)

rate <- MASS::fitdistr(tot$V1,"Poisson")
rate

#The rate is  6.1764706

val <- 1:10000
rp<-rpois(val, 6.1764706) #assigning the random draws to a variable rp
hist(rp, probability = TRUE, col = "orange",xlab='outcomes',main='Histogram of Poisson')

#5c) Based on your distribution, where do the center/mass of outcomes lie? 
#The center or the mass is around the rate parameter or one could say between the intervals 4-7

#5d) What happens to the shape of your distribution when you change your mean/sd/rate parameter/probability of successes to twice that you observed in your data? 
val <- 1:10000
rp<-rpois(val, 6.1764706*2) #assigning the random draws to a variable rp
hist(rp, probability = TRUE, col = "brown", main="Poisson distribution",xlab="outcomes")

#By changing the rate parameter to twice(12.35294) the shape of the distribution becomes more symmetric and it looks more like a bell curve with some skewness to the right

#5e) What happens to the shape of your distribution when you change your mean/sd/rate parameter/probability of successes to half that you observed in your data? 
val <- 1:10000
rp<-rpois(val, 6.1764706*0.5) #assigning the random draws to a variable rp
hist(rp, probability = TRUE, col = "purple",main="Poisson distribution",xlab='outcomes')

#The distribution is more skewed to the right and is still a poisson distribution. The mass is concentrated around the rate parameter which is 3.088235

#Note: For question #5, please repeat for each variable collected. 

#Additional note: It may be possible that your variable(s) of interest from your sampling #survey do not fit any of the distributions used in this script or mentioned in lecture. #If this is the case, please use this website to determine the probability distribution #that best fits the kind of data that you have
#website: https://www.johndcook.com/blog/distributions_r_splus/

#please note that you will need to look up these distributions online and in R help to determine which best fit your data. 




















