#For today's R script, we will learn how to construct confidence intervals
#Let's start with an example

#Suppose we are fishermen in Eastern Ontario in a fishing derby (google is your friend), catching bullhead (google is your friend). We collected 222 bullhead fish (yes, this is excessive!) and found that the the mean length was 329.6mm with a standard deviation of 24.5. What is the 95% confidence interval? 

#using the formula covered during lecture, we find the margin of error (m) as follows: 
1.96*24.5/sqrt(222)

#lower confidence interval
329.6 - 1.96*24.5/sqrt(222)

#upper confidence interval
329.6 + 1.96*24.5/sqrt(222)

#we can report this answer in multiple ways:
#1) Margin of error format: 329.6 ± 3.2 mm
#2) Interval format: (326.4, 332.8) mm

#Another way of determining confidence interval is using the t-test function

#let's start by loading the bullhead data
#bullhead <- read.csv("http://tiny.cc/pubh2w") #fun fact: you can read csv data from urls!!!!
setwd("D:/bilal's books 8/Lie Detector/Lab assignments/Lab_12")
bullhead <- read.csv("data_pib.csv")
#using the t.test function
t.test(bullhead$length)

#what is we want to find the 90% confidence interval?
#we can do this by: 
t.test(bullhead$age,conf.level = .90)

#Find the 99% confidence interval using the t.test function
t.test(bullhead$age,conf.level = .99)

#Please note that the t.test does not use Z values to calculate confidence intervals. 
#It uses the t-distribution instead
#However, the t-distribution is more sensitive to sample size than the Z distribution (represents properties for Z values)
#So if we have small sample sizes, we need to check for outliers

#Here is an example
x <- c(12,14,16,19,21)
boxplot(x,horizontal = TRUE) #creating a horizontal boxplot
#no outliers

t.test(x)

#what happens when there are outliers?
y <- c(12,14,16,19,31) #31 is an outlier.

#The mean fluctuates(increases) because of the outlier. 
#Since the sample size is small therefore the change also seems quite large.
#It inturn changes the confidence interval values. 

#What is the definition of outliers that boxplots use? 
boxplot(y,horizontal = TRUE)
t.test(y)
#An outlier is defined as a data point that is located outside the 
#1.5*Inter Quartile Range

#Although we have not covered this during lecture
#I think it is useful to learn how to calculate confidence intervals for proportions or percentages
#From a sample of 150 students visiting the Health and Wellness center, 83 had obtained a flu shot. Find a
#90% confidence interval for the percentage of students who have received a flu shot.
p.hat <- 83/150
p.hat

#look up the general formula online
#p.hat +- z*sqrt(p.hat*(1-p.hat)/n)


#lower interval
p.hat - 1.645*sqrt(p.hat*(1-p.hat)/150)

#upper interval
p.hat + 1.645*sqrt(p.hat*(1-p.hat)/150)

#another way is using a built-in R command
prop.test(83,150)
#like before, default is 95% confidence interval

#what if we want to determine 90% confidence interval
prop.test(83,150,conf.level=.90)

#Place the 90% and 95% confidence intervals in context (hint: lecture slides from today's lecture)
#The statement seems a bit ambiguous as if what do we have to actually do 
#but nevertheless I am answering as per what I have understood

#To interpret these results within the context of the problem, we can say that with 
#95% confidence the percentage of the times we can expect to find  a student who has been given a flu shot
#is somewhere between 47.01% and 63.3%, based on our sample.
#In the case of 90% confidence the percentage of the times we can expect to find  a student who has been given a flu shot
#is somewhere between 48.28% and 62.17%, based on our sample.

#In the 95% interval 95% of values fall within two standard deviations of the mean proportion
#In the 90% interval 90% of values fall within 1.645 standard deviations of the mean proportion

#The greater the confidence level the wider the intervals would be. 
#99% confidence intervals are wider than 95% intervals, and 90% intervals are narrower.


#we can use $ to pull out confidence interval from output
prop.test(83,150,conf.level=.90)$conf.int

#Lab completion

#Question 1: As you may know, Pakistan has a serious water pollution issue. 
#A sample of 67 water samples was taken from across the United States. 
#The mean arsenic (google is your friend) level in the 67 samples was 9.6 parts per billion (ppb) with a standard deviation of 3.4 ppb. 
#Construct the 95% confidence interval for the mean arsenic level.
(9.6)+1.96*3.4/sqrt(67)
(9.6)-1.96*3.4/sqrt(67)
#Question 2: 
#A sample of 350 elm trees was taken in Islamabad and 95 of the trees showed evidence of Dutch elm disease (google is your friend).
#Construct the 90% confidence interval for the proportion of trees in Islamabad with evidence of Dutch elm disease.
prop.test(95,350,conf.level=.90)
