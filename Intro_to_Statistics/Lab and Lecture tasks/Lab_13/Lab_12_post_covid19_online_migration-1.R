# For today's lab, we will learn how to do Null Hypothesis Significance Tests in R using different statistical techniques for different types of data
#Let's start by setting our working directory and loading relevant libraries
setwd("D:/bilal's books 8/Lie Detector/Lab assignments/Lab_13")
#If these packages are not installed on your machine, please use the following code
#install.packages("package_name", dependencies = TRUE)
library(ggplot2)        # plotting & data
library(dplyr)          # data manipulation
library(tidyr)          # data re-shaping
library(magrittr)       # pipe operator
library(gridExtra)      # provides side-by-side plotting

#we will be using the midwest dataset that is built into the ggplot2 package
head(midwest)
str(midwest)
unique(midwest$inmetro)
#1) How many variables does this dataset have? How many variables are categorical and how many variables are continuous?
#The dataset has 28 variables
#5 variables (county, state,inmetro, category) are categorical 
#Out of these 5 variables 1 variable is the unique identifier (which is also categorical)
#23 variables are continuous 

#Let's learn how to do a t-test
#There are multiple kinds of t-tests. Here is how to do all of them in R

#One sample t-test
#The one-sample t-test compares a sample’s mean with a known value, when the variance of the population is unknown. 
#Suppose we want to assess the percent of college educated adults in midwest US and compare it to a certain value. 
#For example, let’s assume the nation-wide average of college educated adults is 32% (Bachelor’s degree or higher) 
#we want to see if the midwest mean is significantly different than the national average; in particular we want to test if the midwest average is less than the national average.

#2) What is our null hypothesis? 
#There is no significant difference between the national average and midwest average

head(midwest$percollege, 10) #let's look at the first 10 lines

summary(midwest$percollege) #let's get a sense of the measures of center and spread

#3) What is the center of the data (use two measures of center)? What is the spread of the data (use two measures of spread)?

#Measures of Center
#The mean of the data 18.273
#The median of the data is 16.798

#Measures of Spread
sd(midwest$percollege)

max(midwest$percollege)-min(midwest$percollege)
#The standard deviation of the data is 6.261908
#The range of the data is 40.7424

#Prior to conducting a t-test, we need to see if our data meet the assumptions
#Here are the assumptions of a t-test
#1) scale of measurement applied to the data collected follows a continuous or ordinal scale
#2) simple random sample, data is collected from a representative, randomly selected portion of the total population.
#3)Data, when plotted, results in a normal distribution, bell-shaped distribution curve.
#4)Reasonably large sample size
#5) Homogeneity of variance. Homogeneous, or equal, variance exists when the standard deviations of samples are approximately equal

hist(midwest$percollege) #data is not normally distributed

#let's transform the data into a normal distribution by taking the natural logarithm of the values
#However, as with any transformation, please keep in mind that this will affect the way we interpret the results from our statistical analysis 
#i.e. results from our analyses will pertain to the natural logarithm of our results rather than the results themselves
zl<- log(midwest$percollege)
hist(zl) #looks normally distributed!

#To test if the midwest average is less than the national average we will perform two tests. 
#First we will use a normal t.test without any distribution transformations. 

t.test(midwest$percollege, mu = 32, alternative = "less")

#4) What is the p-value obtained from the t-test? Do we accept or reject the null hypothesis?
#The p-value is 2.2e-16. 
# Since the p-value is less than 0.05 we reject the null hypothesis at a significance level of 0.05.


#however, due to the non-normality concerns we cannot trust the results from this analysis

#Let's do a t.test on our log transformed data
t.test(log(midwest$percollege), mu = log(32), alternative = "less")

#5) What is the p-value obtained from the t-test on transformed data? Do we accept or reject the null hypothesis?
#The p-value obatined is 2.2e-16. 
# Since the p-value is less than 0.05 we reject the null hypothesis at a significance level of 0.05.


#Two sample t-test
#let’s say we want to compare the differences between the average percent of college educated adults in Ohio versus Michigan. 
#Here, we want to perform a two-sample t-test because we are comparing between two samples, Ohio and Michigan.

#6) What is our null hypothesis? 
#There is no significant difference between the average of percent of college educated 
#adults in Ohio and the average of percent of college educated adults in Michigan

#setting up a dataframe with only data from Michigan and Ohio
df <- subset(midwest, state == "OH" | state == "MI")[,c("state", "percollege")]
str(df)
nrow(df[df$state=="OH",])
nrow(df[df$state=="MI",])

#summary for Ohio only (using pipeline syntax)
summary(df%>%filter(state == "OH")%>%.$percollege)

#summary for michigan only
summary(df %>% filter(state == "MI") %>% .$percollege)

#We can see Ohio appears to have slightly less college educated adults than Michigan 
#but the summary statistcis don’t tell us if it is statistically significant or not.

#making a boxplot in ggplot2
ggplot(df, aes(state, percollege)) +
        geom_boxplot()
#boxplot shows that Ohio has less college educated adults than Michigan

#looking at histograms
hist(subset(df, state == "OH")$percollege)
hist(subset(df, state == "MI")$percollege) #not normal, skewed

#same procedure as before
#without transformations
t.test(percollege ~ state, data = df)

#7)What is the p-value obtained from the t-test on our data? Do we accept or reject the null hypothesis?

#The p-value is 0.01032. Since the p-value is less than 0.05 we reject the null hypothesis at a significance level of 0.05.


#let's transform data
#remember that when we transform data to do an analysis, the results of the analysis pertain to the transformed data and MUST be interpreted as such

t.test(log(percollege) ~ state, data = df)

#8) What is the p-value obtained from the t-test on our transformed data? Is it different from the p-value obtained on untransformed data? Do we accept or reject the null hypothesis?
#The p-value is 0.003567. 
#It is different from untransformed data. 
# Since the p-value is less than 0.05 we reject the null hypothesis at a significance level of 0.05.



#Paired t-test
#To illustrate the paired t-test, we will use the sleep dataset
#Like the midwest dataset, this dataset is also built into the ggplot package

#In this case we are assessing if there is a statistically significant effect of a particular drug on sleep (increase in hours of sleep compared to control) for 10 patients. See ?sleep for more details on the variables.



str(sleep)
sleep
#9) How many variables does this dataset have? How many are continuous? How many are categorical?
#There are three variables.
# 1 variable (extra) is continuous
# 2 variables are categorical. 
#Out of these two variables one variable is ID.

#We want to see if the mean values for the extra variable differs between group 1 and group 2. For this, we will use a paired t-test. In a paired t-test, each sample or subject is measured twice, resulting in a pair of observation. Here the same patients are asked to sleep without a drug and then sleep with a drug. Because the increase in hours of sleep is measured on the same patient (number of hours slept with drug minus number of hours slept without drug)

#10) What is our null hypothesis?
#The null hypothesis is that the mean difference between paired observations is zero. 
#When the mean difference is zero, the means of the two groups must also be equal.

#looking at histograms
hist(subset(sleep, group == 1)$extra)

hist(subset(sleep, group == 2)$extra)

#11) Does the data look normally distributed? Do we need to transform this data?

#The data looks normally distributed. We will not have to transform this data.

#Let's look at a boxplot
ggplot(sleep, aes(group, extra)) +
        geom_boxplot()

#Doing a paired t-test is the same as doing a two sample t-test. We  but just add the paired = TRUE argument
t.test(extra ~ group, data = sleep, paired = TRUE)

#12)Determine if the above t-test has been performed correctly? If you think it has been performed correctly, why? If not, please re-do the test correctly and explain the results. Do we accept or reject the null hypothesis?

#The t-test has been performed correctly. 
#Since the data holds all the assumptions of t-test 
#Furthermore, the distribution of the data was normal.
# Since the p-value is less than 0.05 we reject the null hypothesis at a significance level of 0.05.


#Chi-sq tests in R
#In the earlier examples, we were testing to see if there is difference in continuous data across two categories. But what both our explanatory (independent) and response (dependent) variables are continuous and we want to find if there are patterns between two sets of categorical variables? In this case, we use a Chi-sq test. 

#Before we get started, there are a few things we need to keep in mind when performing a Chi-sq test in R
#1) The input data is in the form of a table that contains the count value of the variables in the observation.

#2) We use chisq.test function to perform the chi-square test of independence in the native stats package in R.
# For this test, the function requires the contingency table to be in the form of a matrix. 
#Depending on the form of the data, to begin with, this can need an extra step, either combining vectors into a matrix or cross-tabulating the counts among factors in a data frame. 

#3)We use read.table and as.matrix to read a table as a matrix. 
#While using this, be careful of extra spaces at the end of lines. 
#Also, for extraneous characters on the table, as these can cause errors.


#Suppose we have 105 patients under study and 50 of them were treated with the drug. 
#Moreover, the remaining 55 patients were kept under control samples. 
#Thus, the health condition of all patients was checked after a week.

#With the following table, we can assess if their condition has improved or not. 
#How can we tell if the drug had a positive effect on the patient?

#We can tell that the drug had a positive effect by doing a hypothesis (Chi-square) test 
#and using it to understand if the drug prescribed for the patient 
#has any outcome in improving patient’s chances of recovery.
#If there is a relationship between drug administration 
#and patient improvement then we can say that the drug had a positive effect.

#let's start by reading in the data
drug_data <- read.csv("Chi_sq_lab_12.csv")  #Reading CSV
str(drug_data)

#13) How many variables are in this dataset? How many of these variables are categorical and how many of these variables are continuous?
#There are three variables in our data
# All three of the variables are categorical
#Out of these three variables one variable is id the unique identifier

#14) What is our null hypothesis?
#Our null hypothesis is that there is no relationship between the drug administration 
#and patient improvement

#creating a table for treatment and improvement

z<-table(drug_data$treatment, drug_data$improvement)
z


#before we run the chi-sq test, we need to figure out if our data meets the assumptions of this test. 
#Remember that every statisitical analysis has a set of assumptions
#If we apply a statisitcal analysis to our data without meeting the asusmptions, then the results of our test are NOT reliable

#Assumptions of a Chi-sq test
#1) Single random sample
#2) Large sample size.
# If a chi squared test is conducted on a sample with a smaller size, then the chi squared test will yield an inaccurate inference. 
#3)Adequate expected cell counts. 
#A common rule is 5 or more in all cells of a 2-by-2 table, 
#and 5 or more in 80% of cells in larger tables, but no cells with zero expected count.
#4)The observations are always assumed to be independent of each other. 
#This means chi-squared cannot be used to test correlated data
#Running Chi-sq test
?chisq.test
#take a look at the help file to understand how this function works
chisq.test(z, correct=FALSE)

#let's take a look at the data before we run the Chi-sq test
plot(z)
plot(drug_data$treatment, drug_data$improvement)

#15) What do these plots show?
#The number of people who improved after being treated with the drug are higher
#then the number of people who were not treated with the drug
#another way
chisq.test(drug_data$treatment, drug_data$improvement, correct=FALSE)

#16)What does the analysis show about differences in treatment and improvement between the control group and the ones which were provided the medication? Do we accept or reject the null hypothesis?
#The p-value is 0.01841 it shows that there is a relationship between drug treatment and improvement.
#The number of people who improved after receiving the drug were more as compared to the number of people who did not receive the drug.
#Furthermore, the number of people who recovered after taking the drug were more than the people who did not improve after taking the drug.
# Since the p-value is less than 0.05 we reject the null hypothesis at a significance level of 0.05.

#Analysis of variance
#One way analysis of variance extends the t-test by allow us to compare continuous data between more than two groups. 

#Let's imagine this scenario
#We are interested in knowing whether the brand of car tyre can help us predict whether we will get more or less mileage before we need to replace them.
#We will draw a random sample of 60 tyres from four different tyre manufacturers
#While we expect variation across our sample, 
# we’re interested in whether the differences between the tyre brands (the groups) is significantly different than what we would expect by random chance for tires within each tyre brand

#let's start with reading the dataset
tyre <-read.csv("https://datascienceplus.com/wp-content/uploads/2017/08/tyre.csv")

str(tyre)

#17) How many variables are in this dataset? How many of these variables are categorical and how many of these variables are continuous?

#There are two variables in the dataset. 
#Brands is categorical
#Mileage is continuous

#18) What is our null hypothesis?
#The average mileage for all groups (brands of car tyres) is same.
#As always, we are going to start with a graph
boxplot(tyre$Mileage, horizontal = TRUE,  main="Mileage distribution across all brands", col = "blue")

#a simple boxplot of all 60 samples shows the center, shape and spread of the data
#we can get a sense of the center and spread of the data by building a boxplot grouping data by tyre brands
boxplot(Mileage ~ Brands, data = tyre, horizontal = TRUE,  main="Mileage distribution across all brands", col = rainbow(4), mat = TRUE) #mat = TRUE creates a matrix

#19) What does this plot show? Which brand has the most mileage? Which has the least? Does the variation in mileage for tyres within each band differ when we compare all the brands?

#The plots show that the box plots are overlapping.
#Falken has the most mileage as its mean is larger than the rest.
#Bridgestone has the least mileage as its mean is smaller than the rest of the brands.
#CEAT has lower variation towards the upper end of the plot and also has one outlier.
#The means of CEAT and Apollo seem to overlap.
#Overall, the variation for all brands does not seem so different.

#Doing an analysis of variance
#Here are the asusmptions of a one-way analysis of variance
#1) Independence of observations – this is an assumption of the model that simplifies the statistical analysis.
    
#2) Normality – the data or the distributions of the residuals are normal.
    
#3) Equality (or "homogeneity") of variances, called homoscedasticity — the variance of data in groups should be the same.

#20) Does our data meet these assumptions? Why or why not?

#Yes, we can say our data  holds these assumptions because:

hist(tyre$Mileage)
#The mileage data is normally distributed
#The observations are assumed to be independent as the mileage of one tyre brand doesnot affect the other.

var(tyre[tyre$Brands=='CEAT',]$Mileage)
var(tyre[tyre$Brands=='Apollo',]$Mileage)
var(tyre[tyre$Brands=='Falken',]$Mileage)
var(tyre[tyre$Brands=='Bridgestone',]$Mileage)
nrow(tyre[tyre$Brands=='CEAT',])
nrow(tyre[tyre$Brands=='Apollo',])
nrow(tyre[tyre$Brands=='Falken',])
nrow(tyre[tyre$Brands=='Bridgestone',])
#The variance for Bridgestone and Apollo appears to be very close
#The variance of CEAT is the highest and variance of Falken is the lowest
#The variance of the rest is different but we can't say simply from the numbers that the difference is statistically significant
#Moreover, the sample size of each group is same i.e. 15.
#Furthermore, from the box plots the spread of data seems to be fairly even between groups 
#there is no skew and no group seems to have larger variance than the other. 
#Therefore, by looking at the boxplots we can say that the variance is same for all groups.

#Hence, all the assumptions for Anova hold.

#lets run an analysis of variance (one-way)

tyres.aov<- aov(Mileage~Brands, tyre)
#we are using the aov command in R to perform a one way analysis of variance
#we are assigning the results of the aov command to a variable called tyres.aov

#let's look at the class of the tyres.aov variable
class(tyres.aov)
typeof(tyres.aov)
names(tyres.aov)

#21) What do these functions tell us about the tyres.aov variable?
tyres.aov
#tyres.aov has the class "aov" and "lm"
#the type of tyres.aov is a list
#tyres.aov contains different informations (approximately 13) 
#We can access each of them using tyres.aov$information_name 
#The information_name can be coefficients, residuals, effects, rank, model etc

#let's look at the results
summary(tyres.aov)

#22) Based on this analysis what can we say about the differences in mileage between different brand of tyres? Do we accept or reject the null hypothesis?
#Since the p-value is 2.78e-08 the differences in mileage between different brand of tyres is significant. 
#We reject the null hypothesis at a significance level of 0.05.