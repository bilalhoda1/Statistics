#Week 14 Lecture 2: Multiple regression in R 
###############Important announcements##############################################
####Important Announcement #1
# I have graded all of the draft reports for the group project. 
#The final Report is due Sunday April 19th at 12:00am. 
#I will use the same rubric for marking as the draft report. 
#However, 10% of the grade will be dependent on if and how well the changes suggested by me in the draft report have been incorporated in the final report.
#Rubric for the final report will be provided on LMS by the end of today

#####Important Announcement 2
#As you are aware there is a final project due for this course which is worth 30% percent of your grade
#For this final project, you will work in groups
#For this project you have to:
#a) develop a research question
#b) find a large/publicly available dataset to answer this research question
#c) make decisions on which analyses you are going to apply based on what you have learned in this course
#d) submit a draft report (similar in format to group assignment #1)
#e) receive feedback from me and submit a final report

#Please note that for this assignment I expect students to apply: 
#i) Regression analysis (Simple or Multiple)
#ii) Analysis of Variance
#iii) Principal components analysis

#You can use any dataset that you like!!!!!!

#Here are some potential data repositories that you can use: 
#1) Dryad (https://datadryad.org/stash). Data from scientific papers that have been published
#2) A good list of data repositories for sciences and social sciences: https://www.nature.com/sdata/policies/repositories#social
#3) List of Pakistan based data: https://propakistani.pk/2015/07/08/comprehensive-list-of-all-public-data-sources-in-pakistan/
#4)List of international relations and human rights data: https://hls.harvard.edu/library/research/find-a-database/international-relations-human-rights-data/
#5)Seshat Global History Databank: http://seshatdatabank.info/
 
#During the break starting next week, you must determine your research question as a group and find an appropriate dataset that can be used to answer this question
# You will be expected to send me a one page maximum proposal by April 30th 2020 with the following: 
#a) One paragraph providing the general context of your research question
#b) One, two or three inter-connected research questions
#c) Scientific hypothesis (NOT statistical hypothesis, i.e. I want your best guess on what the outcome of your study will be rather than the Null or alternate hypothesis)
#d) The dataset that you will be using
#Please make sure to include the names of all group members on this document

#This proposal is worth 3% of the grade for your final assignment. Once I provide my approval for your research question and dataset, you can start working on your analysis strategy and first draft. Deadlines for your first draft will be announced next week on the class chat (as well as via email). Please make sure to keep these deadline in mind

####Important announcement #3
#I will be assigning two video lessons to everyone on logistic regression and principal component analysis during the break (on chat and via email)
#Each video lesson will be no more than 30 minutes
#All students are expected to watch these videos and come prepared to lecture when classes resume on June 1st 2020

################Today's Lesson###########################################################
#Let's get started with today's lesson
#For today, we will be continuing with multiple regression which we introduced in our last lecture
#However, unlike our other classes, we will be working together as a class through an R script

#Review from last lecture
#In simple linear regression, we assess the relationship between two quantitative variables by fitting a line that best describes this relationship. 
#This line of best fit is determined by reducing the sum of squared errors between the line proposed and actual points (also known as residuals)
#We can also use the R2 value: also known as the coefficient of determination to assess the proportion of variation in the response (or Y variable) that is explained by the explanatory (or x variable)
#A good explanation of what R2 is: https://www.youtube.com/watch?v=2AQKmw14mHM

#But rarely in life do we have situations where only one variable explains most of the variation in the response variable of interest
#Most of the time, we have a few candidate variables that COULD be affecting the response variable 
#In this scenario, we use a method known as multiple regression

#For today's class, we will use the low birthweight dataset from Hosmer and Lemeshow (2000) Applied Logistic Regression: Second Edition. 
#These data are copyrighted by John Wiley & Sons Inc. and must be acknowledged and used accordingly. 
#Data were collected at Baystate Medical Center, Springfield, Massachusetts during 1986.

#let's start by setting the working directory 
setwd("/home/sarah/Documents/Habib/Math107/Lectures")
#loading the datafile
birth<-read.csv("birthweight.csv")
#let's take a look at the dataset
str(birth)
#X is the id of the baby
#low: indicator of birth weight less than 2.5 kg 
#age: mother's age in years.
#lwt: mother's weight in pounds at last menstrual period.
#race: mother's race (1 = white, 2 = black, 3 = other).
#smoke: smoking status during pregnancy.
#ptl: number of previous premature labours.
#ht: history of hypertension.
#ui: presence of uterine irritability.
#ftv: number of physician visits during the first trimester.
#bwt: birth weight in grams.

#How many categorical and continuous variables do we have in this dataset?

#Let's make some scatterplots
plot(bwt ~ age, data =birth)
plot(bwt ~ lwt, data = birth)
plot(bwt ~ ptl, data = birth)
plot(bwt ~ ftv, data = birth)

#what do these scatterplots show?

#Let's do a correlation analysis
colnames(birth) #to figure out which columns are continuous variables
cor(birth[c(1,2,5,11),])
#what does the correlation show?
#let's fit a multiple regression for birth weight in grams with all of the other continuous variables in the dataset as explanatory variables
lm1<-lm(bwt ~ age + lwt + ptl + ftv, data = birth)
#note how + allows us to add multiple variables in a linear model context
#let's look at the summary
summary(lm1)

#In the summary command, we are provided with the following
#1) The form of the linear regression model we used (which is the response variable and which are the explanatory variables)
#2) A summary of center and spread for the residual (Mininum value, 1Q (first quartile), Median value, 3Q(third quartile), Maximum value)
#3) Estimates of the intercept for the line of best fit (minimizes sum of squared error) 
#and slopes for each variables in the model (see table labelled coefficients)
#asterisks denote variables which explain a statistically significant proportion of the variable
#The total proportion of variation in birth weight explained by this model is provided as the multiple R-squared value
#However, R squared value increases with an increased number of explanatory variables in the model
#Therefore, the adjusted R squared value provides an R-squared value that is corrected for the number of explanatory variables in the model
#The p-value of the whole model based on the F statistic is provided as well in "p-value"

#let's look at the plots to make sure that we are fitting the assumptions
plot(lm1)
#The first plot is the residuals versus fitted plot. 
#If the residuals are increasing or decreasing with fitted values, then there is no constant variance and this is a violation of this assumption of homoscedasticity
#The next plot is the Normal Q-Q plot
#If your data does not follow the diagonal line closely, then it is not normally distributed and your are violating the assumption of normality
#The next plot is the scale-location plot which another way of examining whether variance in your data is increasing or decreasing as values of the x variable/variables increase or decrease. 
#If points in this plot show a triangle or a fan shape, then your data is violating the assumption of equal variances (homoscedasticity)
#The final plot is the residuals versus leverage plot which is used to identify outliers. This plot shows points which have an undue influence on the form of the regression line which is chosen as the best fit. If a data point falls above or below the 0.5 dashed line (representing cook's distance), then this data point is an outlier. 

#Back to our model
#Please note that the more variables we add to the model, the more variation we explain in birth weight
#However, we only want to include variables that explain a statistically significant proportion of the variation in birth weight

#How do we go about doing this? 
#We have number of statistical procedures that we can use to determine the most important explanatory variables.
#These procedures are known as model selection prodecures since they are used to select the best model


#Broadly speaking, there are three ways of model selection
#1) Adjusted R-squared values
#2) Partial F-test selection
#3) Akaike's Information Criterion


#First, let's learn about the difference between R2 and adjusted R2
#https://www.youtube.com/watch?v=KjRrdb2x6dA
#Let's apply model selection using adjusted R squared

#Model selection tends to be done iteratively
#This means we add or remove value and see the effect this has on how well our model explains the variation in our response variable
#If we start off with a simple model and add variables one by one, this is called forward selection
#If we start off with the full model (the model contains all of the variables) and then remove variables one by one, this is called backward selection

#Let's forward select with adjusted R-squared
lm1.1<-lm(bwt ~ age, data = birth)
summary(lm1.1) #adjusted R2 is 0.002853

#What does this adjusted R2 value mean?

#let's add another variable
lm1.2<-lm(bwt ~ age + lwt, data = birth)
summary(lm1.2) #what is the adjusted R2 value now?

#What does this mean about the relative importance of lwt versus age?

lm1.3<-lm(bwt ~ age + lwt + ptl, data = birth)
summary(lm1.3) #what is the adjusted R2 value now?
#What does this mean about the relative importance of lwt and age versus ptl?

lm1.4<-lm(bwt ~ age + lwt + ptl + ftv, data = birth)
summary(lm1.4) #what is the adjusted R2 value now?
#What does this mean about the relative importance of lwt, age and ptl, versus ftv?
#Based on this, which variable do you think should be in the final model?

#Best model: Lwt + Ptl
lmbest<-lm(bwt ~ lwt + ptl, data = birth)
summary(lmbest)

#Let's try backwards selection
#Full model
lm1.4<-lm(bwt ~ age + lwt + ptl + ftv, data = birth)
summary(lm1.4)
#Let's drop an explanatory variable (age)
lm1.5<-lm(bwt ~ lwt + ptl + ftv, data = birth)
summary(lm1.5) #what is the adjusted R2 value?
#What does this mean about the importance of age as a variable in explaining variation in birth weight?
#We can continue this procedure until we are left with no variables

#Try doing backward selection on your own

#Instead of using Adjusted R2 values, we can also use  partial F-tests to determine which variables are the most important
#For more information about model selection using F-tests, see: https://www.youtube.com/watch?v=G_obrpV70QQ
#In this procedure, we can do both forward or backwards selection with partial F-tests

#Let's do backwards selection
#Starting with the full model
lm1.4<-lm(bwt ~ age + lwt + ptl + ftv, data = birth)
#and the model without age
lm1.5<-lm(bwt ~ lwt + ptl + ftv, data = birth)
#then we use the following code to perform the F-tests
anova(lm1.4, lm1.5, test = "F")
#This provides us with an analysis of variance table with different numbers
#We are most interested in the Pr(>F), which is the probability of obtaining an F value as extreme or more extreme than the one obtained
#What is the p value here?
#Given what we know about p values and null hypothesis significance testing, do we think that age of the mother is a significant predictor of birth weight?

#P value is not less than 0.05. 
#Therefore, there was no effect of the removal of age on the amount of variation that was explained by our model. 
#Hence age is NOT an important predictor of birth weight. 

#One more time
lm1.5<-lm(bwt ~ lwt + ptl + ftv, data = birth)
#and the model without ptl
lm1.6<-lm(bwt ~ lwt + ftv, data = birth)
#perform F-test
anova(lm1.5, lm1.6, test = "F")
#ptl is not a significant explanatory variable for birth weight because p value is NOT less than 0.05

#We can do a similar procedure with Akaike's information criterion (AIC)
#please see this video to get a good background on AIC:  https://www.youtube.com/watch?v=YkD7ydzp9_E
#If we use Akaike's information criterion, the model with the lowest AIC value is the best model

#Now you can imagine that forward or backwards selection is simple when you have a few explanatory variables
#But what if you have more than 5 variables. And what if the variables interact with each other?
#How can we do model selection in a painless way in this scenario?
#We can automate this whole procedure and examine the AIC values of all possible combinations at once
library(MuMIn) #please install this package

#we can use the dredge command on the full model and examine the AICs of all possible forms
dredge(lm1.4)
#when we do this, we get a model selection table
#In each row is a form of the model
#explanatory variables names are provided as column names followed by likelihood based measures of model performance
#for each row, if there are numbers in the column of an explanatory variable, this means that the variable was included in the model assessed
#since the model with the lowest AIC (or AICc, which is AIC corrected for small sample sizes) value is the best model. 
#which variables among the ones provided explain ths most variation?


#Models with AIC/AICc values that are very close to each other are considered equivalent as long as the delate value is less than 2
#If we apply this, which variables/models should be considered equivalent to the model with the lowest AIC value?

#based on the models with delta less than 2, what is the final form of the model which maximizes the variation explained in birth weight with the least number of variables?





