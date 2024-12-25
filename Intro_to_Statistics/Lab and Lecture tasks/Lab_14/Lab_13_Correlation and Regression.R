#In today's lab, we will be learning how to perform correlation analysis and simple linear regression

library(MASS)
library(ISLR) # you will need to install this package
library(ggplot2)

#we are going to use the built-in dataset Boston in the MASS package
#This dataset contains information collected by the U.S Census Service concerning housing in the area of Boston Showing results for Massachusetts
names(Boston) #looking at the variable names in the Boston dataset
str(Boston) #looking at the structure of the variables

#There are 14 attributes in the dataset. They are as follows:
#CRIM - per capita crime rate by town
#ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
#INDUS - proportion of non-retail business acres per town.
#CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
#NOX - nitric oxides concentration (parts per 10 million)
#RM - average number of rooms per dwelling
#AGE - proportion of owner-occupied units built prior to 1940
#DIS - weighted distances to five Boston employment centres
#RAD - index of accessibility to radial highways
#TAX - full-value property-tax rate per $10,000
#PTRATIO - pupil-teacher ratio by town
#BLACK - 1000(Bk - 0.63)^2 where Bk is the proportion of African American in each town
#LSTAT - % lower status of the population
#MEDV - Median value of owner-occupied homes in $1000's

#Let's see if any of these variables are associated with each other
#Start with CRIM (per capita crime rate) and INDUS (proportion of industrial business per acre per town)
#We will do this both in base R and in ggplot2

#Base R
plot(crim ~ indus, data = Boston, xlab = "Proportion of Industrial Business per acre per town", ylab = "per capita Crime Rate")
#Q1)Using the techniques from our lecture, please describe the direction, form and strength of the relationships between these two variables. Are there any potential outliers?
#cor(Boston$crim,Boston$indus)

#The relationship is not linear
#The relationship is moderate and is positive because at one point y_values increase  
#There are outliers when x is 18 the y_value is changing

#Q2)Make a scatterplot for variables ZN and NOX. Using the techniques from our lecture, please describe the direction, form and strength of the relationships between these two variables. Are there any potential outliers?
plot(zn ~ nox, data = Boston, xlab = "proportion of residential land zoned", ylab = "nitric oxides concentration (parts per 10 million)")
plot(nox~zn, data = Boston, xlab = "nitric oxides concentration (parts per 10 million)", ylab = "proportion of residential land zoned")
#cor(Boston$zn,Boston$nox)

#The direction of the relationship is negative
#The relationship is moderate
#There are outlier present in the data 
#The relationship is not linear
#We can imagine that for a large dataset like this, it can get very tedious to make scatterplots that explore the relationship between every variable. 

#To deal with this, we can make scatterplot matrices which show the relationship between each variable
pairs( ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat +medv, data = Boston, main="Simple Scatterplot Matrix")

#However, since we have a lot of variables it is hard to decide which ones are related and which ones are not based on the scatterplot matrix

#Q3) Split the Boston dataset in half and create scatterplot matrices for both halves. Once you have done this, post them on LMS (under assignments). This will count as your lab participation mark. 
#smp_size <- floor(0.5 * nrow(Boston))
## set the seed to make your partition reproducible
#set.seed(123)
#first_ind <- sample(seq_len(nrow(Boston)), size = smp_size)

#first <- Boston[first_ind, ]
#second <- Boston[-first_ind, ]

pairs( ~ crim + zn + indus + chas + nox + rm + age, data = Boston, main="Simple Split Scatterplot Matrix for First Half")
pairs( ~ dis + rad + tax + ptratio + black + lstat +medv, data = Boston, main="Simple Split Scatterplot Matrix for Second Half")

#Q4) Please describe the association between each variable in your scatterplots (hint: what we did in lecture and what you did for that single scatterplot)

#Now that we have visually described each variable, how can we quantify the strength of the association? We can do a correlation analysis using the cor() function in R. 

cor(Boston) #This produces a correlation matrix. In a correlation matrix, the correlation coefficient for all possible combinations of specified variables from a dataset is provided. 
 #The default is pearson's correlation coefficient. 
#use ?cor to learn how to change the default type to Spearman's rho and Kendall's tau. 

cor(Boston$crim, Boston$indus) # this is how you can assess the relation
cor(Boston$crim, Boston$indus,method = "pearson") #0.4065834
cor(Boston$crim, Boston$indus,method = "kendall")  #0.5210138
cor(Boston$crim, Boston$indus,method = "spearman") #0.7355237

#Q5) List the pearson, spearman, and Kendal correlation coefficient for the relationship between the following variables: 
#a) crim and zn
cor(Boston$crim,Boston$zn) #pearson = -0.2004692
cor(Boston$crim, Boston$zn,method = "kendall") #kendall = -0.4620566
cor(Boston$crim, Boston$zn,method = "spearman") #spearman = -0.5716602

#b) nox and black
cor(Boston$nox,Boston$black) #pearson = -0.3800506
cor(Boston$nox,Boston$black,method = "kendall") #kendall = -0.2024303
cor(Boston$nox,Boston$black,method = "spearman") #spearman = -0.2966616

#c)ptratio and medv
cor(Boston$ptratio,Boston$medv) #pearson = -0.5077867
cor(Boston$ptratio,Boston$medv,method = "kendall") #kendall = -0.3987886
cor(Boston$ptratio,Boston$medv,method = "spearman") #spearman = -0.5559047

#Q6)What do these correlations show? 
#It shows strength and direction of a relationship between two quantitative variables.
#For example: relationship between crim and zn is negative & weak
#relationship between nox and black is negative & weak
#relationship between ptratio and medv is negative & moderate

#Q7)Are there any differences between the pearson, spearman and kendall correlation coefficients?

#numerical value differences can be seen in all 3.
#For Pearson each variable should be continuous.
#For Spearman and Kendall variables should be measured on 
#an ordinal, interval or ratio scale.


###### Linear Regression#############
#In order to determine how well one variable explains the variation in another variable, we perform linear regression
#Assumptions of a simple linear regression:
#a) Linearity: The relationship between X and the mean of Y is linear.
#b) Homoscedasticity: The variance of residual (difference between actual data for Y and the predicted values, also known as fitted values estimated from the model. This is a measure of error in our model) is the same for any value of X.
#c)Independence: Observations are independent of each other.
#d) Normality: For any fixed value of X, Y is normally distributed.


#To perform a linear regression in R, we use lm()


pm<-lm(ptratio ~ medv, data = Boston)

#let's look at the summary of this model
summary(pm)
#summary shows us the form of the model (which variable is the response, and which variable is the explanatory variable)
#provides a description of centre and spread of the residuals. Note residuals is the difference between the actual data and that predicted by the model
#p value in the summary for medv tells us that medv is a statistically significant predictor of ptratio
#Adjusted R2 value (also known as coefficient of determination) tells us what percentage of the variance in ptratio is explained by medv

#let's visualize how well this linear model fits our data
plot(pm)

#when we apply the plot function on a variable assigned a linear model, it produces four plots. The plots are called diagnosis plots and are used to determine how good of a fit the model we proposed is with actual data. 
#The following is a description of each of the four diagnostic plots produced:
#1)The first plot (residuals vs. fitted values) is a simple scatterplot between residuals (difference between actual data and predicted values) and predicted values. The plot is used to detect non-linearity, unequal error variances, and outliers  It should look more or less random. If there is an increasing or decreaing pattern in this plot, this means your model does not fit your data well. 
#2)The second plot (normal Q-Q) is a normal probability plot. It is used to check if your data meet the assumption of normality. If your underlying response and explanatory data are normally distributed, then your errors should be normally distributed as well. Points close to the dashed line in the middle suggest a normal distribution. If most of your points are not close to this line, then your underlying data is NOT normally distributed and you need to use a generalized linear model with a non-normal distribution. 
#3) The third plot (Scale-Location), like the the first, should look random. This plot shows whether residuals are spread equally along the ranges of input variables (predictor). The assumption of equal variance (homoscedasticity) could also be checked with this plot.
#4) Residuals versus leverage: This plot helps us find out if we have any outliers or influential cases in our data
#5) Any data points that fall outside the red dashed line (cook's distance threshold) are outliers. When cases are outside of the Cook’s distance (meaning they have high Cook’s distance scores), the cases are influential to the regression results. The regression results will be altered if we exclude those cases.

#Q8)Based on these four diagnostic plots, are the assumptions of linear regression being met?

#residuals vs. fitted values plot looks random & there is no increasing or decreaing pattern in this plot. So model does fit the data well
#normal Q-Q: Not all points, but only points in the middle are close to the dashed line so underlying data is normally distributed.
#Scale-Location: residuals are not spread equally along the ranges of input variables but the pattern more or less looks random so I would say 
#homoscedasticity holds. 
#Residuals versus leverage: there are significant outliers

#Note: We will learn about model selection and multiple linear regression in our next lab

