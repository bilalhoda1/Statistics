#Lecture Notes
#Dealing with count data
#Please note that the data and the R file for today's lecture is provided in the teams chat

#Announcement
#Instructions and grading criteria for draft report of project II has been uploaded to LMS
#Deadline is 20th June 12:00am
#You should have finished collecting data from your databases and should start thinking about how you are going to do your regression analysis

#Let's start with a review

#Starting off easy
#What are the components of a GLM?
1) Random
2) Systematic
3) Link function

#A little bit harder
#What are the main steps in implementing a GLM?
1) Specify the distribution of the response variable 
2) Specify the relationship between the response and explanatory variables
3) Specify the link function

#What is a response function? (Hint: It is related to the link function!)
The inverse of the link function. Sometimes used in lieu of the link function

#What is the difference between a logistic regression and a binomial GLM? 
#Think about what the outcomes look like
1) logistic uses the sigmoid link function (logit) - two events - binary outputs
2) Binomial GLM: 0-1, any value for probability of an event, i.e. no binary output


#For running a logistic regression, what is the probability distribution that we use?
Binomial

#For running a logistic regression, what is the link function that we use?**
Logit AND Probit

#What are two components of a logit function?
1)Odds
2)Logarithmic transformation 

Logit = log(odds)

#Suppose both logit and probit link functions are used with the same model and the same data
#How would the fit of the model using the logit link function differ from the fit of the model using a probit link function?
1) What is a probit? Inverse of the normal cumulative density function - determined analytically
2) Probit is steeper than logit
#That's it for the review

#Today we are going to introduce you to GLMs for dealing with count data. 
#Let's start with a super easy question! What is count data?
Count data: discrete, non-negative, integer value - number of objects/people


#What is the problem of using a simple linear regression or an OLS regression when using count data?
When we are using linear regression, we are assuming data is normally distributed. Since normally distributed data can have negative values, predicted values can also be negative, which is impossible for count data

#What probability distribution do count data have?
Poisson distribution

#Important features of the poisson distribution:
#1) Mean and variance are determined by the same parameter, lambda
#Let's go back to our cod fish example from last week
#Here is an interesting link about the cod fishery collapse in Newfoundland Canada
#https://www.youtube.com/watch?v=0sFmT8IXGhw
#We go fishing for cod fish at a depth of 10m and we find no fish. 
#Generally, the average number of fish caught at a depth of 10m is 3
#What is the probability of catching no fish at the depth of 10m?
dpois(0, lambda = 3) #probability of catching no fish at 10m


#let's find out the probabilities of catching between 1 to 20 cod fish at a depth of 10 m, when on average 3 fish are caught at this depth
z<- NULL
for (i in 1:20){
z[i]<-dpois(i, lambda = 3)
}
plot(z, type = 'l') #what features of the poisson probability distribution can we develop from this plot

#Let's assume that the mean is 10 fish instead of 3 fish. What happens then?
y<- NULL
for (i in 1:20){
y[i]<-dpois(i, lambda = 10) #changing mean to 4
}
plot(y, type = 'l') 

par(mfrow = c(2,1))
plot(z, type = 'l', main = "Average at 10m = 3 fish")
plot(y, type = 'l', main = "Average at 3m = 10 fish")

#what happens to the probability of getting outcomes between 1 and 20 when the mean increases? 
#as mean approaches the middle of the range of possibile outcomes/counts, the distirbution becomes more symmetric
#what happens to the variance as the mean increases?

#you can also visualize this using this link:
#https://seeing-theory.brown.edu/probability-distributions/index.html#section2
#please select poisson distribution and play around with the mean

#Now that we know a little bit about how the poisson distirbution works, let's run an example poisson GLM

#we will use the cod data that we have been talking about so far


#setting up working directory
setwd("D:/bilal's books 8/Lie Detector/Lab assignments/Lecture 16/Lecture")
fish<-read.csv("Fish_Lec16.csv")

#let's look at this data
str(fish)
#we have lots of variables
#we are interested in how period, mean depth and swept area influence the total abundance of cod caught 
#Given this, what is our Y variable? What are our X variables?
Y: Total abundance
X: Period, Mean depth, swept area

#Let's break this GLM analysis down:
#1) specify the distribution
Poisson
#2) specify the systematic component
Y = B1 (Intercept) + B2*Period + B3*Mean_depth + B4*Swept_area
B2 = slope coefficient for Period
B3 = slope coefficient for Mean depth
B4 = slope coefficient for Swept Area

#3) specify the link function
#for the link function it is important to note that we have two options. We have the option of an identity link function. What does this mean?

identity = E(Y|X) which is the mean of the distribution of y variables is equal to the Y in the systematic component - direct link
mu (mean of probability distribution of Y|X) = B1 + B2*Period + B3*Mean_depth + B4*Swept_area

#Can anyone think about any problems with using the identity link function?
#Possibility of negative values when assuming mean of poisson can estimated from a line

#we can also use the log link function. This will prevent us from producing negative fitted value which are impossible for abundance data. 

#let's run this model
glm1<-glm(TotAbund ~ Period + MeanDepth + SweptArea, data = fish, family = poisson(link = "log"),na.action = "na.fail")
#once we run a model, what do we do first?

#we check whether assumptions are met! How do we do this?
#visually, using the plot command
plot(glm1)

#residuals versus fitted looks good (no increasing or decreasing pattern)
#we don't care about normality since our distribution is poisson
#scale-location plot is hmmm. (trend is increasing towards the end)
#we also got a few outliers.... #40, 23, 103

#generally, when we have outliers, we run our analysis BOTH with and without them
#then we explain the results of these two analyses
#also we need to explain why we might have gotten these outliers in our discussion

#let's ignore outliers for now. The scale-location plot has a small increasing trend likely driven by the outliers, so ignoring this as well

#the next step is model selection
#What does this mean?
Picking the variables which best explain the variation in Y

#Not all of the variables that we have added in a model are going to explain the variation in our response variable

#So what techniques can we use to determine which variables explain a significant variation in total cod abundance. 
#Hint: Think about model selection in a multiple regression framework!

#Three possible methods
#1) Comparing adjusted R2 values
#2) Forward or backwards selection using F-tests
#3) AIC values

#For GLMs, R2 values don't really exist. So we have two options forwards or backwards selection with F-tests and using AIC values

#For poisson GLM, we use Chi-sq tests instead of F-tests

#Let's do backwards selection (does anyone remember what this is?)

#We start out with the model with all of the X variables
#Then we remove variables one by one and see if the variance explained for the Y by the model is significantly different
#we do this using maximum likelihood estimation methods

glm1.1<-update(glm1, .~. -Period) #.~. terminology mean everything and then we are remoming period from glm1
    anova(glm1, glm1.1, test = "Chisq")
#Model with period explains significantly more variation than model without
#Keeping period in the final model

#Let' do this for Mean depth
glm1.2<-update(glm1, .~. -MeanDepth)
anova(glm1, glm1.2, test = "Chisq")
#Model with mean depth explains significantly more variation than model without
#Keeping mean depth in the final model

#One more time for Swept area
glm1.3<-update(glm1, .~. -SweptArea)
anova(glm1, glm1.3, test = "Chisq")
#Swept area is also explaining significant variation in Y
#So we are keeping this in the final model as well

#In this case, the final model is the full model

#Another way we can do this is using the dredge function in MuMIn package
library(MuMIn) #please install this package if you haven't done so already
dredge(glm1) #this is the full model
#let's look at this table
#each line represent a model with a different structure

#MnDpt is mean depth

#Perid is period

#SwptA is swept area

#as you can see some models have all the X variables and others have combinations of some or only 1 or 2

#df is degrees of freedom (number of X variables in model -1 )

#logLik is the log likelihood value 

#AICc is the AIC value corrected for low sample size

#delta is the difference between the AIC value of the model and the model listed before it

#models with delta AICc values less than 2 or less than 4 (depending on which statistician you follow) are considered to be equivalent in terms of fit

#in the case of this data, the full model with the best fitting model

#let's look at the summary of the best fit model
summary(glm1) #all variables are significant!!
#Also note that no R2 value is present

#let's visualize how well the model is fitting with the data (for each x)
summary(fish)
#for period, since there are only two option,we can use a boxplot
boxplot(TotAbund ~ Period, data = fish, xlab = "Fishing Period", ylab = "Total Abundance of Cod fish")
#what does this show?
#for continuous variables, we will need to plot the predicted line with the observed data to evaluate fit

#first we need to create a large number of x values, arranged in sequence from minimum x to maximum x
summary(fish) #getting minimum and maximum values for each variable in dataset
x_swept_area<-seq(7970, 223440, 1)
x_mean_depth<-seq(804, 4865, 1)

#now let's predict y values for the x values obtained above
glmswept_only<-glm(TotAbund ~ SweptArea, data = fish, family = poisson(link = "log")) #we need to create model with one x variable at a time
y_swept_area<-predict(glmswept_only, list(SweptArea = x_swept_area), type = "response") 

#let's plot this
plot(TotAbund ~ SweptArea, data = fish, xlab = "Swept Area (km2)", ylab = "Total Abundance of Cod fish")
points(y_swept_area, type = "l")

#note that this fits the line showing the trends
#What does this trend show?
It is showing a decreasing trend.

#Homework: Please create a plot fitting the trend line for mean depth and total abundance as determined by the best fit glm. You will need to do this individually and submit your plots to me on the Microsoft teams chat. Both the observed data and the predicted trend line MUST be a different color (NOT black!)
#Due date: 10:00am tomorrow!

#now let's predict y values for the x values obtained above
glmdepth_only<-glm(TotAbund ~ MeanDepth, data = fish, family = poisson(link = "log")) #we need to create model with one x variable at a time
y_depth<-predict(glmdepth_only, list(MeanDepth = x_mean_depth), type = "response") 

#let's plot this
plot(TotAbund ~ MeanDepth, data = fish,main="Total Abundance vs Mean Depth", xlab = "Mean Depth", ylab = "Total Abundance of Cod fish",col='cyan',pch=16)
points(y_depth, type = "l",col='red')

The trend is a decreasing trend.