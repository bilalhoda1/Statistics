#GLMs in R (adapted from the analysis factor)

#Ordinary Least Squares regression provides linear models of continuous variables. However, much data of interest to statisticians and researchers are not continuous and so other methods must be used to create useful predictive models.

#The glm() command is designed to perform generalized linear models (regressions) on binary outcome data, count data, probability data, proportion data and many other data types.

#In the mtcars data set, the variable "vs" indicates if a car has a V engine or a straight engine. In this dataset, cars either have a V engine or they have a straight engine (data is binary)

#We want to create a model that helps us to predict the probability of a vehicle having a V engine or a straight engine given a weight of 2100 lbs and engine displacement of 180 cubic inches.


#let's start by exploring our data
str(mtcars)
summary(mtcars)

#running a binomial glm with logit link 
model <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)
#most commonly used link function for binary data (yes or no) is a logit link. This is the default link function used in binomial regression using the glm command

??glm
#read the help file to learn about how the glm function is parameterized

summary(model) #looking at summary

#this is what the summary looks like
#Call:
#glm(formula = vs ~ wt + disp, family = binomial, data = mtcars)

#Deviance Residuals:
#Min        1Q    Median        3Q       Max
#-1.67506  -0.28444  -0.08401   0.57281   2.08234

#Coefficients:
            #Estimate  Std. Error z value  Pr(>|z|)
#(Intercept)  1.60859    2.43903   0.660    0.510
#wt           1.62635    1.49068   1.091    0.275
#disp        -0.03443    0.01536  -2.241    0.025 *
#---
#Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 43.86 on 31 degrees of freedom
#Residual deviance: 21.40 on 29 degrees of freedom
#AIC: 27.4

#Number of Fisher Scoring iterations: 6

#Weight influences vs positively, while displacement has a slightly negative effect. Note:  In summary, under coefficients, slope coefficient for wt is positive while slope coefficient for disp is negative

#The slope coefficient of weight is non-significant (p > 0.05), while the coefficient of displacement is significant (p <0.025)

#Note that coefficients of slope and intercept are in units called logit

#examine the diagnostic plots for the glm (using the plot() command)
plot(model)
#which of these four plots should be ignored
#The normal qq plot should be ignored because it is used for testing normality and in our case the 
#The response variable is categorical and we are using logistic regression to predictt the categories
#hence the distribution of the response variable will be binomial.

# do the residual versus fitted and scale versus location plot show homoscedasticity (no trend in prediction error as the value of the explanatory variables increases)
#The residual vs fitted plot isn't completely random as in linear regression. 
#We can see there are two separate groups of points following some sort of a pattern.

#The scale vs location plot shows some sort of trend such that the residual values increase as the predicted
#values increase and after a certain point both of them start to decrease.

#Hence, both of the plots show there is heteroscedaticity.

#are there any outliers?
#There are no outliers although the data point Hornet 4 Drive is quite 
#close to the upper dashed line of COok's Distance

#Let's plot observed data alongside predicted data
#model with only wt as explanatory variable
model_weight <-glm(formula = vs ~ wt, family = binomial, data = mtcars)
summary(model_weight)

#To plot our model we need a range of values of weight for which to produce fitted values. 
range(mtcars$wt)
#what is the range of wt?

#creating a sequence of values from 0 to 6 (representing the range of wt) in intervals of 0.01
xweight <- seq(0, 6, 0.01)

#using the predict() function to create the model for all of the values of xweight.
yweight <- predict(model_weight, list(wt = xweight),type="response")

#plotting data
plot(mtcars$wt, mtcars$vs, pch = 16, xlab = "WEIGHT (g)", ylab = "VS")
#adding the line of best fit from the model
lines(xweight, yweight)

#what does this plot show you?
#The plot shows that the smaller the weight the higher the probability of the car having v engine

#check the diagnostics plots for this glm? Are the assumptions of glms being met?
plot(model_weight)
#The residuals vs fitted plot shows that the points are not randomly spread which means there is heteroscedasticity

#The normal QQ plot would be irrelevant because it is used for checking normality the points are close to the line
#But it seems that they donot satisfy normality which is fine because GLMS allow the distribution of response to be non-normal
#and allows distribution to be from the exponential family of distributions. Our distribution would be binomial because we 
#are cheking if the car has a v engine or not.

#The scale location plot shows a pattern so from -4 to 0 the as the predicted values increase the values of 
#residuals also increases but after a point as the predicted values increase the residuals increase and decrease both
#hence it seems there in homoscedasticity from the residual and fitted plot and scale and location plots.
#As per this link: https://online.stat.psu.edu/stat504/node/216/
#Homoscedasticity does not to be necessarily satisfied for glms

#The residuals vs leverage plot shows there are no outliers as the dashed line for cook's distance is
#not appearing and no point is outside the dashed line

#None of the assumptions of glms seem to be violated.

#Build a separate model for explaining vs using displacement. 
#examine the summary of the model. What does the summary tell you?
model_disp <- glm(formula = vs ~ disp, family = binomial, data = mtcars)
summary(model_disp)

#Using the procedure shown above, plot the predicted values from this model with the observed data.
xdisp <- seq(70, 500, 0.01)
ydisp <- predict(model_disp, list(disp = xdisp),type="response")
#plotting data
plot(mtcars$disp, mtcars$vs, pch = 16, xlab = "Disp", ylab = "VS")
#adding the line of best fit from the model
lines(xdisp, ydisp)

#what does this plot show you?

#The plot shows the smaller the displacement the higher would be the probability of the car having v engine


#check the diagnostic plots for this glm? Are the assumptions of the glms being met?
plot(model_disp)

#The residuals vs fitted plot shows that the points are not randomly spread which means there is heteroscedasticity

#The normal QQ plot would be irrelevant because it is used for checking normality the points are close to the line
#But it seems that they donot satisfy normality which is fine because GLMS allow the distribution of response to be non-normal
#and allows distribution to be from the exponential family of distributions. Our distribution would be binomial because we 
#are cheking if the car has a v engine or not.

#The scale location plot shows a pattern so from -4 to 0 the as the predicted values increase the values of 
#residuals also increases but after a point as the predicted values increase the residuals increase and decrease both
#hence it seems there is no homoscedasticity from the residual and fitted plot and scale and location plots.
#As per this link: https://online.stat.psu.edu/stat504/node/216/
#Homoscedasticity does not to be necessarily satisfied for glms

#The residuals vs leverage plot shows there are no outliers as the dashed line for cook's distance is
#not appearing and no point is outside the dashed line

#None of the assumptions seem to be violated.


