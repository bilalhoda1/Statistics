#Instructions

#Please download the smoke.csv dataset. This dataset contains data from 
#Canadian study of mortality by age and smoking status.

#Please run a poisson glm examining the effect of type of smoking (no smoking (no), 
#cigarette only, cigar pipe only, 
#cigaretter plus cigar pipe (known as cigarette plus) and age on the total number of deaths (death). 
#Pop is the total number of people followed from each age category in the study. 
#Make sure to check model fit and pick the best fitting model. 
#Your R script will be due on Thursday June 11th at 11:30pm. 

setwd("D:/bilal's books 8/Lie Detector/Lab assignments/Lab_17")
data <- read.csv('smoking-1.csv')
data
summary(data)
str(data)
#We have 4 variables
#explanatory variables: age, pop, smoke  
#response variable: dead
#two variables are continuous and two are categorical
unique(data$smoke) #The smoke variable has 4 levels

#We plot the histogram of the response variable
hist(data$dead, col='cyan', main='Histogram of Response Variable', xlab='Dead') 
#The data is not normally distributed

#We measure the center 
mean(data$dead) #253.6111
sd(data$dead) #262.5975
var(data$dead) #68957.44
#The variance is quite large and the standard is little higher than the mean.

#We fit a Poisson  glm
glm1 <- glm(dead ~ age + smoke + pop, data, family = poisson(link = "log"),na.action = "na.fail")
plot(glm1,col='blue',pch=16)

#The residual vs fitted plot seems to be random there is no trend
#We ignore the normal qqplot because we are using poisson glm
# The scale location plot shows the points are random with some increase and decrease at some positions
#There are some outliers and in some cases the observations with leverage  are not being plot


#We use maximum likelihood estimation methods for model selection

#For poisson GLM, we use Chi-sq tests instead of F-tests
#We started out with the model with all of the X variables
#Now we remove variables one by one and see if the variance explained for the Y by 
#the model is significantly different

#We remove the smoke variable fromm the full model

glm1.1<-update(glm1, .~. -smoke)
anova(glm1, glm1.1, test = "Chisq")
#Model with smoke explains significantly more variation than model without smoke
#Hence we are keeping smoke in the final model

glm1.2<-update(glm1, .~. -age) 
anova(glm1, glm1.2, test = "Chisq")
##Similarly, model with age explains significantly more variation than model without age
#Hence we are keeping age in the final model

glm1.3<-update(glm1, .~. -pop)
anova(glm1, glm1.3, test = "Chisq")
#Model with pop is also explaining significant variation than model without pop
#So we are keeping this in the final model as well

#In this case, the final model would be the full model

#We can also use the dredge function to do this

library(MuMIn) 
dredge(glm1) #this is the full model

#We can see from the dredge table the minimum AIC value is of the full model

#We use the summary function on full model
summary(glm1) 
#We can see age 70-74 is not significant.
#The row with smokeCigarrettePlus is showing NAs

# We use the summary function on the dataset 
#to get minimum and maximum values for each variable in the dataset
summary(data)

#we create models with one x variable at a time to study the impact of each variable on response
#We make a glm with only pop variable and make predictions using it
x_pop<-seq(90,6100,1)
glm_pop<-glm(dead ~ pop, data = data, family = poisson(link = "log")) 

y_dead<-predict(glm_pop, list(pop = x_pop), type = "response")

#We plot this
plot(dead ~ pop, data = data, xlab = "pop", ylab = "dead",col='blue',pch=16,main='POpulation vs Dead')
points(y_dead, type = "l",col='red')
#We get a sort of an increasing exponential curve with points concentrated at the start
#We can interpret it as the larger the population the more will be the deaths

#we create model with age only
data$age
glm_age<-glm(dead ~ age, data = data, family = poisson(link = "log")) 
y_dead_age<-predict(glm_age, type = "response")
#we make a boxplot of this
plot(dead ~ age, data = data, xlab = "age", ylab = "dead",col='orange',main='Age vs Dead')
points(y_dead_age, type = "l",col='purple')
#We can see people of greater ages have higher average of death specifically 
#people between the age of 60-70

cat<-rep(c("CigarPipeOnly","CigarretteOnly","CigarrettePlus"), 250)
x_smoke<-seq(90,6100,1)
#we create a model with only smoke variable
glm_smoke<-glm(dead ~ smoke, data = data, family = poisson(link = "log")) 
y_dead_smoke<-predict(glm_smoke, list(smoke = cat), type = "response")
#We plot this
plot(dead ~ smoke, data = data, xlab = "smoke", ylab = "dead",col='green',main='Smoke vs Dead')
points(y_dead, type = "l",col='brown')
#We can see that the average number of deaths for cigarette plus and cigarette only is higher
#than cigar pipe only and no

