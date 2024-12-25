# Instructions
# Please download the smoke.csv dataset. 
# This dataset contains data from Canadian study of mortality by age and smoking status. 
# 
# Please run a poisson glm examining the effect of type of 
# smoking (no smoking (no), cigarette only, cigar pipe only, cigaretter plus cigar pipe (known as cigarette plus) and 
# age on the total number of deaths (death). Pop is the total number of people followed from each age category in the study. 
# Make sure to check model fit and pick the best fitting model. Your R script will be due on Thursday June 11th at 11:30pm. 

#predictor variables: age, pop, smoke - response variable: dead

data <- read.csv("smoking-1.csv")
names(data) # Extract column names from dataframe
ls.str(data)

#dependent variable dead data continuity by creating a histogram:
hist(data$dead) #Clearly, the data is not in the form of a bell curve like in a normal distribution.

#mean and variance of the dependent variable
mean(data$dead) #253.6111
var(data$dead) #68957.44
#The variance is much greater than the mean, which suggests that we will have over-dispersion in the model.

#Lets fit the Poisson model: using glm
glm1 <- glm(dead ~ age + smoke + pop, data, family = poisson(link = "log"),na.action = "na.fail")
plot(glm1)

#model selection: using maximum likelihood estimation methods

glm1.1<-update(glm1, .~. -age) 
anova(glm1, glm1.1, test = "Chisq")
##Model with age explains significantly more variation than model without
#Keeping age in the final model

glm1.2<-update(glm1, .~. -smoke)
anova(glm1, glm1.2, test = "Chisq")
#Model with smoke explains significantly more variation than model without
#Keeping smoke in the final model

glm1.3<-update(glm1, .~. -pop)
anova(glm1, glm1.3, test = "Chisq")
#pop is also explaining significant variation in Y
#So we are keeping this in the final model as well

#In this case, the final model is the full model

summary(glm1) #age 70-74 is not significant.


summary(data) ##getting minimum and maximum values for each variable in dataset
x_pop<-seq(90,6100,1)
glm_pop<-glm(dead ~ pop, data = data, family = poisson(link = "log")) #we need to create model with one x variable at a time
y_dead<-predict(glm_pop, list(pop = x_pop), type = "response")
#let's plot this
plot(dead ~ pop, data = data, xlab = "pop", ylab = "dead")
points(y_dead, type = "l")
#inverse relation trend

summary(data)
data$age

glm_age<-glm(dead ~ age, data = data, family = poisson(link = "log")) #we need to create model with one x variable at a time
y_dead_age<-predict(glm_age, type = "response")
#let's plot this
plot(dead ~ age, data = data, xlab = "age", ylab = "dead")
points(y_dead_age, type = "l")

f<-rep(c("CigarPipeOnly","CigarretteOnly","CigarrettePlus"), 250)
x_smoke<-seq(90,6100,1)
glm_smoke<-glm(dead ~ smoke, data = data, family = poisson(link = "log")) #we need to create model with one x variable at a time
y_dead_smoke<-predict(glm_smoke, list(smoke = f), type = "response")
#let's plot this
plot(dead ~ smoke, data = data, xlab = "smoke", ylab = "dead")
points(y_dead, type = "l")




