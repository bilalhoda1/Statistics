#setwd() to set working directory
#setwd("D:/bilal's books 8/Lie Detector/Lab assignments/Lab 2 assignment")

#installed mlbench package using install.packages('mlbench')

#used library command to load the package
library(mlbench)

#used data to load the PimaIndiansDiabetes dataset
data(PimaIndiansDiabetes)
pima <- PimaIndiansDiabetes
pima

#Question 1

#using str function to get a summary of the dataset 
#the summary would contain number of observations, variables, class of each variable
#There are 9 variables, 768 observations in the dataset
#The class of diabetes is factor and the class of remaining variables is num
str(pima)

#Question 2
#We use the subset function which returns 
#subsets of vectors, matrices or data frames which meet conditions.

#DataFrame1
subset(pima,pregnant >5 & glucose>150)

#nrow(subset(pima,pregnant >5 & glucose>150))

#DataFrame2
#assuming to remove means selecting all those rows which donot have mass between 30 and 40
subset(pima,mass<30 | mass>40)

#nrow(subset(pima,mass<30 | mass>40))

#DataFrame3
subset(pima,age<50 & insulin<400)

#nrow(subset(pima,age<50 & insulin<400))

#DataFrame4
subset(pima,pedigree>1 & diabetes=='neg' & pressure>80)

#nrow(subset(pima,pedigree>1 & diabetes=='neg' & pressure>80))

#Question 3
#Assuming that removing the values means replacing the values with NA otherwise it doesn't make sense
#So there was a column with strings so when the matrix was formed it converted all the columns to numbers
#positive changed into 2 and negative changed into 1 in the diabetes column
#using data.matrix to convert data frame into matrix 
#we selected the first 100 observations and the specified columns using [1:100,c(1,2,3,5,6,8,9)]

mat <- data.matrix(pima[1:100,c(1,2,3,5,6,8,9)])
mat
mat[mat==10 | mat==25 | mat==45 | mat==60] <- NA
mat

#Question 4
#The mean glucose levels of women between age 20 and 30 is 114.1751
meanGluAge <- 0
count <- 0
for (i in 1:nrow(pima)){
  if (pima$age[i]>=20 & pima$age[i]<=30){
    meanGluAge <- meanGluAge + pima$glucose[i] 
    count <- count + 1
    }
}
if(count==0){
  meanBetween <- 0
} else {
  meanBetween <- meanGluAge/count
  
}
meanBetween
#The mean glucose level of all women is 120.8945
meanGlu <- mean(pima$glucose)
meanGlu

#normal blood sugar level is between 4.0 to 5.4 mmol/L (72 to 99 mg/dL) when fasting
#Up to 7.8 mmol/L (140 mg/dL) 2 hours after eating (Oral glucose tolerance test)

#The mean glucose level is 120.8945 and the mean for women between 20-30 is 114.1751 
#Both of which lie in the normal range 

#References: https://www.diabetes.co.uk/diabetes_care/blood-sugar-level-ranges.html
#https://www.mayoclinic.org/diseases-conditions/diabetes/diagnosis-treatment/drc-20371451

#Question 5
#assigned N to those with pregnant values 0 and Y to greater than 0 and stored in a vector
pregnancy <- NULL
for (i in 1:nrow(pima)){
  if (pima$pregnant[i]==0){
    pregnancy[i] <- 'N'
  }
  else if (pima$pregnant[i]>0){
    pregnancy[i] <- 'Y'
  }
}
pregnancy
#Created a new column pregnancy and assigned it the vector pregnancy
pima$pregnancy <- pregnancy
pima
#To find how many women were pregnant we will use the comparison pregnancy == 'Y'
#and then calculate the total number of rows using nrow
#657 women have been pregnant
nrow(pima[pima$pregnancy=='Y',])
#We will use similar approach to find non pregnant women
#111 women were not pregnant
nrow(pima[pima$pregnancy=='N',])

#Question 6
pressureType <- NULL
for (i in 1:nrow(pima)){
  if (pima$pressure[i]>80){
    pressureType[i] <- 'high'
  }
  else if (pima$pressure[i]>=40 & pima$pressure[i]<=80){
    pressureType[i] <- 'average'
  }
  else if (pima$pressure[i]<40){
    pressureType[i] <- 'low'
  }
}
pressureType
pima$pressuretype <- pressureType

#165 women have high blood pressure
nrow(pima[pima$pressuretype=='high',])

#564 women have average blood pressure
nrow(pima[pima$pressuretype=='average',])

#39 women have low blood pressure
nrow(pima[pima$pressuretype=='low',])

#77 women having high blood pressure are diabetic
nrow(pima[pima$pressuretype=='high' & pima$diabetes=='pos',])

#Question 7
minAge <- min(pima$age)
minAge

maxAge <- max(pima$age)
maxAge
#to make the bins of size 5, adjusting the max value of the loop
loop <- maxAge+(5-length(minAge:maxAge)%%5)
#initializing variables
count <-1
pregnancies <- 0
meanPregnancy <- 0
maximum <- 0
ageYear <- 0
rows <- 0

#running the loop from min age to max age

#maintaining a count so that when 5 years are done i.e. count%%5==0 print the average number of pregnancies and age with maximum pregnancy in the 5 year
#after that resetting all the values

for(i in minAge:loop){
  #finding age wise sum of pregnancies
  current <- sum(pima[pima$age==i,]$pregnant)
  pregnancies <- pregnancies + current
  #maintaining a record of number of entries for that particular age
  rows <- rows + nrow(pima[pima$age==i,])
  
  #if current sum of pregnancies is greater than the maximum update maximum 
  if(current > maximum){
    maximum <- current
    ageYear <- i
  }
  #if count becomes 5 then outputting results for 5 years
  if(count%%5 ==0){
    #if no pregnancy in 5 years then the number of rows would be 0 so putting a check to detect 0/0 division
    if (rows==0){
      meanPregnancy <- 0
    }else {
      meanPregnancy <- pregnancies/rows      
    }
    #outputting results
    print(paste("For age",i-4,'-',i,'the maximum number of pregnancies is',maximum, 'for age',ageYear))
    print(paste("For age",i-4,'-',i,'the average number of pregnancies is',meanPregnancy))
    count <- 0
    pregnancies <- 0
    rows <- 0
    maximum <- 0
    ageYear <- 0
  }
  count <- count + 1
}

#Question 8

#excluding non-diabetic people
nonDiab <- pima[pima$diabetes=='neg',]
nonDiab

minAge <- min(nonDiab$age)
minAge

maxAge <- max(nonDiab$age)
maxAge

#to make bins of size 10, adjusting the max value of the loop
loop <- maxAge+(10-length(minAge:maxAge)%%10)
#initializing variables
count <-1
pregnancies <- 0
meanPregnancy <- 0
maximum <- 0
ageYear <- 0
rows <- 0

#running the loop from min age to max age

#maintaining a count so that when 10 years are done i.e. count%%10==0 print the average number of pregnancies and age with maximum pregnancy in the 10 years
#after that resetting all the values

for(i in minAge:loop){
  #finding age wise sum of pregnancies
  current <- sum(nonDiab[nonDiab$age==i,]$pregnant)
  pregnancies <- pregnancies + current
  #maintaining a record of number of entries for that particular age
  rows <- rows + nrow(nonDiab[nonDiab$age==i,])
  
  #if current sum of pregnancies is greater than the maximum update maximum 
  if(current > maximum){
    maximum <- current
    ageYear <- i
  }
  #if count becomes 10 then outputting results for 10 years
  if(count%%10 ==0){
    #if no pregnancy in 10 years then the number of rows would be 0 so putting a check to detect 0/0 division
    if (rows==0){
      meanPregnancy <- 0
    }else {
      meanPregnancy <- pregnancies/rows      
    }
    #outputting results
    print(paste("For age",i-9,'-',i,'the maximum number of pregnancies is',maximum, 'for age',ageYear))
    print(paste("For age",i-9,'-',i,'the average number of pregnancies is',meanPregnancy))
    count <- 0
    pregnancies <- 0
    rows <- 0
    maximum <- 0
    ageYear <- 0
  }
  count <- count + 1
}
#Question 9 
#installed plyr using install.packages('plyr')
#loaded the package using library
library(plyr)

#Plyr has functions for operating on lists, data.frames and arrays (matrices, or n-dimensional vectors). 
#Each function performs:
#A splitting operation
#Apply a function on each split in turn.
#Recombine output data as a single data object.

#Part i
#The first argument we gave was the data.frame we wanted to operate on: in this case the pima data.
#The second argument indicated our split criteria: in this case the “pressuretype” column.
#The third argument is the function we want to apply to each grouping of the data. 

#Mean
#average 117.8387
#high 132.5697
#low 115.6923
ddply(
  .data = pima,
  .variables = "pressuretype",
  .fun = function(x) mean(x$glucose)
)

#Median
#average 112
#high 131
#low 115
ddply(
  .data = pima,
  .variables = "pressuretype",
  .fun = function(x) median(x$glucose)
)

#sd
#average 32.25350
#high 29.42917
#low 26.91718
ddply(
  .data = pima,
  .variables = "pressuretype",
  .fun = function(x) sd(x$glucose)
)

#minimum
#average 0
#high 61
#low 73
ddply(
  .data = pima,
  .variables = "pressuretype",
  .fun = function(x) min(x$glucose)
)

#maximum
#average 199
#high 196
#low 183
ddply(
  .data = pima,
  .variables = "pressuretype",
  .fun = function(x) max(x$glucose)
)


#Part ii
#considering pressure type and pregnancy together
#Not putting down values in comments otherwise it would take a lot of space
#and would be tedious

#Mean
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy'),
  .fun = function(x) mean(x$mass)
)

#Median
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy'),
  .fun = function(x) median(x$mass)
)

#sd
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy'),
  .fun = function(x) sd(x$mass)
)

#minimum
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy'),
  .fun = function(x) min(x$mass)
)

#maximum
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy'),
  .fun = function(x) max(x$mass)
)

#Part iii
#considering blood pressure types, pregnancy categories, and diabetes categories together

#Insulin level
#Mean
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy','diabetes'),
  .fun = function(x) mean(x$insulin)
)  

#Glucose level
#Mean
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy','diabetes'),
  .fun = function(x) mean(x$glucose)
)  

#Insulin level
#Median
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy','diabetes'),
  .fun = function(x) median(x$insulin)
)  

#Glucose level
#Median
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy','diabetes'),
  .fun = function(x) median(x$glucose)
)  

#Insulin level
#sd
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy','diabetes'),
  .fun = function(x) sd(x$insulin)
)  

#Glucose level
#sd
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy','diabetes'),
  .fun = function(x) sd(x$glucose)
)

#Insulin level
#minimum
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy','diabetes'),
  .fun = function(x) min(x$insulin)
)  

#Glucose level
#minimum
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy','diabetes'),
  .fun = function(x) min(x$glucose)
)

#Insulin level
#maximum
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy','diabetes'),
  .fun = function(x) max(x$insulin)
)  

#Glucose level
#maximum
ddply(
  .data = pima,
  .variables = c("pressuretype",'pregnancy','diabetes'),
  .fun = function(x) max(x$glucose)
)

#Question 10
#the plot function makes a plot
#the legend function is used to make a legend for the plot

#part i
plot(glucose~insulin, data = pima,  type='p',pch=19,xlab="Insulin level (mu U/ml)", ylab="Glucose level (mg/dL)", col=c("red", "blue")[pima$diabetes], main="Insulin vs Glucose level of diabetic and non-diabetic women")
legend("topright", inset=c(0,0),legend = c(levels(pima$diabetes)), col = c("red", "blue"), pch = c(19,19))

#part ii
plot(insulin~pedigree, data = pima,  type='p',pch=19,xlab="Pedigree factor", ylab="Insulin level (mu U/ml)", col=c("red", "blue")[pima$diabetes], main="Pedgiree factor vs Insulin level of diabetic and non-diabetic women")
legend("topright", inset=c(0,0),legend = c(levels(pima$diabetes)), col = c("red", "blue"), pch = c(19,19))

#part iii

#encoded the pregnancy labels Y and N to 17 and 16 which corresponds to triangle and circle 
#triangle indicates pregnant
#circle indicates not pregnant
#red indicates non-diabetic 
#blue indicates diabetic

shapes = c(16, 17) 
shapes <- shapes[as.numeric(as.factor(pima$pregnancy))]

plot(pressure~mass, data = pima,  type='p',pch=c(shapes),xlab="Body Mass Index (weight in kg/(height in m)^2)", ylab="Blood Pressure (mm Hg)", col=c("red", "blue")[pima$diabetes], main="BMI vs Blood Pressure")
legend("topright", inset=c(0,0),legend = c(unique(pima$pregnancy),levels(pima$diabetes)), col = c("black",'black',"red", "blue"), pch = c(unique(shapes),15,15))

