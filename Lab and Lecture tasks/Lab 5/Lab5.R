# @@ RGEDIT LANDMARK @@: Introduction to data summaries and displays
#Today we will learn how to summarize datasets numerically and graphically
#Remember that the most useful information in any dataset is understanding the center, spread, shape and outliers
#For this lab, you will be calculating data summaries and making histograms, stemplots and boxplots from the following three datasets:
#1) The 1907 Romanian Peasant Rebellion
#2) Toxicity of Nitrofen pesticide in Aquatic Ecosystems
#3) Space Shuttle O - Ring failures

# @@ RGEDIT LANDMARK @@: Dataset 1: 1907 Romanian Peasant Rebellion
#Please create a new project file (with src, doc and output folders)
#Download the .doc file for this dataset and place it in the doc folder
#setting working directory
setwd("D:/bilal's books 8/Lie Detector/Lab assignments/Lab 5")
#loading dataset
chirot<-read.csv("Chirot.csv", header = TRUE)
#let's look at the structure
str(chirot)

#please read the document file and figure out what these variables are

#answer the following questions

#please provide all measures of center for the intensity of rebellion
mean(chirot$intensity)
median(chirot$intensity)
#gives single mode
#getmode <- function(x){
#  uniq <- unique(x)
#  uniq[which.max(tabulate(match(x,uniq)))]
#}
#gives mode values if more than one mode exists
getmode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
getmode(chirot$intensity)
#please provide the standard deviation and variance for intensity of the rebellion
sd(chirot$intensity)
var(chirot$intensity)
#which county had the greatest intensity of rebellion
#county 24
chirot[chirot$intensity == max(chirot$intensity),]
#which county has the least intensity of rebellion
#county 31 and 32
chirot[chirot$intensity == min(chirot$intensity),]

#please provide mean, median and mode for all other variables
mean(chirot$commerce)
mean(chirot$tradition)
mean(chirot$midpeasant)
mean(chirot$inequality)

median(chirot$commerce)
median(chirot$tradition)
median(chirot$midpeasant)
median(chirot$inequality)

getmode(chirot$commerce)
getmode(chirot$tradition)
getmode(chirot$midpeasant)
getmode(chirot$inequality)

#please compute standard deviation and variance for all other variables
sd(chirot$commerce)
sd(chirot$tradition)
sd(chirot$midpeasant)
sd(chirot$inequality)

var(chirot$commerce)
var(chirot$tradition)
var(chirot$midpeasant)
var(chirot$inequality)

#getting the five number summary from summary function
summary(chirot$intensity)

#please compute the five number summary for all other variables
summary(chirot$commerce)
summary(chirot$tradition)
summary(chirot$midpeasant)
summary(chirot$inequality)

#which variables are more spread out (have more variation) as compared to others? 
#midpeasant has the most variance after that 
#commerce has the second most variance
#then tradition, intensity and then finally inequality

#Visualizing data
#making stemplots
stem(chirot$intensity)

#create stemplots for all variables in this dataset
stem(chirot$commerce)
stem(chirot$tradition)
chirot$tradition
stem(chirot$midpeasant)
stem(chirot$inequality)

#making histograms
hist(chirot$intensity, main = "Histogram of Intensity of Rebellion", col = c("blue"))

#what does the data look like (symmetric, bi-modal, uniform, skew (which direction))?
#right skewed because the tail is on the right
#try changing the interval size using the breaks function
hist(chirot$intensity, main = "Histogram of Intensity of Rebellion", col = c("blue"), breaks = 4)
#find the optimal number of breaks that show both the center and extent of spread of the data
hist(chirot$intensity, main = "Histogram of Intensity of Rebellion", col = c("blue"), breaks = 8)
#make historgrams for all variables in this dataset
hist(chirot$commerce, main = "Histogram of Commerce of Rebellion", col = c("blue"), breaks = 8)
hist(chirot$tradition, main = "Histogram of Tradition of Rebellion", col = c("blue"))
hist(chirot$midpeasant, main = "Histogram of Midpeasant of Rebellion", col = c("blue"), breaks = 20)
hist(chirot$inequality, main = "Histogram of Inequality of Rebellion", col = c("blue"))

#making boxplots
boxplot(chirot$intensity, ylab = "Rebellion Intensity", main = "Boxplot of Rebellion Inensity in Romania")
#are there any outliers
#which districts do these belong to? 
# district 24, 25 and 26
out <- subset(chirot,intensity>=3.7)
out
#make boxplot for all other variables 
boxplot(chirot$commerce, ylab = "Rebellion Commerce", main = "Boxplot of Rebellion Commerce in Romania")
boxplot(chirot$tradition, ylab = "Rebellion Tradition", main = "Boxplot of Rebellion Tradition in Romania")
boxplot(chirot$midpeasant, ylab = "Rebellion MidPeasant", main = "Boxplot of Rebellion MidPeasant in Romania")
boxplot(chirot$inequality, ylab = "Rebellion Inequality", main = "Boxplot of Rebellion Inequality in Romania")
#Let's see if there is any relationship between intensity and commerce
#break up commerce into categories (low <20, average: between 20 and 30, high >30) 
commercecat<-NULL
for (i in 1:32){
	if(chirot$commerce[i] < 20){
		commercecat[i]<-"low"
	}
	else if(chirot$commerce[i] >= 20 & chirot$commerce[i] <=30){
		commercecat[i]<- "average"
	}
	else {
		commercecat[i]<- "high"
	}
}

#making a boxplot
boxplot(intensity ~ commercecat, data = chirot, xlab = "Commerce category", ylab = "Rebellion Intensity")
#what does this relationship look like
#what does the spread and center of points in each category tell you about rebellion intensity?
high<- chirot[chirot$commerce>=30,]
high
hist(high$intensity, main = "Histogram of Intensity of Rebellion", col = c("blue"),breaks=5)
#For high commerce the data is quite spread out
#For average commerce the mass is concentrated but the range is high so one of tails are skewed
#For low commerce there is lesser variation
#We can say that there might be some sort of correlation between commerce and intensity
#We will have to observe other variables as well

#break up the rest of the variables into categories
#(hint: use histograms to decide how to break up continuous data into categories)
#create boxplots examining the relationship between these variables and rebellion intensity
summary(chirot$tradition)
#using the five summary statistic to choose the limits for our category
traditioncat<-NULL
for (i in 1:32){
  if(chirot$tradition[i] < 82){
    traditioncat[i]<-"low"
  }
  else if(chirot$tradition[i] >= 82 & chirot$tradition[i] <=88){
    traditioncat[i]<- "average"
  }
  else {
    traditioncat[i]<- "high"
  }
}
boxplot(intensity ~ traditioncat, data = chirot, xlab = "Tradition category", ylab = "Rebellion Intensity")

midpeasantcat <- NULL
summary(chirot$midpeasant)
for (i in 1:32){
  if(chirot$midpeasant[i] < 4){
    midpeasantcat[i]<-"low"
  }
  else if(chirot$midpeasant[i] >= 4 & chirot$midpeasant[i] <=8){
    midpeasantcat[i]<- "average"
  }
  else {
    midpeasantcat[i]<- "high"
  }
}
boxplot(intensity ~ midpeasantcat, data = chirot, xlab = "MidPeasant category", ylab = "Rebellion Intensity")


inequalitycat <- NULL
summary(chirot$inequality)
for (i in 1:32){
  if(chirot$inequality[i] < 0.5){
    inequalitycat[i]<-"low"
  }
  else if(chirot$inequality[i] >= 0.5 & chirot$inequality[i] <=0.7){
    inequalitycat[i]<- "average"
  }
  else {
    inequalitycat[i]<- "high"
  }
}
boxplot(intensity ~ inequalitycat, data = chirot, xlab = "InEquality category", ylab = "Rebellion Intensity")

# @@ RGEDIT LANDMARK @@: Dataset 2: Toxicity of Nitrofen in Aquatic Ecosystems
#loading new dataset
nitro<-read.csv("nitrofen.csv", header = TRUE)
str(nitro)
#please read the document file and figure out what each variable means

#find all measures of center, standard deviation and variance for all variables except one (figure out which on this is)
#find the five number summary of all of the variables except the excluded variable
mean(nitro$brood1)
mean(nitro$brood2)
mean(nitro$brood3)
mean(nitro$total)

median(nitro$brood1)
median(nitro$brood2)
median(nitro$brood3)
median(nitro$total)

getmode(nitro$brood1)
getmode(nitro$brood2)
getmode(nitro$brood3)
getmode(nitro$total)


sd(nitro$brood1)
sd(nitro$brood2)
sd(nitro$brood3)
sd(nitro$total)

var(nitro$brood1)
var(nitro$brood2)
var(nitro$brood3)
var(nitro$total)


#which variable has the least spread? Which variable has the most spread? 
#The variance of conc variable is the largest w.r.t its mean
#The variance of brood1 variable is the lowest w.r.t its mean

#make stemplots for concentration of nitrofen
stem(nitro$conc)
#make histograms for each variable (except excluded)
hist(nitro$brood1, main = "Histogram of Nitrofen Brood1", col = c("blue"),breaks=10)
hist(nitro$brood2, main = "Histogram of Nitrofen Brood2", col = c("blue"),breaks=20)
hist(nitro$brood3, main = "Histogram of Nitrofen Brood3", col = c("blue"),breaks=20)
hist(nitro$total, main = "Histogram of Nitrofen Total", col = c("blue"),breaks=10)
#what is the shape of the data for each variable
#For brood1 the shape is left skewed bell shaped 
#For brood2 the shape is bimodal 
#For brood3 the shape is multimodal
#For total the shape is bimodal

#make singular boxplots for each variable (except excluded)

#break up concentration into categories 
#create a boxplot to examine the effect of different levels of nitrofen concentration 
#on live offspring produced in each brood and total


# @@ RGEDIT LANDMARK @@: Dataset 3: Space shuttle O - ring failures
#loading new dataset
space<-read.csv("SpaceShuttle.csv", header = TRUE)
str(space)
#please read the document file and figure out what each variable means

#find all measures of center, standard deviation and variance for all variables except two (figure out which on this is)
#find the five number summary of all of the variables except the excluded variables

#which variable has the least spread? Which variable has the most spread? 

#make stemplots for temperature and pressure

#make histograms for each variable (except excluded)
#what is the shape of the data for each variable

#make singular boxplots for each variable (except excluded)

#create boxplots to examine the effect of temperature and pressure on the failures of O-rings in space shuttles





