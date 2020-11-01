#Used setwd to set the working directory
setwd("D:/bilal's books 8/Lie Detector/Lab assignments/Lab 1 assignment")

#Question 1

#read.csv to read file and stored in a variable
gapminder <- read.table(file = "data/gapminder.csv", header = TRUE, sep = ",")

#Used the str function to get a summary of the whole dataset.
#The str function gives information regarding number of observations, number of variables etc
#Our data has 6 variables country,year,pop, continent, lifeExp, gdpPercap
#Our data has three different types of classes factor, integer and numeric
str(gapminder)

#Question 2
#Used the levels function to get the levels from the dataset
#There are 142 countries
#Five continent are represented ~ North and South America is represented as Americas
#Antarctica is Missing
#The reason why Antarctica is not included because it has little/no population hence it wouldn't have life Expectancy and gdp info 
levels(gapminder$country)
levels(gapminder$continent)

#Question 3
#min function gives minimum and max function gives maximum from a set of values
#The oldest time point can be found using the min function because the years are integers so the minimum integer would represent the oldest time point
#The oldest time point is 1952
oldest <- min(gapminder$year)
oldest

#The newest time point can be found using the max function because the years are integers so the maximum integer would represent the recent time point
#The recent time point is 2007
recent <- max(gapminder$year)
recent

#Question 4
#the subset function is used to select variables and observations
#to find most populous country we first find the maximum population 
#and then find the countries having maximum population using subset function 
#China is the most populous country

most <- max(gapminder$pop)
most
subset(gapminder, pop==most)


#to find least populous country we first find the minimum population 
#and then find the countries having minimum population using subset function 
#Sao Tome and Principe is the least populous country 
least <- min(gapminder$pop)
least
subset(gapminder, pop==least)

#Question 5
# by(data,categories,function) 
#by splits the data by categories and applies the function to each group

#mean life expectancy of each country 
by(gapminder$lifeExp,gapminder$country,mean)

#median life expectancy of each country
by(gapminder$lifeExp,gapminder$country,median)

#mean life expectancy of each continent
by(gapminder$lifeExp,gapminder$continent,mean)

#median life expectancy of each cotinent
by(gapminder$lifeExp,gapminder$continent,median)

#which.max gives where the maximum value is
#which.min gives where the minimum value is
#so we first get the means/standard-deviation of all countries/continents by using 'by' and 
#then we get the position of max and min mean/standard-deviation using which.max and which.min respectively 
#finally we use this position to get the value of mean/standard-deviation in the dataframe outputted by 'by'

#country and continent having the greatest mean life expectancy
#Country: Iceland
gCountry <- by(gapminder$lifeExp,gapminder$country,mean)
gCountry[which.max(gCountry)]

#Continent: Oceania
gContinent <- by(gapminder$lifeExp,gapminder$continent,mean)
gContinent[which.max(gContinent)]

#country and continent having the least mean life expectancy
#Country: Sierra Leone 
lCountry <- by(gapminder$lifeExp,gapminder$country,mean)
lCountry[which.min(lCountry)]

#Continent: Africa 
lContinent <- by(gapminder$lifeExp,gapminder$continent,mean)
lContinent[which.min(lContinent)]

#country having the most variation in life expectancy across time
#Country:Oman
mVariation <- by(gapminder$lifeExp,gapminder$country,sd)
mVariation[which.max(mVariation)]

#country having the least variation in life expectancy across time
#Country: Denmark
lVariation <- by(gapminder$lifeExp,gapminder$country,sd)
lVariation[which.min(lVariation)]

#Question 6
# by(data,categories,function) 
#by splits the data by categories and applies the function to each group

#mean gdp per capita of each country 
by(gapminder$gdpPercap,gapminder$country,mean)

#median gdp per capita of each country
by(gapminder$gdpPercap,gapminder$country,median)

#mean gdp per capita of each continent
by(gapminder$gdpPercap,gapminder$continent,mean)

#median gdp per capita of each cotinent
by(gapminder$gdpPercap,gapminder$continent,median)

#Assuming that the greatest mean gdp per capita is required because otherwise year wise would have been mentioned
#We use the same functions as question 5

#country and continent having the greatest mean gdp per capita
#Country: Kuwait
gCount <- by(gapminder$gdpPercap,gapminder$country,mean)
gCount[which.max(gCount)]

#Continent: Oceania
gCont <- by(gapminder$gdpPercap,gapminder$continent,mean)
gCont[which.max(gCont)]

#country and continent having the least mean gdp per capita
#Country: Myanmar 
lCount <- by(gapminder$gdpPercap,gapminder$country,mean)
lCount[which.min(lCount)]

#Continent: Africa
lCont <- by(gapminder$gdpPercap,gapminder$continent,mean)
lCont[which.min(lCont)]

#country having the most variation in gdp per capita across time
#Country: Kuwait
mVar <- by(gapminder$gdpPercap,gapminder$country,sd)
mVar[which.max(mVar)]

#country having the least variation in gdp per capita across time
#Country: Ethiopia
lVar <- by(gapminder$gdpPercap,gapminder$country,sd)
lVar[which.min(lVar)]


#Question 7
#the plot function makes a plot
#the legend function is used to make a legend for the plot
plot(lifeExp~gdpPercap, data = gapminder,  type='p',pch=19,xlab="gdp per capita (x)", ylab="life expectancy (y)", col=c("red", "blue", "green","orange","purple")[gapminder$continent], main="Gdp Per Capita vs Life Expectancy")
legend("topright", inset=c(0,0),legend = c(levels(gapminder$continent)), col = c("red", "blue", "green","orange","purple"), pch = c(19,19,19))
  