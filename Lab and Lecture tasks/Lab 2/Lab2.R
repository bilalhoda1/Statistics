#Today's R lab will focus on building data manipulation skills and introduce you to Rmarkdown as a way of generating documents in R. Please note that this script and lesson are adpated from software carpentry and Queen's University R Club 
#Let's get started with learning more tips and trick for data manipulation
#But first a review of subsetting data
#We are going to use the gapminder dataset

#Make sure to create a new project called Lab 2 and set the working directory to it. 
#please make sure that the gapminder.csv file is in the Lab 2 folder since this folder is the working directory. We will call this csv file from this folder

#Setting working directory (this will be different on your machine)
setwd("/home/sarah/Documents/Habib/Math204/Lab")


# @@ RGEDIT LANDMARK @@: Subsetting data
#Creating a new vector and assigning names to each element
x <- c(5.4, 6.2, 7.1, 4.8, 7.5)
names(x) <- c('a', 'b', 'c', 'd', 'e')

#calling the first element in the vector x
x[1]
#output provides both the name and the value of the first element

#calling the fourth element in the vector x
x[4]

#asking for multiple elements at once
x[c(1, 3)] #here we are asking for both the first and the third element in the vector X

#asking for elements 1 to 4 using the colon
x[1:4]

#asking for the same element multiple times
x[c(1,1,3)]

#If we ask for an index beyond the length of the vector, R will return a missing value
x[6]
#This is a vector of length one containing an NA, whose name is also NA.

#If we ask for the 0th element, we get an empty vector
x[0]

#If we use a negative number, R will return every element of the vector, EXCEPT the number in brackets
x[-2]

#skipping multiple elements in a vector
x[c(-1, -5)]

#what if we want to remove/negate a sequence in a vector?
x[-1:3]

#oh oh. We get a cryptic error
#Why? Because : is a function for which we have defined -1 as the first argument and 3 (positive) as the second argument, so  it generates the sequence of numbers: c(-1, 0, 1, 2, 3).The correct solution is to wrap that function call in brackets, so that the - operator applies to the result:

x[-(1:3)]

#to remove elements from a vector we can reassign the same variable with the updated vector like this:
x <- x[-4]
x

#Task 1
#With the following code 
x <- c(5.4, 6.2, 7.1, 4.8, 7.5)
names(x) <- c('a', 'b', 'c', 'd', 'e')
print(x)

#use at least two commands above to produce the following output: 
#Output
# b   c   d 
#6.2 7.1 4.8 

#we can extract elements by their name
x <- c(a=5.4, b=6.2, c=7.1, d=4.8, e=7.5) # we can name a vector 'on the fly'
x[c("a", "c")]

#We can also subset using logical operators
x[c(FALSE, FALSE, TRUE, FALSE, TRUE)]
#don't include elements 1, 2, and 4

#we can call specific values as well
x[x > 7]

#we can also call elements by name
x[names(x) == "a"]

#Task: Given the code below, write a subsetting command that pulls out values greater than 4 and less than 7

x <- c(5.4, 6.2, 7.1, 4.8, 7.5)
names(x) <- c('a', 'b', 'c', 'd', 'e')
print(x)

#It is possible for different elements in a vector to have the same name
x <- 1:3
x
#assigning the same name to all elements
names(x) <- c('a', 'a', 'a') 
x

#calling a
x['a'] # only returns first value
x[names(x) == 'a']  # returns all three values

#how to skip or remove names elements in a vector
x <- c(a=5.4, b=6.2, c=7.1, d=4.8, e=7.5) # we start again by naming a vector 
x[-"a"] #when we try this, we get an error. Why?

#Let's use the not equals operator to construct a logical vector that does what we want
x[names(x) != "a"] #skips/removes a
x[names(x)!=c("a","c")] #skips/removes a and c

z<-names(x) != c("a", "c")
z
#Why does R give TRUE as the third element of this vector, when names(x)[3] != "c" is obviously false? 
#When you use !=, R tries to compare each element of the left argument with the corresponding element of its right argument.
#When one vector is shorter than the other, the shorter vector get recycled. 
#In this case R repeats c("a", "c") as many times as necessary to match names(x), i.e. we get c("a","c","a","c","a")
#Since the recycled "a" doesn’t match the third element of names(x), the value of != is TRUE.
#Because in this case the longer vector length (5) isn’t a multiple of the shorter vector length (2), R printed a warning message. 
#f we had been unlucky and names(x) had contained six elements, R would silently have done the wrong thing (i.e., not what we intended it to do).
#This recycling rule can can introduce hard-to-find and subtle bugs!

#The way to get R to do what we really want (match each element of the left argument with all of the elements of the right argument) it to use the %in% operator. 
#The %in% operator goes through each element of its left argument, in this case the names of x, and asks, “Does this element occur in the second argument?”. 
#Here, since we want to exclude values, we also need a ! operator to change “in” to “not in”:
x[! names(x) %in% c("a","c") ]

#At some point you will encounter functions in R that cannot handle missing, infinite, or undefined data.

#There are a number of special functions you can use to filter out this data:

#is.na() will return all positions in a vector, matrix, or data.frame containing NA (or NaN)
#is.nan(), and is.infinite() will do the same for NaN and Inf.
#is.finite will return all positions in a vector, matrix, or data.frame that do not contain NA, NaN or Inf.
#na.omit will filter out all missing values from a vector

#Subsetting a factor
f <- factor(c("a", "a", "b", "c", "c", "d"))
f[f == "a"]

f[f %in% c("b", "c")]

f[1:3]

f[-3]

#Matrix subsetting
#Matrices are also subsetted using the [ function. 
#In this case it takes two arguments: the first applying to the rows, the second to its columns:
set.seed(1)
m <- matrix(rnorm(6*4), ncol=4, nrow=6)
m[3:4, c(3,1)]

#You can leave the first or second arguments blank to retrieve all the rows or columns respectively:
m[, c(3,4)]

#If we only access one row or column, R will automatically convert the result to a vector:
m[3,]

#If you want to keep the output as a matrix, you need to specify a third argument; drop = FALSE:
m[3, , drop=FALSE]

#Unlike vectors, if we try to access a row or column outside of the matrix, R will throw an error:
m[, c(3,6)]

#Because matrices are understood as group of vectors, we can also subset vectors using only one argument:
m[5]

#matrices are laid out in column-major format by default. The elements of the vector are arranged column-wise:
matrix(1:6, nrow=2, ncol=3)

#If you wish to populate the matrix by row, use byrow=TRUE
matrix(1:6, nrow=2, ncol=3, byrow=TRUE)
#Matrices can also be subsetted using their rownames and column names instead of their row and column indices.

#Task 4
#Which of the following commands will extract the values 11 and 14?

# A) m[2,4,2,5]
#B) m[2:5]
#C) m[4:5,2]
#D) m[2,c(4,5)]

m <- matrix(1:18, nrow=3, ncol=6)
print(m)

#List Subsetting
#There are three functions used to subset lists: [, [[, $

#Using [ will always return a list. 
#If you want to subset a list, but not extract an element, use [.
iris <- read.csv("iris.csv", header = TRUE)
xlist <- list(a = "Lie Detector", b = 1:25, data = head(iris))
xlist[1]

#subset elements of a list exactly the same way as atomic vectors using [
#Comparison operations however won’t work as they’re not recursive, 
#they will try to condition on the data structures in each element of the list, 
#not the individual elements within those data structures.

xlist[1:2]
#use the double-square bracket function [[ to extract individual elements of a list
xlist[3]
xlist[[3]]
#compare the results of the two function above? What is different?

#Notice that the result is a vector
#We cannot use two square brackets to extract more than one individual element
xlist[[1:2]]

#And we can't skip elements
xlist[[-1]]

#use names to both subset and extract elements:
xlist[["b"]]

#$ function is a shorthand way for extracting elements by name:
xlist$data

#Review: Subsetting data frames
gap <- read.csv("gapminder.csv")
head(gap[["lifeExp"]]) #extracts the first few values for the life expectancy column

#$ provides a convenient shorthand to extract columns by name
head(gap$year) #extracts the first few values of the column year

#With two arguments, [ behaves the same way as for matrices:
gap[1:3,]

#If we subset a single row, the result will be a data frame
gap[3,]

#Task
#Fix each of the following common data frame subsetting errors:
#Extract observations collected for the year 1957
gap[gap$year = 1963,]

#Extract all columns except 1 through to 4
gap[,-1:4]

#Extract the rows where the life expectancy is longer the 80 years
gap[gap$lifeExp > 80]

#Extract the first row, and the fourth and fifth columns (continent and lifeExp).
gap[1, 4, 5]

#Advanced: extract rows that contain information for the years 2002 and 2007
gap[gap$year == 2002 | 2007,]

#Note that indexing in R starts at 0, not 1


# @@ RGEDIT LANDMARK @@: Using if/else statements

#If is a conditional statement used to instruct R to do something ONLY if a certain condition is met
#it takes the form

#if
#Below is NOT code. Please do not execute
if (condition is true) {
  perform action
}
#another conditional statement is 
# if ... else
#Below is NOT code. Please do not execute
if (condition is true) {
  perform action
} else {  # that is, if the condition is false,
  perform alternative action
}

#Suppose we want R ot print a specific message when variable x has a specific value
x <- 2

if (x >= 18) {
  print("x is greater than or equal to 18")
}

x

#The print statement does not appear in the console because x is not greater than 10. 
#We can add an else statement so that a message appears when values are less than 10
x <- 4

if (x >= 18) {
  print("x is greater than or equal to 18")
} else {
  print("x is less than 18")
}

#Testing multiple conditions
x <- 5

if (x >= 20) {
  print("x is greater than or equal to 20")
} else if (x > 10) {
  print("x is greater than 10, but less than 20")
} else {
  print("x is less than 10")
}

#when R evaluates the condition inside if() statements, it is looking for a logical element, i.e., TRUE or FALSE

#Task: Use an if() statement to print a suitable message reporting whether there are any records from 1998 in the gapminder dataset. Now do the same for 2010.

#R also has a built in ifelse function which make it easier to program if/else statements
# ifelse function 
#Below is NOT code. Please do not execute
ifelse(condition is true, perform action, perform alternative action) 

y <- -10
ifelse(y < -1, "y is a negative number", "y is either positive or zero")

#Two more useful functions
#any() function will return TRUE if at least one TRUE value is found within a vector, otherwise it will return FALSE
#all() will only return TRUE if all values in the vector are TRUE.

# @@ RGEDIT LANDMARK @@: Repeating operations using for loops
#If you want to iterate over a set of values, when the order of iteration is important, and perform the same operation on each, a for() loop will do the job. 
#most flexible of looping operations, but therefore also the hardest to use correctly.
#avoid using for() loops unless the order of iteration is important: i.e. the calculation at each iteration depends on the results of previous iterations.
#your order of iteration is not important, then you should learn about vectorized alternatives, such as the purr package, 
#as they are more computationally efficient which saves time when used on very large datasets

#basic structure of a for loop
#Below is NOT code. Please do not execute
for(iterator in set of values){
  do a thing
}

#try this out
for(i in 1:100){
  print(i)
}

#we can use a for() loop nested inside another for() loop to iterate over two things at once.

for(i in 1:26){
  for(j in LETTERS){
    print(paste(i,j))
  }
}

#when the first index (i) is set to 1, the second index (j) iterates through its full set of indices. 
#Once the indices of j have been iterated through, then i is incremented.

#Let's create a for loop to write the output of the earlier loop into a new object

output_vector <- c() #creating an empty vector
for(i in 1:26){
  for(j in LETTERS){
    output_temp <- paste(i, j)
    output_vector <- c(output_vector, output_temp)
  }
}
output_vector

#One of the biggest things that trips up novices and experienced R users alike, 
#is building a results object (vector, list, matrix, data frame) as your for loop progresses. 
#Computers are very bad at handling this,
#so programs become very slow
#It’s much better to define an empty results object before hand of appropriate dimensions, 
#rather than initializing an empty object without dimensions.

output_matrix <- matrix(nrow=5, ncol=5) #creating an empty matrix with 5 rows and 5 columns
j_vector <- LETTERS[1:5]
for(i in 1:5){
  for(j in 1:5){
    temp_j <- j_vector[j]
 temp_output <- paste(i, temp_j)
    output_matrix [i, j] <- temp_output
  }
}
output_vector2 <- as.vector(output_matrix)
output_vector2


# @@ RGEDIT LANDMARK @@: while loops
#Sometimes you will find yourself needing to repeat an operation as long as a certain condition is met. 
#For this, use a while loop
#Below is NOT code. Please do not execute
while(this condition is true){
  do a thing
}

#try this
z <- 1
while(z > 0.1){
  z <- runif(1) #generating random numbers from a uniform distribution
  cat(z, "\n")
}
#while loop that generates random numbers from a uniform distribution (the runif() function) between 0 and 1 until it gets one that’s less than 0.1.

#Task #1: Compare the objects output_vector and output_vector2. Are they the same? If not, why not? How would you change the last block of code to make output_vector2 the same as output_vector?

#Task #2: Write a script that loops through the gapminder data by continent and prints out whether the mean life expectancy is smaller or larger than 40 years.

#Task #3:Modify the script from Task 2 to loop over each country. This time print out whether the life expectancy is smaller than 40, between 40 and 60, or greater than 60.


# @@ RGEDIT LANDMARK @@: Using the plyr package to split gapminder data
library('plyr')
#if plyr is not installed, install it using install.packages("plyr")

#Plyr has functions for operating on lists, data.frames and arrays (matrices, or n-dimensional vectors). 
#Each function performs:
#A splitting operation
#Apply a function on each split in turn.
#Recombine output data as a single data object.

#The functions are named based on the data structure they expect as input, 
#and the data structure you want returned as output: [a]rray, [l]ist, or [d]ata.frame. 
#The first letter corresponds to the input data structure, 
#the second letter to the output data structure, and then the rest of the function is named “ply”.

#using the plyr package, we can calculate the mean GDP per continent
ddply(
 .data = gap,
 .variables = "continent",
 .fun = function(x) mean(x$gdp)
)

#ddply function feeds in a data.frame (function starts with d) and returns another data.frame (2nd letter is a d) i
#the first argument we gave was the data.frame we wanted to operate on: in this case the gapminder data.
#The second argument indicated our split criteria: in this case the “continent” column.
#Note that we gave the name of the column, not the values of the column like we had done previously with subsetting. Plyr takes care of these implementation details for you.
#The third argument is the function we want to apply to each grouping of the data. 

#Task: Use the ddplyr function, calculate the average life expectancy per continent. Which has the longest? Which had the shortest?

#What if we want a different type of data structure? 
dlply(
 .data = gap,
 .variables = "continent",
 .fun = function(x) mean(x$gdp)
)
#note the dl instead of dd, which means input is dataframe and output is list
#plyr rearranges datatsets into an input and output by a function. 
#plyr function can be applied in the following format xxplyr()
#the first letter in xxply denotes the type of data input
#the second letter in xxply denotes the type of data output
#e.g. daply means that the input is a dataframe and the output is an array
#d is dataframe
#l is list
#a is array
#r is n replicates
# m is a function argument
# _ is nothing. Note that noting cannot be in the input

#specifying multiple columns in a dataframe for an output
ddply(
 .data = gap,
 .variables = c("continent", "year"),
 .fun = function(x) mean(x$gdp)
)

#dd means that input and output is dataframe
daply(
 .data = gap,
 .variables = c("continent", "year"),
 .fun = function(x) mean(x$gdp)
)

#d means input is dataframe and a means output is an array
#these functions can be used in place of for loops (and it is usually faster to do so).

#Task: Calculate the average life expectancy per continent and year. Which had the longest and shortest in 2000? Which had the greatest change in between 1967 and 2000?


