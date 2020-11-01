#Lecture 17

#Announcements

#By this time, you should have already started, if not completed your regression analysis. 
#If you have not done this yet, I suggest that you do so as soon as possible since your first draft is going to be due on 21st June (12:00am)
#I would also suggest that one group member start outlining and writing the introduction, while another group member works on the methods section to describe the analysis which has already been completed or is underway
#If you have completed your regression analysis, then you should start writing your regression results, while a group member completed the multivariate analysis
#Please let me know if you would like to have my input for any aspect of your project. I won't give you the answer, but I will point you in the right direction!

#Review 

#Which two plots do we use to determine if the requirement of homoscedasticity is met?


#How do we determine outliers for generalized linear models?


#What do we use to quantify how well a generalized linear model fits data? 


#What do phi values of less than 1 mean? What do we call this?



#What do phi values of more that 1 mean? What do we call this?


#What is the difference between a quasi poisson GLM and a poisson GLM?


#What can we do when a poisson distribution is very over-dispersed?


#What kind of data is described by the negative binomial distribution? 


#What features of the negative binomial distribution allow it to model over dispersed count or abundance data?


#what tests do we use to determine which explanatory variables,X, significantly explain the difference in the response variable, Y when using a negative binomial model?



#Let's start today's lecture
#Today, we are going to focus on how to start assessing/analyzing datasets with a large number of variables
#We are moving away from regression analysis
#However, before we do this, I would like to remind everyone one important cardinal rule when doing regression
#Does anyone know what this rule is? (Hint: It has to do with the explanatory variables, i.e. X)

The explanatory variable have to be independent of each other (i.e. they cannot be correlated)
#This means that before fitting a linear regression model or a generalized linear model, you HAVE to check for correlation between your X variables

#So your steps in regression analysis should be 
#1) Histogram to check distribution of Y variable. If Y variable does not look normally distributed (or cannot be normally distributed depending on the type of data it is), then you need to figure out which probability distribution to use
#2) Correlation analysis between X variables (if all are continuous). If X variables have low correlation (<0.3), then apply regression analysis
#2) Create linear regression or generalized linear model with Y and X
#3) Check assumptions using plot command
#4) If more than one X variable, then use model selection procedures to determine form of the final model (only include X variables that significantly explain variation in Y)
#5) Check assumption of final model using plot command
#6) Check summary to get R2 value for linear regression and see coefficients of variation
#7) Obtain p-value from summary (if using dredge and AIC values for model selection) or directly from log-likelihood ratio tests if using backwards or forwards selection for GLMs
#8) For your final model, report degrees of freedom, R2 (if applicable), p-values for X variables, as well as general trend between Y and X for each significant X variable (increasing/decreasing, large increase or decree etc). For X variables that are categorical, present appropriate measure of center and spread for each category and present differences in measures of center between categories. If more than two categories in a categorical variable and categorical variable is significant, you have to do Tukeys HSD to determine which differences between variables are significantly different.

#If you would like to know more about how to deal determine differences between more than two categories when data is non-normally distributed, talk to me!

#What happens if our X variables are correlated? 

#Then we cannot use regression analysis. Instead we need to use multivariate techniques such as principal component analysis or correspondence analysis


#Let's start with principal component analysis

#Principal component analysis (PCA) is one of the most widely used multivariate statistical techniques. It is a linear dimensionality reduction algorithm (assumes all variables are related linearly with each other). 
#The goal of the PCA is to reduce and summarize a large number of variables (correlated or not) into a smaller number of components which contain most of the information contained in the dataset

#Reducing the number of variables of a data naturally affects accuracy, but dimensionality reduction technique such as PCAs trade a little  accuracy for simplicity.

#Example: You want to compare water chemistry (e.g. pH, nitrates, phosphates, dissolved oxygen, dissolved organic carbon) across multiple watersheds. 

#What a principal component analysis does is create an p-dimensional ellipsoid based on your dataset (with number of dimensions dependent on number of variables), and then reduces it to components that explain the most variation in each dimension

#It does this by: 

#1) First subtract the mean of each variable from the dataset to center the data around the origin.
    
#2) Compute the covariance matrix of the data  and calculate the eigenvalues and corresponding eigenvectors of this covariance matrix

#Intro to eigenvalues and eigenvectors for those interested: https://www.mathsisfun.com/algebra/eigenvalue.html


#3)Normalize each of the orthogonal eigenvectors to become unit vectors 

#4)Once this is done, each of the mutually orthogonal, unit eigenvectors can be interpreted as an axis of the ellipsoid fitted to the data


#An easier way to think about this is:
# In this analysis, we will start by fitting a line to the data which minimizes the most (euclidean) distance from data points to the line in multidimensional space. 

#For euclidean distance: https://www.cut-the-knot.org/pythagoras/DistanceFormula.shtml


#Think of this as a line of best fit in multivariate space, as close as possible to all the points with the variation maximized along the line and minimized on the perpendicular away from the line. 

#The first principal component (PC1) corresponds to a line that passes through the multidimensional mean and minimizes the sum of squares of the distances of the points from the line. PC1 captures the maximum degree of variance in the dataset

#The second principal component (PC2) would be fit based on two criteria: 

#1) Maximizing second most degree of variance (minimizes euclidean distance between points and line)

#2) Orthogonal to the first principal component analysis (No correlation with PC1)

#and so on...

#Link to ellipsoid: https://www.monroecc.edu/faculty/paulseeburger/calcnsf/CalcPlot3D/#ParametricSurfaces
#Under examples, click parametric surfaces and then click ellipsoid!

#One more example!

#Suppose we are botanists interested in variation in leaf shape across different species (slide 1)

#We measure two variables: Leaf Length and Width 1 (slide 2)

#The first principal component fit would be along the diagonal as it captures the most variation (minimizes the euclidean distance between most points) (slide 3)

#The second principal component (PC2) fit would be across from the diagonal, as it captures the second most variation in the data point AND is orthogonal to PC1

#When we plot the eigenvalues (obtained when calculating euclidean distance between two points) for each row of data (row could be sample number, site number, specimen number, etc...), we can determine if difference rows (which represent sites, specimens, groups, etc....) group together when all variables assessed are considered together. 

#Here you can see that when all leaf traits are considered together by plotting eigenvalues for all species for PC1 and PC2 (PC1: Explains most variation in the dataset, PC2: Explains second most variation in the dataset), species 1 and species 2 group separately from each other. This suggests that species 1 and species 2 are very different from each other in terms of leaf traits. 

#Let's learn how to do a principal component analysis in R

#We are going to use the leaf shape dataset that was used in the example above. Source: http://environmentalcomputing.net/principal-components-analysis/

#setting working directory
setwd("/home/sarah/Documents/Habib/Math107/Lectures")
leaf<-read.csv("Leafshape.csv")

#let's look at the data
str(leaf)
#We have 6 variables, with species as a categorical variables
#since we only run PCA using continuous data***, we should separate species from our dataset
Species<-leaf$Species
leaf_new_data<-leaf[,c(2:6)] #only including columns that contain continuous data

#now we need to examine the center and spread of each variable
#why?
#because if some variables have lots of variation and others have very little, then PCA is going to be biased towards variables that have more variance. 

#Comparing variance on a single plot
par(mfrow = c(3,2))
hist(leaf$Total_length)
hist(leaf$Petiole_length)
hist(leaf$Leaf_length)
hist(leaf$Width1)
hist(leaf$Width2)

#all variables seems to have similar levels of spread, except Width1

#we should standardize our data

#we can both run the PCA and standardize our data using the princomp command built into R base

#let's do this

leaf_pr <- princomp(leaf_new_data,  center = TRUE, cor = FALSE) 
#center = TRUE means we are centering data, by subtracting each data point from the mean of that variable. We are standardizing because variances are a little different!
#cor = FALSE means that the principal component analysis will use a covariance matrix to determine orthogonality between different components. 
#Using a covariance matrix is fine if all your variables are measured in the same units
#In our case, all measurements on leaf are made in mm, so we can use the covariance matrix
#But if we measured percent diseases (between 0-100%) as well as these leaf traits, then we would have to use a correlation matrix
#To do this, we would have to set cor = TRUE

summary(leaf_pr)
#First principal component explains 90% of variance in all variables (see proportion of variance in table)
#Second principal component explains 6% of variance in all variables (see proportion of variance in table)

#How much variance does PC3 explain?

#What about PC4 and PC5?

plot(leaf_pr) #visualizing the amount of variance explained by each component
#Keep in mind that the number of principal components produced in a principal component analysis is equal to number of variables in dataset


#another way to do this is by using a screeplot
screeplot(leaf_pr, type = "lines") 

#Note that the Y-axis presented is not the % of variation explained. 
# The variance explained always declines with the number of the component
#We want as much variation as possible explained by the first two principal components, as these are the ones we are using to visualize the data.

#visualizing data with the first two principal components (PC1 and PC2)
plot(leaf.pr$scores, xlab = "PC1", ylab = "PC2", col = leaf$Species, pch = 5, cex = 2) #coloruing by species
#adding legend
legend(0,0.4,c("Species A","Species B"),pch=5,col=c("black","red"))

#Interpreting the results
#You can see how well each variable corresponds with a specific principal component axis
loadings(leaf_pr)

#loadings show that Total_length corresponds most with PC2 (Comp.2) as the value of the loading is the greatest on this component. 
#please note that loadings for Total_length are also large for PC5, but PC5 does not really explain a lot of the variance
#Therefore, this association is not really important

#What about Petiole_Length?

#What about Width1?

#We can visualize the association between variables and different principal component axes using a biplot
biplot(leaf_pr, xlim = c(-0.5, 0.5))
#this should match the associations provided in loadings

#Assumptions of PCA
#1) Linearity
#Associations between variables MUST be linear. If not, you will need to use correspondence analysis.
#This can be done by plotting variables with each other
#This can also be checked using decorana function in vegan package (next lecture)

#2) Correlation versus covariance matrices
#As I mentioned earlier, you can run PCA using a covariance matrix, which is appropriate when all variables are measured on the same scale, or a correlation matrix, which is appropriate if variables are measured on very different scales.
#Using different matrices will produce different outputs!
#Covariance matrix is affected by the differences in size of variances between variables
#If we want variances for variables measured in different scales to have the same influence in our analysis, we need to standardize our data (e.g. center it or scale it to unit variance (i.e divide every value in a variable by the standard deviation)). 

#3) Outliers
#Outliers will have a large influence on the output of a PCA! This effect will be more prominent if a covariance matrix is used!


#Other things to consider:
#Rotating axes: The rotated PCA (RPCA) methods rotate the PCA eigenvectors, so they point closer to the local clusters of data points.
#princomp function introduced in this lecture produces unrotated axes. Rotated PCA axes are beyond the scope of this course, but we can have a conversation about this outside of class for those who are interested!



















