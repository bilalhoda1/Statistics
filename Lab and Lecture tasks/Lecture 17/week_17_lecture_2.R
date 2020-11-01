#Week 17 Lecture 2
#General announcements
#Please note that the deadline for submitting your draft has been extended from June 20th 12:00am to June 21st 12:00am (extenstion by one more day). You should have finished your regression analysis by now and should have started working on the ordination analysis. 

#Let's start with a review

#What is a requirement that needs to be met when performing a regression analysis (hint: X variables)

#What are two general kinds of multivariate techniques?

#What is the goal of principal component analysis?

#What is the relationship between the number of principal components and the number of variables in a dataset?

#How much variation in a dataset does PC1 always explain?

#What is the relationship between PC1 and PC2?

#How can we determine which X variable is associated with which principal component?

#What is the goal of standardizing your data prior to performing principal component analysis?

#When is it appropriate to use a covariance matrix and when is it appropriate to use a correlation matrix in a PCA?

#Let's start with the lecture for today!
#Based on Ch 5 in Numerical Ecology in R by Borcard, Gillet, and Legendre

#Continuing with Ordination methods
#Most ordination methods (except NMDS) are based on the extraction of the eigenvectors of an association matrix. 
#They can be classified according to the distance preserved among sites and to the type of variables that they can handle.

#Comparing PCA with other ordination methods
#Principal component analysis (PCA): the main eigenvector-based method.
#Works on raw, quantitative data. Preserves the Euclidean distance among sites.

#Correspondence analysis (CA): works on data that must be frequencies or frequency-like, dimensionally homogeneous, and non-negative. #Preserves the Chi-sq distance among rows or columns.

#Principal coordinate analysis (PCoA): devoted to the ordination of distance matrices, instead of dataset containing matrices. 
#Hence, great flexibility in the choice of association measures.

#Nonmetric multidimensional scaling (NMDS): unlike the three others, this is not an eigenvector-based method. 
#NMDS tries to represent the set of objects along a predetermined number of axes while preserving the ordering relationships among them.

#Today we will review a more complex procedure for conducting a PCA and then introduce correspondence analysis

# @@ RGEDIT LANDMARK @@: Complete procedure for PCA
#setting working directory
setwd("/home/sarah/Documents/Habib/Math107/Lectures/Lecture 17")

#loading packages
library(ade4)
library(vegan)
library(gclus)
library(ape)

#loading additional functions
# (files must be in the working directory)
source("evplot.R")
source("cleanplot.pca.R")
source("PCA.R")
source("CA.R")

# Import the data from CSV files
#loading species, environment, and spatial data from the Doubs river watershed in France
# (files must be in the working directory)
spe <- read.csv("DoubsSpe.csv", row.names=1) 
env <- read.csv("DoubsEnv.csv", row.names=1)
spa <- read.csv("DoubsSpa.csv", row.names=1)

#checking the dataset out!
str(spe)
str(env)
str(spa)

# Remove empty site 8
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]

#Let's start by running a PCA on the env dataset
#We have 11 quantitative environmental variables at our disposal. 
#How are they correlated? What can we learn from the ordination of the sites?
summary(env) #taking a look at descriptive statistics

# PCA based on a correlation matrix 
#Does anyone know why we are using a correlation matrix?

env.pca <- rda(env, scale=TRUE) # Argument scale=TRUE calls for a standardization of the variables, i.e. correlation matrix
env.pca
summary(env.pca) # Default scaling 2

#The principal component analysis output using the vegan package requires some explanation

####Inertia: in vegan’s language, this is the general term for “variation” in the data
#In PCA, the “inertia” is either the sum of the variances of the variables (PCA on a covariance matrix) or
#in this case (PCA on a correlation matrix), the sum of the diagonal values of the correlation matrix, 
#i.e. the sum of all correlations of the variables with themselves, corresponds to number of variables

###Eigenvalues: symbolized lj, these are measures of the importance (variance) of the axes. 
#They can be expressed as Proportions Explained, or proportions of variation accounted for, by dividing them by the total inertia.

###Scaling: not to be confused with the argument SCALE calling for standardization of variables. 
#“Scaling” refers to the way ordination results are projected in the reduced space for graphical display.
#There is no single way to optimally display objects and variables together in a PCA biplot, i.e. a plot showing two types of results, here the sites and the variables.
#Two main types of scaling are used:

#Scaling 1 = distance biplot: the eigenvectors are scaled to unit length.
#Distances among objects in the biplot are approximations of their Euclidean distances in multidimensional space. 
#This means that distances between datapoint on the plot are accurate but angles between variables in the biplot do not reflect their correlation

#Scaling 2 = Scaling 2= correlation biplot: each eigenvector is scaled to the square root of its eigenvalue.
#Distances between datapoints in the biplot are not approximations of their Euclidean distances in multidimensional space. 
#The angles between descriptors in the biplot reflect their correlations

####Species scores: coordinates of the arrow heads of the variables. 
#For historical reasons, response variables are always called “species” in vegan, no matter what they represent.

####Site scores: coordinates of the sites in the ordination diagram. 
#Objects are always called “Sites” in vegan output file

 
summary(env.pca, scaling=1)


# @@ RGEDIT LANDMARK @@: Examining Two PCA biplots: scaling 1 and scaling 2


# Plots using vegan's biplot.rda
x11(title="PCA biplots - environment - biplot.rda", 12, 6) 
#if you are using Windows, x11 should be changed to windows. If you are using Mac, use "quartz"
par(mfrow=c(1,2))
biplot(env.pca, scaling=1, main="PCA - scaling 1")
biplot(env.pca, main="PCA - scaling 2")	# Default scaling = 2

#In scaling = 1, distances in multivariate space between sites are accurate
#In scaling = 2, the angle and the length of the arrows which represent the variables in the dataset are accurately depicted

#The scaling 2 biplot shows that the variables are organized in groups. Can we identify what these are?

# Plots using cleanplot.pca()
# A rectangular graphic window is needed for the two plots
windows(title="PCA biplots - environment - cleanplot.pca", 12, 6)
cleanplot.pca(env.pca)							# with site labels only (vegan's standard)
cleanplot.pca(env.pca, point=TRUE)	# with points for sites and arrowheads
cleanplot.pca(env.pca, ahead=0)			# ... and without arrowheads

# In scaling =1, the radius of this circle represents the length of the vector representing a variable that would contribute equally to all the dimensions of the PCA space.
#for any given pair of axes, the variables that have vectors longer than this radius make a higher contribution than average

# Examine and plot partial results from PCA output
?cca.object # Explains how an ordination object produced by vegan is structured and how to extract its results.

#####Important point
#PCA is not a statistical test, but a heuristic procedure.  
#It aims at representing the major features of the data along a reduced number of axes 
#hence, the expression “ordination in reduced space”

#How do we decide which components explain a significant proportion of the variation in our data?

#In our lecture, we examined the eigenvalues (i.e. proportion of variance explained)
#and decided how many axes were worth representing and displaying on the basis of the amount of variance explained

#Rather than choosing arbritarily, we can use a few procedure to standardize selection of components
#One option is Kaiser–Guttman criterion) 
#It consists of computing the mean of all eigenvalues and interpreting only the axes whose eigenvalues are larger than that mean.

#To do this, we will need to pull out the eigenvalues
(ev <- env.pca$CA$eig)

# Apply Kaiser-Guttman criterion to select axes
ev[ev > mean(ev)] #pulling out eigenvalues that are larger than the mean eigenvalue

#Another option is to compute the broken stick model

#This model which randomly divides a stick of unit length into the same number of pieces as there are PCA axes.
#The pieces are then put in order of decreasing length and compared to the eigenvalues.
#Only components with eigenvalues that are larger than the length of the corresponding piece of the stick are selected

#Building broken stick model
# Broken stick model
n <- length(ev) #findiing out the number of variables
bsm <- data.frame(j=seq(1:n), p=0) #creating an empty dataset
bsm$p[1] <- 1/n 
for (i in 2:n)
{
	bsm$p[i] = bsm$p[i-1] + (1/(n + 1 - i))
}
bsm$p <- 100*bsm$p/n
bsm

# Plot the output from Kaiser-Guttman and Broken stick model
windows(title="PCA eigenvalues")
par(mfrow=c(2,1))
barplot(ev, main="Eigenvalues", col="bisque", las=2)
abline(h=mean(ev), col="red")		# average eigenvalue
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
barplot(t(cbind(100*ev/sum(ev),bsm$p[n:1])), beside=TRUE, 
	main="% variance", col=c("bisque",2), las=2)
legend("topright", c("% eigenvalue", "Broken stick model"), 
	pch=15, col=c("bisque",2), bty="n")

# Same plots using a single function: evplot()
# Plot eigenvalues and % of variance for each axis
evplot(ev)


# @@ RGEDIT LANDMARK @@: Combining clustering and ordination results

#We can compare a cluster analysis and an ordination to explain or confirmthe differences between groups of sites.
#Here, I have used two ways of combining these results
#The first differentiates clusters of sites by colours on the ordination plot
#the second overlays a dendrogram on the plot

# Clustering the objects using the environmental data: Euclidean 
# distance after standardizing the variables, followed by Ward clustering
env.w <- hclust(dist(scale(env)), "ward")
# Cut the dendrogram to yield 4 groups
gr <- cutree(env.w, k=4)
grl <- levels(factor(gr))

# Get the site scores, scaling 1
sit.sc1 <- scores(env.pca, display="wa", scaling=1)

# Plot the sites with cluster symbols and colours (scaling 1)
x11(title="Ordination and clustering")
p <- plot(env.pca, display="wa", scaling=1, type="n", 
	main="PCA correlation + clusters")
abline(v=0, lty="dotted")
abline(h=0, lty="dotted")
for (i in 1:length(grl))
{
	points(sit.sc1[gr==i,], pch=(14+i), cex=2, col=i+1)
}
text(sit.sc1, row.names(env), cex=0.7, pos=3)
# Add the dendrogram
ordicluster(p, env.w, col="dark grey")
# Add legend interactively
legend(locator(1), paste("Cluster", c(1:length(grl))), pch=14+c(1:length(grl)), 
	col=1+c(1:length(grl)), pt.cex=2)


# @@ RGEDIT LANDMARK @@: Introduction to Correspondence Analysis
#Correspondence analysis (CA) has been one of the most used ordination tools for the analysis of count, presence-absence, or abundance data
#The raw data are first transformed into a matrix based on contribution to the Chi-q statistic
#Then the matrix is decomposed to a singular value to compute eigenvalues and eigenvectors
#This results in ordination where Chi-sq distance is preserved between sites rather than Euclidean distance

#Note that the Chi-sq distance is not influenced by zeros. 
#Therefore we can perform CA without transforming our data (e.g. through standardization)

#For technical reason not mentioned here, CA produces one less axis (component) than the number of variables

#Note that CA should be used when we do not have or expect a linear relationship between variables in a dataset
#CA assumes that the relationship between variables in a dataset is unimodal

#How do we determine if we have a linear relationship between variables or not?
#we can use the decorana function (detrended correlational analysis)
#If value of the longest gradient is close or larger than 4, then we use unimodal methods (CA, CCA)
#If not, then we use linear methods (PCA, RDA)

#let's examine the species dataset (spe)
decorana(spe)

#Longest gradient is close to 4 
#We will use CA analysis (although we could get away with using PCA if data was standardized)

# Compute CA
spe.ca <- cca(spe)
spe.ca
summary(spe.ca)		# default scaling 2
summary(spe.ca, scaling=1)

#What proportion of the total inertia (variation) does the first axis account for?
#What about the second and third axes?


#Making the Kaiser–Guttman and broken stick plots
(ev2 <- spe.ca$CA$eig)
x11(title="CA eigenvalues")
evplot(ev2)

#Are there differences between the number of axes considered important by the broken stick plots and Kaiser-Guttman procedure?


#Examining biplots
# CA biplots
x11(title="CA biplots", 14, 7)
par(mfrow=c(1,2))
# Scaling 1: sites are centroids of species
plot(spe.ca, scaling=1, main="CA fish abundances - biplot scaling 1")
# Scaling 2 (default): species are centroids of sites
plot(spe.ca, main="CA fish abundances - biplot scaling 2")

#Note that these plots do not include arrows!
#Species names are provided in red! 

