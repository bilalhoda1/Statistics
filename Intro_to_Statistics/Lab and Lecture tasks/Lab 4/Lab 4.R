#In today's tutorial, we will learn how to make interactive documents using R markdown (adapted from software carpentry)
#Rmarkdown helps us write documents containing snippets of code directly in R
#Within RStudio, click File → New File → R Markdown and you’ll get a dialog box 
#You can stick with the default (HTML output), but give it a title.
#The initial chunk of text (header) contains instructions for R to specify what kind of document will be created, and the options chosen. 
#You can use the header to give your document a title, author, date, and tell it that you’re going to want to produce html output (in other words, a web page).

---
title: "Initial R Markdown document"
author: "Sarah Hasnain"
date: "Feb 4th, 2015"
output: html_document
---

#You can delete any of those fields if you don’t want them included. 
#The double-quotes aren’t strictly necessary in this case. They’re mostly needed if you want to include a colon in the title.
#RStudio creates the document with some example text to get you started. Note below that there are chunks like this:

```{r}
summary(cars)
```
#These are chunks of R code that will be executed by knitr and replaced by their results.
#Also note the web address that’s put between angle brackets (< >) 
#as well as the double-asterisks in **Knit**. This is Markdown.

#What is markdown?
#Markdown is a system for writing web pages by marking up the text much 
#as you would in an email rather than writing html code. 
#The marked-up text gets converted to html, replacing the marks with the proper html code.

#For now, let’s delete all of the stuff that’s there and write a bit of markdown.

#You make things bold using two asterisks, like this: **bold**
#you make things italics by using underscores, like this: _italics_.

#You can make a bulleted list by writing a list with hyphens or asterisks, like this:

* bold with double-asterisks
* italics with underscores
* code-type font with backticks

#or like this:

- bold with double-asterisks
- italics with underscores
- code-type font with backticks

#You can use whatever method you want, but be consistent!
#You can make a numbered list by just using numbers. You can even use the same number over and over if you want:

1. bold with double-asterisks
1. italics with underscores
1. code-type font with backticks

#You can make section headers of different sizes by initiating a line with some number of # symbols:

# Title
## Main section
### Sub-section
#### Sub-sub section

#You compile the R Markdown document to an html webpage by clicking the “Knit” button in the upper-left.

#Task 1 Create a new R Markdown document for the gapminder dataset. Delete all of the R code chunks and write a bit of Markdown (some sections, some italicized text, and an itemized list).

#Convert the document to a webpage.

#You can make a hyperlink like this: 
[text to show](http://the-web-page.com).

#You can include an image file like this: 
![caption](http://url/for/file)

#You can do subscripts (e.g., F~2~) with F~2~ and superscripts (e.g., F^2^) with F^2^.

#If you know how to write equations in LaTeX, you can use $ $ and $$ $$ to insert math equations, like $E = mc^2$ and

#$$y = \mu + \sum_{i=1}^p \beta_i x_i + \epsilon$$

#You can review Markdown syntax by navigating to the “Markdown Quick Reference” 
#under the “Help” field in the toolbar at the top of RStudio.

#The main code chunks look like this:

#```{r load_data}
#gapminder <- read.csv("gapminder.csv")
#```

#Task 2: In the markdown document created in Task 1 Add code chunks to:

#Load the ggplot2 package
#Read the gapminder data
#Create a plot

#More options in R markdown
#Use echo=FALSE to avoid having the code itself shown.
#Use results="hide" to avoid having any results printed.
#Use eval=FALSE to have the code shown but not evaluated.
#Use warning=FALSE and message=FALSE to hide any warnings or messages produced.
#Use fig.height and fig.width to control the size of the figures produced (in inches).

#So you might write:

```{r load_libraries, echo=FALSE, message=FALSE}
library("dplyr")
library("ggplot2")
```

#Often there will be particular options that you’ll want to use repeatedly; 
#for this, you can set global chunk options, like so:

```{r global_options, echo=FALSE}
knitr::opts_chunk$set(fig.path="Figs/", message=FALSE, warning=FALSE,
                      echo=FALSE, results="hide", fig.width=11)
```






