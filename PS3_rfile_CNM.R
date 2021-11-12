##################
#Problem Set 3 CNM
##################


#First load the libraries 
options(scipen = 999) # to change the default output of numbers
library(tidyverse)
library(broom)

#Load in the Data 
incumbents <- read_csv("https://raw.githubusercontent.com/caoimheni/StatsI_Fall2021/main/datasets/incumbents_subset.csv")

#Exploratory Data Analysis 
summary(incumbents)
str(incumbents)
head(incumbents)


###########
#Question 1
###########

#We are interested in knowing how the difference in campaign spending between incumbent
#and challenger affects the incumbent's vote share

#Run a regression where the outcome variable is voteshare
#and the explanatory variable is difflog

#Regression code 
summary(lm(data = incumbents, voteshare ~ difflog))

#Make a scatterplot of the two variables and add the regression line. 
ggplot(data = incumbents, aes(x = difflog, y = voteshare)) +
  geom_point(alpha = 0.2) + #add a scatterplot
  geom_smooth(method = lm) #add a linear regression line

#Save the residuals of the model in a separate object
q1_residuals <- residuals(lm(data = incumbents, voteshare ~ difflog))

#Write the prediction equation.
#y = a + bx 
lm(data = incumbents, voteshare ~ difflog)
#0.579031 = a + 0.041666x



###########
#Question 2
###########


#We are interested in knowing how the difference between incumbent and challenger's 
#spending and the vote share of the presidential candidate of the incumbent's party are related

#Run a regression where the outcome variable is presvote (Y)
#and the explanatory variable is difflog (X)

#Regression code 
summary(lm(data = incumbents, presvote ~ difflog))

#Make a scatterplot of the two variables and add the regression line. 
ggplot(data = incumbents, aes(x = difflog, y = presvote)) +
  geom_point(alpha = 0.2) + #add a scatterplot
  geom_smooth(method = lm) #add a linear regression line

#Save the residuals of the model in a separate object
q2_residuals <- residuals(lm(data = incumbents, presvote ~ difflog))

#Write the prediction equation.
#y = a + bx 
summary(lm(data = incumbents, voteshare ~ difflog))

###########
#Question 3
###########
#We are interested in knowing how the vote share of the presidential candidate of the
#incumbent's party is associated with the incumbent's electoral success.

#Run a regression where the outcome variable is voteshare 
#and the explanatory variable is presvote.

#Regression code 
summary(lm(data = incumbents, voteshare ~ presvote))

#Make a scatterplot of the two variables and add the regression line. 
ggplot(data = incumbents, aes(x = presvote, y = voteshare)) +
  geom_point(alpha = 0.2) + #add a scatterplot
  geom_smooth(method = lm) #add a linear regression line


#Write the prediction equation.
#y = a + bx 
lm(data = incumbents, voteshare ~ presvote)


###########
#Question 4 
###########
#Residuals 

#Run a regression where the outcome variable is the residuals from Question 1 
#and the explanatory variable is the residuals from Question 2.

#Regression code 
summary(lm(data = incumbents, q1_residuals ~ q2_residuals))

#Make a scatterplot of the two variables and add the regression line. 
ggplot(data = incumbents, aes(x = q2_residuals, y = q1_residuals)) +
  geom_point(alpha = 0.2) + #add a scatterplot
  geom_smooth(method = lm) #add a linear regression line

#Write the prediction equation.
lm(data = incumbents, q1_residuals ~ q2_residuals)
#y = a + bx 


###########
#Question 5 
###########

#What if the incumbent's vote share is affected by both the president's popularity 
#and the difference in spending between incumbent and challenger? 

#Run a regression where the outcome variable is the incumbent's voteshare 
#and the explanatory variables are difflog and presvote
#Regression code 
summary(lm(data = incumbents, voteshare ~ difflog + presvote))

#prediction equation
lm(data = incumbents, voteshare ~ difflog + presvote)

