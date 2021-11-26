###################################
#Problem Set 4 Caoimhe Ni Mhaonaigh 
###################################


#First load the libraries 
options(scipen = 999) # to change the default output of numbers
library(tidyverse)
library(broom)


######################
#Question 1: Economics  
######################


#Load in the Data 
install.packages("car")
library("car")
data(Prestige)
help(Prestige)


#We would like to study whether individuals with higher levels of income 
#have more prestigious jobs. Moreover, we would like to study whether professionals 
#have more prestigious jobs than blue and white collar workers

#######
#Part A 
#######

#Create a new variable \texttt{professional} by recoding the variable 
#type so that professionals are coded as 1, and blue and white 
#collar workers are coded as 0. (Hint: ifelse.)

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

#######
#Part B 
#######

#Run a linear model with prestige as an outcome and income, 
#professional, and the interaction of the two as predictors
#(Note: this is a continuous times dummy interaction.)

interaction_reg <- lm(data = Prestige, prestige ~ income + professional + income:professional)
summary(interaction_reg)


#######
#Part C 
#######

#I can use the following code to get the information to write the prediction equation 

interaction_reg

#######
#Part F 
#######

#What is the effect of a $1,000 increase in income on prestige score for professional 
#occupations?  In other words,  we are interested in the marginal effect of income when 
#the variable professional takes the value of1.  Calculate the change inˆyassociated with 
#a $1,000 increase in income based on your answer for (c)


#I am looking a the prestige increase when income changes from 0 to 1000. This will be 
#the same as a 1000 dollar increase at any pay level as it is a linear relationship. 

prestige0 <-  21.142259 + 0.003171*0 + 37.781280*1 + (-0.002326*0*1)
prestige0

prestige1000 <-  21.142259 + 0.003171*1000 + 37.781280*1 + (-0.002326*1000*1)
prestige1000

differenceincome <- prestige1000 - prestige0 
differenceincome

#######
#Part G 
#######

#What is the effect of changing one’s occupations from non-professional to professional
#when her income is $6,000?  We are interested in the marginal effect of professional jobs
#when the variable income takes the value of 6,000.Calculate the change inˆybased on your 
#answer for (c).

#Here I have inserted 1 for professional and 0 for non professional and
#subtracted to find the marginal effect. 

nonprofes6000 = 21.142259 + 0.003171*6000 + 37.781280*0 + (-0.002326*6000*0)
nonprofes6000

profes6000 = 21.142259 + 0.003171*6000 + 37.781280*1 + (-0.002326*6000*1)
profes6000

differenceprofes <- profes6000 - nonprofes6000


##############################
#Question 2: Political Science  
##############################

#######
#Part A 
#######

#We input the t statistic into the the following code to output the answers: 

2*pt(2.625, df = 128, lower.tail =FALSE)


#######
#Part B 
#######

#We input the t statistic into the the following code to output the answers:
2*pt(3.23076923, df = 128, lower.tail =FALSE)
