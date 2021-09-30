#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

#load any necessary packages
lapply(c("tidyverse"),  pkgTest)

# set working directory
setwd("~/StatsI_Fall2021/problemSets/PS01/template")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#Question 1.1 Find a 90% confidence interval for the average student IQ in the school.

#Our confidence coefficient is = .90 
#because n is lower than 30 we need to use a t distribution. 

#Find the sample mean
sample_mean <- mean(y , na.rm = TRUE)

#Find the sample standard deviation
sample_sd <- sd(y, na.rm = TRUE)

#Find the T statistic
t90 <- qt((1-.90)/2,df=24)

#Find the Correlation Coefficiant by plus/minusing the error from the sample mean. 
n <- length(na.omit(y))

lower_90 <- sample_mean - (t90 * (sample_sd/sqrt(n)))
upper_90 <- sample_mean + (t90 * (sample_sd/sqrt(n)))

confint90 <- c(lower_90, upper_90)
print(confint90)

#Question 1.2: the school counselor was curious whether the average student IQ in her school
#is higher than the average IQ score (100) among all the schools in the country.
#Using the same sample, conduct the appropriate hypothesis test with α = 0.05.


#Step one: assumptions 
#As our sample size is 25 we will be using a t distribution, but will assume normality
#we are assuming the sampling methods was randomized 
#the type of data is Quantitative 

#we then set up the null and alternative hypotheses. 

#null hypothesis suggests that there is no difference between the sample
#and the population mean (100)

#H0 : sample mean = 100
#H1 : sample mean is above 100 

#we are going to conduct a one sided test because we are interested about whether the 
#average is higher than the sample 

#In R we can use the t.test function to get the t statistic, the degrees of freedom,
#and the p-value. 

t.test(y, mu = 100,
       alternative = "greater")

#Next step is to calculate the P-Value. The above code prints out the p-value. 
#Next step is to draw a conclusion. as the p value is .7215, we fail to reject the null. 


#####################
# Problem 2
#####################
#first clear the environment 
rm(list=ls())

#Then import the data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)

#Question 2.1 

#Relationships amoung the varibles 
#Y and X1 
plot(expenditure$Y, expenditure$X1)
cor.test(expenditure$Y, expenditure$X1)

#Y and X2
plot(expenditure$Y, expenditure$X2)
cor.test(expenditure$Y, expenditure$X2)

#Y and X3 
plot(expenditure$Y, expenditure$X3)
cor.test(expenditure$Y, expenditure$X3)

#X1 and X2 
plot(expenditure$X1, expenditure$X2)
cor.test(expenditure$X1, expenditure$X2)

#X1 and X3 
plot(expenditure$X1, expenditure$X3)
cor.test(expenditure$X1, expenditure$X3)

#X2 and X3 
plot(expenditure$X2, expenditure$X3)
cor.test(expenditure$X2, expenditure$X3)

#Question 2.2 
#Plot the relationship between Y and Region 

#first load the library 
library(ggplot2)

#plot the relationship
ggplot(expenditure, aes(x=Region, y=Y)) + 
  geom_point(size=2, shape=23)

#Question 2.3 
#Plot the relationship between Y and X1. 
ggplot(expenditure, aes(x=X1, y=Y)) + 
  geom_point(size=2, shape=23)

#Adding the line of best fit to help visually see the data. 
ggplot(expenditure, aes(x=X1, y=Y)) + 
  geom_point(size=2, shape=23)+ 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Question 2.3 

#Reproduce this graph with one more variable Region and display different types of 
#Regions with different colors and symbols 

ggplot(expenditure, aes(x=X1, y=Y, color=as.factor(Region), shape=as.factor(Region))) +
  geom_point() + 
  scale_shape_manual(values=c(1, 2, 3, 4))+ 
  scale_color_manual(values=c('red','orange', 'seagreen1', 'royalblue4'))+
  theme(legend.position="bottom")

#Below I added the line of best fit to help visualisatin. 

ggplot(expenditure, aes(x=X1, y=Y, color=as.factor(Region), shape=as.factor(Region))) +
  geom_point() + 
  scale_shape_manual(values=c(1, 2, 3, 4))+ 
  scale_color_manual(values=c('red','orange', 'seagreen1', 'royalblue4'))+
  theme(legend.position="bottom")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)



