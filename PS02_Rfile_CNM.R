###############
#Probelm Set 2 
##############

#First set the working directory 

getwd()
setwd("~/Desktop/StatsI_Fall2021")

#Next load the libraries to be used 

library(ggplot2)
library(tidyverse)

################################
#Question One: Political Science 
################################

mydata = read.table("excerciseonedata.csv", header=TRUE, sep = ',')

#Looking at the data 
class(mydata)
str(mydata)
head(mydata)
summary(mydata)

#To preform a chi square test on the above data 

#Manually putting the Chi statistic in
bribe_chisq <- pchisq(3.79, df = 2, lower.tail = FALSE)
bribe_chisq

#Input the data to be able to output the standardized residuals 
datatable <- matrix(c(14,6,7,7,7,1),nrow=3,ncol=2) 
datatable

chisq <- chisq.test(datatable,correct=FALSE)
chisq
chisq$stdres

#############
#Exercise Two 
#############

#Frist read in the data
economicsdata <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

#Then look at the data 

class(economicsdata)
str(economicsdata)
head(economicsdata)
summary(economicsdata)

#Run a bivariate regression to test this hypothesis in R. 

data("economicsdata")
lm <- lm(economicsdata$irrigation ~ economicsdata$reserved)
summary(lm)

############################
#Exercise Three fruitfly.csv
############################

#Load in the data 
library(faraway)
data("fruitfly")

#Examine the distribution of the overall lifespan of the fruitflies.
#See the summary of the data and the distribution 
summary(fruitfly)
#Create a Histogram of the lifespan 
p<-ggplot(fruitfly, aes(x=longevity)) + 
  geom_histogram(color="black", fill="lightgreen", binwidth = 7)+
  labs(title="Fruitfly Lifespan Distibution",x="Days", y = "Count")
p

#Plot lifespan vs thorax. Does it look like there is a linear relationship? Provide the plot. 
#What is the correlation coefficient between these two variables?

ggplot(aes(longevity, thorax), data = fruitfly) +
  geom_point(size=2, shape=16) +
  ggtitle("Scatterplot of Thorax and lifespan") +
  labs(y="Thorax Size in mm")+
  labs(x = "Number of Days")

cor.test(fruitfly$longevity, fruitfly$thorax)

#Regress the lifespan on thorax. Interpret the slope of the fitted line. 

lm(formula = fruitfly$longevity ~ fruitfly$thorax)

#Test for a significant linear relationship between lifespan and thorax. 
#Provide and interpret your results of your test.
lm <- lm(fruitfly$longevity~ fruitfly$thorax)
summary(lm)

#Provide the 90% confidence interval for the slope of the fitted model.
#To find the confidence interval by hand

slopeoftheline <- 145.28
criticalvalue <- 1.645
standarderror <- 16.19

marginoferror <- criticalvalue*standarderror

upperlimit = slopeoftheline + marginoferror
lowerlimit = slopeoftheline - marginoferror
upperlimit
lowerlimit 

#To produce using R 
confint(lm, level = 0.9)

#Use thepredict()function inRto (1) predict an individual fruitfly’s lifespanwhenthorax=0.8 
#and (2) the averagelifespanof fruitflies whenthorax=0.8by the fitted model.  
#This requires that you compute prediction and confi-dence intervals.  
#What are the expected values of lifespan?  
#What are theprediction and confidence intervals around the expected values?

fruitflylm <- lm(longevity ~ thorax, data = fruitfly)
predict(fruitflylm, newdata = new_df, interval = "predict")
predict(fruitflylm, newdata = new_df, interval = "confidence")

#For  a  sequence  ofthoraxvalues,  draw  a  plot  with  their  fitted  values  forlifespan, 
#as well as the prediction intervals and confidence intervals.
fruitflylm <- lm(longevity ~ thorax, data = fruitfly)
new_df <- data.frame(thorax = 0.8)
predict(fruitflylm, newdata = new_df)

predict(fruitflylm, newdata = new_df, interval = "predict")

predict(fruitflylm, newdata = new_df, interval = "confidence")

plot(longevity ~ thorax, data = fruitfly, pch = 19, col='darkgrey')


#For  a  sequence  ofthoraxvalues,  draw  a  plot  with  their  fitted  values  for lifespan,
#as well as the prediction intervals and confidence intervals.
#remomve the 0.8 data point 

fruitflylm <- lm(longevity ~ thorax, data = fruitfly)
predictions <- predict(fruitflylm, interval = "predict")
alldata <- cbind(fruitfly, predictions)

ggplot(aes(thorax, longevity), data = alldata) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + 
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") +  
  labs(y="Days") +
  labs(x ="Thorax size mm")
