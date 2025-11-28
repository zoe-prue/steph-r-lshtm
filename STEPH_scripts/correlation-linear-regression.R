# Correlation and simple linear regresion
# 18/11/2025
# Study correlation and simple lienar regression

###################################
# library calls

install.packages("rio")
library(rio)
library(dplyr)

###################################
# global variables

setwd("~/Desktop/steph-r-lshtm/raw_STEPH_data")
bab9<-import("bab9.csv")

###################################
# script

summary(bab9)

##  QUANTITATIVE EXPLANATORY VAIRABLES ##

# exploring with histogram
# assumptions are that y is normally distributed for values of x -> check this
hist(bab9$bweight)

# often we've been stratifying into categorical variables
# we lose information this way becasue the difference between values in the 49th and 50th percentile
# (for example) are not significantly different, probably

# plot simple scatter plot
plot(bab9$gestwks, bab9$bweight, xlab="gestation (weeks)", ylab="birthweight") 
# large number of points clustered in upper right corner of graph

# make the dots less hollow and smaller with cex argument
plot(bab9$gestwks, bab9$bweight, xlab="gestation (weeks)", ylab="birthweight", cex=0.5)

# draw a line of best fit through the graph
abline(lm(bab9$bweight ~ bab9$gestwks))

# Q1. Do you think that a straight line through the points adequately 
# captures the relationship between these two variables?
# The straight line is helpful but does not quantify the relationship between the two variables
# I am assuming that the straight line reduces the distance of the residuals, 
# basically making the difference between the dots and the line as minimal as possible for them all

## LINEAR REGRESSION ##

# we hypothesize that gestweeks is the exposure and bweight is the outcome 
# because of their temporal and biological relationship

# lienar regression equation
# y_i= α + βx_i + ε_i
# where y_i is the measured individuals values of y, bweight
# x_i is the individual measured values for the exposure variabled, gestweeks
# alpha is the expected constant when y=0
# alpha may not be interpretable, as values oputside of the measured x may not be able to interpret
# we could make a case that the zygote is equal to zero weight
# β is the change in mean expected birthweight per additional week of gestational age
# beta is a statistical aprameter assumed about the population
# episilon is the error, essentially, or residuals, which were not accounted by other variables

model_1<-lm(bweight ~ gestwks, data=bab9)
summary(model_1)
# beta is gestweeks coefficient = 206.641, meaning for every 1 week, 
# the fetus gains 206.641 grams of mass (plus the intercept)
# shows effect of gestational age on bweight, called regression coefficient

# confidence intervals for linear regression model
confint(model_1)
# gestwks: lower 95%: 191.9439, upper 95%: 221.3386 grams

# to see only coeffs:
coef(model_1)

# Q2. What are the estimated values of the two parameters?  
# Write down the regression equation, with these estimates in the equation in place of α and β.  
# How would you interpret the coefficient for gestational weeks?
# alpha, or intercept, is -4865.2452 grams, and the beta regression coeff is 206.6412 grams
# y_i = -4865.2452 + (206.6412)(x_i) + -6.97
# the coeff for gestweeks is uninterpretable, as there is no way a fetus can be a negative mass

# Q3. What are the standard errors of the two parameter estimates? 
# How strong is the evidence against the null hypothesis of no linear relationship 
# between birthweight and gestational age?
# the standard error (standard deviations of the sample from the true population parameter)
# for alpha is 290.081 grams
# for beta is 7.485 grams
# ALSO, THE NULL HYPOTHESIS DOESN'T INCLUDE ZERO -> more certainty that the true value of ebta is not zero
# the evidence against the null hypothesis is strong: 
# the p-value for the intercept (alpha) is <2e-16 ***, indicating strong evidence to reject the null hypothesis
# meaning that there is strong evidence that you will observe values as or more extreme than the intercept
# therefore negating the null hypothesis
# There is a similar level of evidence for the beta coeff, where the p-value is <2e-16 ***

# add the variable of the predicted bweight to the bab9 dataset (y)
bab9<-bab9|>
  mutate(y = model_1$fitted.values)

# plot the regression line with the scatterplot using the predicted y values, 
# on top of the actual measured values
plot(bab9$gestwks, bab9$bweight)
lines(bab9$gestwks, bab9$y)

# Q4. Investigate if there is an association between maternal age and birthweight 
# using a t-test and the linear regression output. 
# Check if you get the same results with your t-test as the p-value presented in the regression output.
model_2<-lm(bweight ~ m_age, data=bab9)
summary(model_2)
confint(model_2)
coef(model_2)
# median residuals: 67.76
# intercept: 2935.959 grams
# 95% confidence interval for intercept: (2488.249353, 3383.66837) grams
# regression coeff: 5.686 grams
# 95% confidence interval for m_age: (7.407819, 18.78065) grams
# ALSO, THE NULL HYPOTHESIS DOESN'T INCLUDE ZERO -> more certainty that the true value of ebta is not zero
# the p-value for the beta coeff of maternal age is high: 0.394 
# this means there is not evidence to reject the null hypothesis
# that there is no relationship between m_age and bweight

## CORRELATION ##

# examine the relationship ebtween two variables without assigning one as an exposure and one as outcome
# The association is then measured by the correlation between the two variables
cor(bab9$bweight, bab9$gestwks)
# correlation coeff r = 0.7375501
# this always lies betwwen -1 and 1, which r=0 meaning no linear relationship
# r= -1 being a negative relationship, and r=1 being a positive relationship

# Q5. What is the value of the correlation coefficient calculated above? What does this mean? 
# the correlation coefficient is 0.7375501, meaning that there is somewhat strong evidnece
# that there is a positive correlation between the two variables

## CORRELATION AND LINEAR REGRESSION DON'T WORK FOR NON-LINEAR RELATIONSHIPS ##

# making a non-linear dataset (noting the x^2)
set.seed(101)
nonlinear <- data.frame(x = 1:100 + 2 * runif (n = 100)) %>%
  mutate(y = (50 - x)^2 + (100 * runif(n = 100)))

# Q6. Examine the two variables (x and y) in the nonlinear dataset 
# using the techniques you’ve learned today. 
# Looking at the scatter plot, is there a relationship between the two variables? 
# Does a line of best fit provide a good approximation of the relationship? 
# Does the Pearson correlation coefficient suggest an association? 
# Does regression analysis provide evidence of an association? 
# If so, interpret the regression coefficient. If not, why not?
plot(nonlinear$x, nonlinear$y, xlab="x", ylab="y") 
lines(nonlinear$x, nonlinear$y)
# there is a seemingly nonlinear, parabolic relationship
# the line of best fit shows the relationship and that it is definitely not linear
cor(nonlinear$x, nonlinear$y)
# the pearson's coefficient for fcorrelation shows almost no evidnece of a linear relationship (r=0.1152191)
model_3<-lm(x ~ y, data=nonlinear)
summary(model_3)
confint(model_3)
coef(model_3)
# the regressiona anloysis shows little evidence of a linear relationship
# the beta is 0.004492047, meaning on average, there is little evidence 
# of a consistent increase in x in relation to y
# this si especially true because the 95% confidence interval contains zero (-0.003271296, 0.01225539)
# meaning there is no certainty that the null hypothesis is false
# the p-value of 0.254 for the beta regression coeff also gives little evidence that the null hypothesis is false
# meaning that there is little evidence to reject the hypothesis that there is no relationship between the variables




