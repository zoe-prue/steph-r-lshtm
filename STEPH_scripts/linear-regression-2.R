# Linear Regression 2
# 2/12/2025
# Review simple regression and examine simple regression with binary and categorical exposure variables

###################################
# library calls

library(dplyr)
library(rio)
library(tidyr)
# install.packages("lmtest")
library(lmtest)

###################################
# global variables

setwd("~/Desktop/steph-r-lshtm/raw_STEPH_data")
depress <- import("depress.csv")

###################################
# script

# explore data

head(depress)

## REVIEW OF SIMPLE LINEAR REGRESSION ##

# least squares linear regression methods from the previous regression session

plot(depress$children, depress$depscore, xlab="number of children", ylab="depression")

# use jitter

plot(jitter(depress$children), jitter(depress$depscore), xlab="”number of children", ylab="depression")

# assess trend with simple linear regression
# plot line over the scatterplot with abline

model_dep <-lm(depscore ~ children, data = depress)
summary(model_dep)
confint(model_dep)
abline(lm(depress$depscore ~ depress$children)) # notice order of exposure and outcome

# Q1:  

# a) Which variable is the outcome in this regression and which is the exposure?
# the number of children is the exposure and the depression score is the outcome

# b) What is the equation of the regression line?
# y = 5.95184 + 0.38317x

# c) Is the association between depression score and number of living children positive or negative?
# the association is positive, as seen by the positive regression coefficient

# d) By how much does the depression score increase for each extra living child? 
# (i.e. what is the regression coefficient for children)
# For each living child, the depression score increased by 0.38317 (p < 2e-16)

# e) Is there any evidence against the null hypothesis that the slope of the line is zero?
# there is very strong evidence against the null hypothesis that the slope of the line is zero, due to the p-value (p < 2e-16)
# the 95% confidence interval does not cross zero, meaning that there is certainty that the null hypothesis is not true
# the fact that the CI does not cross zero provides very strong evidence against the null hypothesis

## CONFIDENCE INTERVALS AND THE REGRESSION COEFFICIENT ##

# b ± 1.96 x SE(b) = 0.38 ± (1.96 x 0.037)
# b is the estimated statistical parameter
# SE(b) is its standard error

# test the strength of evidence against the null hypothesis that the true effect is zero by calculating
# t = b / SE(b) = 0.38 / 0.037 = 10.27
# corresponds to the p-value

## REGRESSION WITH BINARY EXPLANATORY VARIABLES ##

# we have so far looked at simple linear regression for quantitative variables on x and y
# can extend to include categorical explanatory variables 
# or a mixture of discrete and continuous explanatory variables
# just need a continuous outcome variable

# derive the mean depression score for the two groups by typing:
depress |>
  group_by(any_prolapse) |>
  summarize(mean = mean(depscore)) 

# t-test to investigate if there is a difference between the groups
t.test(depscore ~ any_prolapse, data = depress, var.equal = TRUE)
# two sample t-test
# the t-statistic from the t-test is -6.0706, which gives p<0.0001.

# examine relationship using regression
pro_model <-lm(depscore ~ as.factor(any_prolapse), data = depress) # notice the as.factor()
summary(pro_model)

# regression coeff is difference between two groups

# regression equation
# Y= α+ βx + ε
# dep_score = α + β(any_prolaspe? 0 or 1) + ε = 6.68 + (1.09 x [any_prolapse? 0/1])

# When any_prolapse = No, x = 0, and the equation becomes
# despcore = 6.68
# And when any_prolapse = Yes, x = 1, the equation becomes
# depscore = 6.68 + 1.09 = 7.78
# the regression coefficient for any_prolapse is the difference in mean depression score 
# for women with and without a prolapse

## REGRESSION WITH CATEGORICAL VARIABLES ##

boxplot(jitter(depscore) ~ type_prolapse, data = depress)

# Q2. How would you interpret these box plots? Do you see any pattern?
# there are a lot of outliers
# the median values of the dep_score are different, and higher in the prolapse groups
# those with severe prolapse always had a depression score above zero

# run regression with these 3 types of prolapse

pro_t_model <-lm(depscore ~ as.factor(type_prolapse), data = depress)
summary(pro_t_model)

# simple testing the relationship between the betas and the baseline (what the betas are)
# The regression coefficient for moderate prolapse is 1.02
# The regression coefficient for severe prolapse is 1.26
# higher level of prolapse seems to indicate higher depression score

# equation:
# Y= α+ β_1 x_1 + β_2 x_2  + ε
# looks are slopes separately

# Q3. Compare the coefficients in the regression analysis to the box plots you made earlier?
# Which box is being compared to which for β_1? β_2?
# There is a lesser increase for the change from no to moderate prolapse (+1.02 depression score)
# compared to the greater increase (+1.26 depression score) between no and severe prolapse

# LIMTATIONS OF MULTIPLE SIMPLE LINEAR REGRESSION MODELS 
# notice that the above analysis has no p-values!!
# no comparison done above between the moderate and severe prolapse groups
# each predictive model (each simple linear regression) does not take into account the other variables
# prone to multiple testing. -> inflated type I error rate
# coefficients not comparable across models
# does not test an overall effect
# asks the question: does this exposure matter alone?

# we can test the null hypothesis that both β1 and β2 are equal to 0 using a global Wald test
# which assesses all coefficients at the same time
# asks: is the exposure as a whole associated with Y?
# one p-value for joint effect
# accounts for correlation between coefficients
# controls for type I error by not doing multiple testing
# asks the question: does this exposure matter at all/belond in the model?

waldtest(pro_t_model, terms = "as.factor(type_prolapse)")

# Q4. Interpret the results of the global Wald test. note the global explains the prolapse vairbale in its entirety
# slope of beta_1 and beta_2
# Is there evidence against the null hypothesis of no association between type of prolapse and depression score? 
# the null hypothesis of the Wald test if that variable (prolapse) explains some of the variability
# Tests at the same time if both beta_1 and beta_2 are zero (tested against the zero)
# no association, or no relationship, between type of prolapse and depression score (totality prolapse as a variable)
# we find that there is strong evidence against the null because there is a significant difference (p=1.004e-08)
# test several variables -> Wald is positive, see which beta has contributed with the simple regression
# Helpful if we had multiple variables in regression line

# in more detail (about the Wald test):
# The global Wald test evaluates a joint hypothesis whether all regression coefficients for a set of explanatory variables 
# are equal to zero (mathematically H0:β1=β2=β3=⋯=0), and therefore asks whether the explanatory variables, 
# collectively, have any association with the outcome.
# If this global Wald test shows no evidence against the null hypothesis, 
# then from a technical standpoint it is possible that an individual coefficient 
# would show strong evidence against the null hypothesis (e.g. H0: β1==0) on its own. 
# all analysis are guided by research question being asked
# for example, for a clinical trial comparing several treatment or exposure groups,
# we care about an OVERALL difference between the groups, assessed by the Walk test
# if the goal is to see what specific exposures differ from each other
# individual Wald tests or pairwise comparisons are needed,
# regardless of whether the global Wald test was significant






