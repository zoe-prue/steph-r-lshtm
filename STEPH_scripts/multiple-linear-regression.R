# Multiple linear regression
# 2/12/2025
# explore multiple linear regression

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

# Q1. Draw a figure to illustrate the causal relationships between age, depression, and number of children. 
# Would you consider age to be a potential confounder?
# done in practical paper

# is age (confounder) associated with exposure (number of children)

plot(depress$age, depress$children, xlab="age", ylab="number of children")

plot(depress$age, depress$depscore, xlab="age", ylab="depression score")

# Recall:
# Y_i= α+ βx_i  + ε_i

# preliminary analysis of children and depression

model1 <-lm(depscore ~ children, data = depress)
summary(model1)

# we will use multiple regression to examine the association between depression 
# and the number of living children, adjusted for age

model2 <-lm(depscore ~ children + age, data = depress)
summary(model2)

# confidence interval for regression coeff

confint(model2)

# remember that:

# Y_i= α+ β_1 x_1i + β_2 x_2i  + ε_i

# depression score = 2.70  + (-0.0049)x(number of living children) + (0.15)x(age) 

# Q2. Does age confound the association between number of children and depression?
# the regression coefficient of 0.15 means that for every year increase in age, 
# the depression score increases by 0.15 point
# and for every -0.0049 decrease in children, the depression score rises 
# However, the children coeff this has a wide confidence interval containing the null value 
# and an extremely high p-value meaning that this finding is insignificant
# meaning that age could be a confounder because the coefficient changed between the first and second regression

# Q3. What is the effect of number of children on depression score adjusted for age?
# The effect of children on the number of children on depression score adjusted for age is insignificant
# said that depression increased with more children, but since the p-value is high, 
# we cannot reject the null hypothesis of no association
# or that beta could equal zero

# BTW the coefficient for the effect of age, β_2, is not interpretable 

# Q4. If we are interested in the effect of age on depression score, why is the number of children not a confounder? 
# interpreting beta_2 would not be ok because we are not looking a the causal pathway
# between age and depression, we are only interested in this as confounding
# it would be a completely different equation (simple linear regression) 
# interpreting the effect of age on depression would go through a causal pathway (number of childern as intermediate)

# Q5. What regression model should we fit instead if we are interested in the effect of age on depression score?
# imagine age -> depression, with an unknown confounder C at top of pyramid
# we would do a simple linear regression showing the effect of age (and its beta) on the depression score
# if there;s another confounder, I would adjust for that 
# can talk about unmeasured confounding, but this is beyond this course

## DOES AGE CONFOUND THE RELATIONSHIP BW PROLAPSE AND DEPRESSION? ##

# Q6. done in practical paper

# Q7. Using descriptive statistics or a graph, does it appear that age is related to prolapse?  

boxplot(age ~ type_prolapse, data = depress)

# answer seven:
# older women more likely to have prolapse based on boxplot
# no effect of age on the type of prolapse (moderate vs severe)

# Q8. Does age likely confound the association between the type of prolapse and depression?

# Now, let’s look at the crude association between type of prolapse and depression 

model3 <- lm(depscore ~ as.factor(type_prolapse), data = depress) # same as last practical of simple linear regression
summary(model3)

# We can now examine the effect of type of prolapse on depression adjusting for age.

model4 <-lm(depscore ~ as.factor(type_prolapse) + age, data = depress)
summary(model4)

# answer to 8:
# the beta_1 and beta_2 rise with the severity of prolapse
# with significant p-values
# indicating that we have strong evidence to reject the null hypothesis that the beta = 0
# However, when adjusting for age as a confounder, the type of prolapse increase in severity 
# does not associate with the depression score, meaning that 
# there is not sufficient evidence to reject the null hypothesis 
# that there is not an association between depression score and prolapse when adjusting for age
# meaning that age is likely a confounder

# Q9. What is the effect of type of prolapse on depression score adjusted for age?
# the prolapse is not significantly associated with depression when adjusted for confounding

### DO THE EXTRA PROBLEMS? ###

