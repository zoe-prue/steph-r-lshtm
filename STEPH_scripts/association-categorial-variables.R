# Association of categorical variables
# 11/10/2025
# PURPOSE

###################################
# library calls

install.packages("gmodels")
library(rio)
library(dplyr)
library(gmodels) 
library(DescTools)


###################################
# global variables

setwd("~/Desktop/steph-r-lshtm/raw_STEPH_data")
bab <- read.csv("bab9.csv")
data_mcc <- read.csv("mccxmpl.csv")

###################################
# script

summary(bab)
head(bab)
dim(bab)

## Q1. How many variables are there? How many observations? 
# there are 7 columns (variables) and 641 rows(observations)

# adding a binary low birthweight variable (0, 1)

bab <- bab|>
  mutate(lbw = if_else(bweight<2500,"1","0")) 

table(bab$lbw)

# making a table of the min, max, mean bweights

bab_lbw <- bab|>
  group_by(lbw) |>
  summarise(min = min(bweight), max = max(bweight), mean = mean(bweight), n = length(bweight))

# adding a variable that just takes the binary lbw variable and makes it a factor (Normal, Low)

bab$lbw_f <-factor(bab$lbw, levels = c(0,1), labels = c("Normal", "Low"))

table(bab$lbw_f)

table(bab$lbw_f, bab$ht)

## Q2. How many babies with low birthweight were born to women with hypertension? 
# 27 with low birthweight and hypertension

## Q3. How many babies with low birthweight were born to women with no hypertension?
# 53 with normal birthweight and no hypertension 

# calculating percentage of low birthweight babies in each hypertension group - calculate column percentages

prop.table(table(bab$lbw_f, bab$ht), margin =  2)

## Q4. Write down the risk of low birthweight in each hypertensive group, and hence calculate the risk difference.
# The risk difference of lbw in no hypertension group is 
0.69662921 - 0.90398551 
# = -0.2073563
# The risk difference of lbw in hypertension group is 
0.30337079 - 0.09601449
# = 0.2073563

## Q5. Use the 2x2 table to calculate the odds of low birthweight in each hypertensive group, and hence the odds ratio.
#         0   1
# Normal 499  62
# Low     53  27
# odds of low birthweight in normal hypertension group: 53/499 = 0.1062
# odds of low birthweight in high hypertension group: 27/62 = 0.4354
0.4354/0.1062
# = 4.099812
# the odds of lbw in the hypertensive group is 4.1 times that of the normal hypertension mothers

prop.table(table(bab$ht, bab$lbw_f), margin =  1)

# all percentages shown at once:

CrossTable(bab$lbw, bab$ht)

# to perform a chi-squared test of association use the chisq.test function:

chisq.test(table(bab$lbw_f, bab$ht))

# X-squared = 28.301, df = 1, p-value = 1.038e-07

## Q6. What is the chi-squared value? And the p-value? Write down an interpretation of this p-value.
# the chi-squared value is 28.301. 
# the p-value is 1.038e-07
# the p-value provides strong evidence to reject the null hypothesis 
# that there is no difference between the proportions 
# of the lbws of babies born from the hypertensive vs nonhypertensive mothers.

# if you have a small sample you can ask R to calculate Fisher's exact test:
fisher.test(table(bab$lbw_f, bab$ht)) 

# Z-test to compare two proportions

ztest_table <- bab |>
  group_by(ht) |>
  summarise(n = n(), lbw = sum(as.numeric(lbw)))

# conduct a Z-test
# x are lbw babies in each group, n is totals

prop.test(x = c(ztest_table$lbw), n = c(ztest_table$n))

# note that this includes a 95% CI, whereas the Chi-squared does not

# Risk ratios and odds ratios

# can calculate risk ratios 
# put outcome first in code, then exposure (order matters!)

DescTools::RelRisk(table(bab$lbw, bab$ht), conf.level = 0.95)

# can calculate odds ratio
# put outcome first in code, then exposure (order matters!)

DescTools::OddsRatio(table(bab$lbw, bab$ht), conf.level = 0.95)

## Q7. Check that the estimates for the risk and odds ratios correspond 
# to the answers you calculated in questions 4 and 5.
# they do!

# linear trends in proportions

# linear trends of lbw and weeks of gestation
# group gestweeks and create a categorical variable (statify)
# many valid ways to do this: can look at clinical implications and standards

hist(bab$gestwks) 

# we will do 5 groups of equal width (number of weeks)

# In this example, gestational age ranges from 24.69 to 42.35 weeks from 
summary(bab$gestwks) 
# which would give us five groups each spanning 3.5 weeks.

# we will also do this in groups of equal size (number of observations). 
# we have 641 observations so could divide these into 5 groups each with about 128 in each group

# make quintiles of gestational weeks
bab <- bab |> 
  mutate(gest5 = ntile(gestwks, 5)) 
table(bab$gest5) 

# see cut-points for groups
bab |> 
  group_by(gest5) |> 
  summarize(min_gestwks = min(gestwks), max_gestwks = max(gestwks), count = n()) 

# verify that low birthweight varies by the quintiles of gestational age by typing
prop.table(table(bab$lbw_f, bab$gest5), margin =  2)
chisq.test(table(bab$lbw_f, bab$gest5)) 

# create a table of the number of lbw babies in the gestational age strata

lbw_by_gest5 <- bab |>
  group_by(gest5) |>
  summarise(n = n(), lbw = sum(as.numeric(lbw)))

# perform a test for linear trend

prop.trend.test(lbw_by_gest5$lbw, lbw_by_gest5$n)
?prop.trend.test()

# X-squared = 133.34, df = 1, p-value < 2.2e-16

# Q8. What is your interpretation of these results?
# there is strong evidence to suggest a linear trend 
# where low birthweight increases linearly with increased gestational weeks.

# paired proportions

# mcnemar's test

table(data_mcc$case, data_mcc$control)

## Q9. Calculate the odds ratio by hand
# use discordant pairs (r/s)
(8/8)/(3/8)
# = 2.666667

# odds ratio using code 

DescTools::OddsRatio(table(data_mcc$case, data_mcc$control), 
                     conf.level = 0.95, paired = TRUE)

# wide CI due to low number of discordant pairs

# Now to conduct McNemar’s test we use the mcnemar.test function:

mcnemar.test(data_mcc$case, data_mcc$control)

# McNemar's chi-squared = 1.4545, df = 1, p-value = 0.2278
# meaning that there is little evidence to reject the null hypothesis


