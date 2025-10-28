# Non-Parametric Methods Practical Week 5 (Comparing 2 Means)
# 27/10/2025
# Non-parametric methods are statistical methods for data that are non-normally distributed

###################################
# library calls

library(rio)
library(dplyr)
library(ggplot2)

###################################
# global variables

setwd("~/Desktop/steph-r-lshtm")
cmv_set <- read.csv("raw_STEPH_data/CMV_data.csv")
non_par <- read.csv("raw_STEPH_data/nonparametric.csv")

###################################
# script

# EXPLORING THE DATA

head(cmv_set)
summary(cmv_set)

# Q1. What is the mean level of CMV and standard deviation in this population?

mean(cmv_set$cmv)
sd(cmv_set$cmv)

# the mean cmv is 1.005588, and the sd is 0.4805313

# Q2. Does the distribution of CMV levels follow a Normal distribution?

ggplot(cmv_set, aes(x=cmv)) +
  geom_histogram(color = "black", fill = "#648FFF") +
  xlab("CMV") +
  ylab("Counts") +
  ggtitle("Histogram of CMV counts")

# by looking at a histogram, is it clear that the data are non-normally distributed

# Q3. Given your response to the above question, how might you better summarise the CMV data? 
# Run an appropriate R command to find out these summary statistics.

median(cmv_set$cmv) # less volatile than the mean
IQR(cmv_set$cmv) # gives us the range of the values b/w the 25th and 75th percentile of observations

# TESTING FOR DIFFERENCES

cmv_set |>
  filter(sex=="Male") |>
  pull(cmv) |> 
  hist(,main="Males", xlab = "CMV IgG OD")

cmv_set|>
  filter(sex=="Female") |>
  pull(cmv) |>
  hist(, main = "Females", xlab = "CMV IgG OD") 

# Q4. What are the median CMV levels in the two groups?

cmv_set %>%
  filter(sex == "Male") %>%
  summarize(median_cmv_male = median(cmv, na.rm = TRUE))

# the median CMV of males is 0.9835

cmv_set %>%
  filter(sex == "Female") %>%
  summarize(median_cmv_female = median(cmv, na.rm = TRUE))

# the median CMV of females is 1.0165

# Q5. Looking at the histograms and the difference in medians, 
# do you think there is a different level of CMV in these two groups? 
# Have a think about the p-value you might find from a formal hypothesis test. 
# How small do you think it might be?

# by looking at the histograms and medians, 
# I think there is not a statistically significant difference
# in the medians between the two groups
# The null hypothesis would be that no such difference exists
# and the p-value would have to be very small to reject the null hypothesis
# I suspect our p-value will be large

# Wilcoxon rank sum test

wilcox.test(cmv ~ sex, data = cmv_set)

# W = 247215, p-value = 0.06844

# Q6. Interpret the p-value from this test

# the p-value is not very small (<0.05)
# this gives us little evidence to reject the null hypothesis that there is a difference 
# in cmv medians between females and males

# Q7. Is there a difference in CMV level between people with and without a TB diagnosis?

# check how many people have tb in this dataset

nrow(cmv_set[cmv_set$tb2 == "TB",])

# 17 people had tb

cmv_set %>%
  filter(tb2 == "TB") %>%
  summarize(median_cmv_tb = median(cmv, na.rm = TRUE))

# cmv in TB cases was 1.357

cmv_set %>%
  filter(tb2 == "Non TB") %>%
  summarize(median_cmv_no_tb = median(cmv, na.rm = TRUE))

# cmv in non tb cases was 0.996

wilcox.test(cmv ~ tb2, data = cmv_set)

# the null hypothesis was that there is no difference in the median cmv of those diagnoses with TB and those without TB
# p-value = 0.001477, meaning that there is strong evidence to reject this null hypothesis.
# However, due to the sample size being 17 in the TB group, this data are prone to sampling error
# further data collection is required to provide more robust results

# TESTING A SINGLE SAMPLE

# running a one-sample Wilcoxon signed-rank test, 
# where the designated comparison value mu is pre-determined (it's 1 in this case)

wilcox.test(cmv_set$cmv, mu = 1)

# V = 472734, p-value = 0.7207
# meaning that there is little evidence to reject the null hypothesis that the cmv is equal to 1

# A researcher is interested in the blood pressure of people with TB, 
# and wants to use your dataset to check her hypothesis that people with TB have an average systolic blood pressure of 120. 
# Filter out the  17 people with a TB diagnosis in these data, 
# look at the distribution of SBP among them, and run an appropriate test of her hypothesis.


cmv_set %>%
  filter(tb2 == "TB") %>%
  summarize(median_sbp_tb = median(sbp, na.rm = TRUE))

# median sbp in TB group is 130

cmv_set %>%
  filter(tb2 == "Non TB") %>%
  summarize(median_sbp_no_tb = median(sbp, na.rm = TRUE))

# median sbp is 120.25 in non TB group (not important here, just cool to know)

wilcox.test(cmv_set$sbp, mu = 120)

# V = 88213, p-value = 0.06914

# Q8. Write a short paragraph, of the sort you might email to her, to describe the data, 
# explain the test you have performed (and why), and to interpret the result.

# The hypothesis that the median sbp of patients with TB equals 120 
# was not supported by the Wilcoxon signed rank test. 
# The test yielded a p-value of 0.06914
# which gives strong evidence to reject your hypothesis

# PARIED DATA

# make histogram

hist(non_par$sfa, bins = 10, main = "SFA", xlab = "skinfold thickness")
hist(non_par$sfb, bins = 10, main = "SFB", xlab = "skinfold thickness")

# apply Wilcoxon Signed Rank test for paired data

wilcox.test(non_par$sfa, non_par$sfb, paired = TRUE) 

# p-value = 0.002625
# meaning that there is strong evidence to reject the null hypothesis that the two groups are equal
# there is strong evidence that there is a difference in the way the two observers measure skinfolds]

# Q9. For comparison, run a t-test. What do you conclude?

# t.test(non_par$sfa ~ non_par$sfb) or t.test(non_par$sfa ~ non_par$sfb, var.equal = FALSE)?

t.test(non_par$sfa, non_par$sfb, paired = TRUE, alternative = "two.sided")

# p-value = 0.001547

t.test(non_par$sfa, non_par$sfb, paired = TRUE, alternative = "two.sided", var.equal = FALSE)

# p-value = 0.001547

# The p-value provides strong evidence to reject the null hypothesis that the difference in measurements is zero
# the p-value is smaller and less conservative






