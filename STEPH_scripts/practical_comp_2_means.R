# Practical Comparing Two Means
# 20/10/2025
# STEPH practical learning to compare two means with software

# notes
# packrat to check if all packages are compatible in an R script

###################################
# library calls/package installs
install.packages("psych")
install.packages("tmvnsim")
library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)
library(haven)  # for loading .dta files

###################################
# global variables

setwd("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/LSHTM_STEPH/raw_STEPH_data")
bab9 <- read.csv("bab9.csv")

###################################
# script

# examine data

str(bab9)
summary(bab9)
head(bab9)

# DECRIBE DIST. OF CONT. VARIABLE

summary(bab9$bweight)
summary(bab9$bweight, detail = TRUE) #
# ?summary()

# or use psych package

describe(bab9$bweight)

# now make a histogram of the bweights

bweight_hist <- ggplot(bab9, aes(x=bweight)) +
  geom_histogram(bins = 12, fill = "#648FFF", color = "black") +
  ggtitle("Histogram of Birthweight") +
  theme(
    plot.title = element_text(size=15)) +
  xlab("Birthweight") +
  ylab("Frequency")

bweight_hist

# now make a histogram with normal distribution

bweight_hist_norm <- ggplot(bab9, aes(x = bweight)) + 
  geom_histogram(aes(y = ..density..), bins = 12, fill = "#648FFF", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(bab9$bweight, na.rm = TRUE), 
                                         sd = sd(bab9$bweight, na.rm = TRUE)), 
                color = "red", linewidth = 1) +
  ggtitle("Histogram of Birth Weight with Normal Distribution") +
  xlab("Birth Weight") +
  ylab("Density")

bweight_hist_norm

# can leave or omit bins = 12, will change the number of bins

# now look at histogram of birthweights for boys only

# now make a histogram with normal distribution

bweight_hist_boys <- ggplot(bab9 %>% filter(sex == 1), aes(x = bweight)) + 
  geom_histogram(bins = 12, fill = "#648FFF", color = "black", aes(y = (..count..)/sum(..count..)*100)) +
  ggtitle("Histogram of Birth Weight (Boys Only)") +
  xlab("Birth Weight") +
  ylab("Percent")

bweight_hist_boys

# check out the fact that boys and 1, and girls are 2

table(bab9$sex)

# plot both plots side by side using facet_wrap() function in ggplot2

bweight_hist_facet_sex <- ggplot(bab9, aes(x = bweight, fill = factor(sex))) + # need to factor sex to make a continuous variable (1 or 2) discrete
  geom_histogram(bins = 12, color = "black", show.legend = FALSE) +
  facet_wrap(~sex, scales = "free_y", # should the scales be free_y?
             labeller = labeller(sex = c(`1` = "Boys", `2` = "Girls"))) + # why concatinate
  scale_fill_manual(values = c("1" = "#648FFF", "2" = "#DC267F")) +
  ggtitle("Histogram of Birthweight by Sex") +
  xlab("Birthweight") +
  ylab("Count") +
  theme_minimal(base_size = 15)

bweight_hist_facet_sex

# CONFIDENCE INTERVALS FOR THE MEAN

# making a function to make confidence interval calculations with t-distribution

mean_ci <- function(x, conf.level = 0.95) {
  n <- length(x) # number of observations
  m <- mean(x) # mean of the observation
  se <- sd(x) / sqrt(n) # SE or measurement of sd's from the true population mean that the sample mean is in a sampling distribution
  error <- qt(conf.level / 2 + 0.5, n - 1) * se # qt gives t-score of the pth quantile, using d.f., division by two gives you two-sided p-value
  c(lower = m - error, mean = m, upper = m + error)
}

?qt()

# finding mean 95% confidence intervals of birth weights, gestation weeks, and m_age (?)

mean_ci(bab9$bweight)
mean_ci(bab9$gestwks)
mean_ci(bab9$m_age)

# or 99% CIs

mean_ci(bab9$bweight, conf.level = 0.99)
mean_ci(bab9$gestwks, conf.level = 0.99)
mean_ci(bab9$m_age, conf.level = 0.99)

# DIST OF RESPONSE VAR ACROSS SUBGROUPS

# mean birth weight by sex

mean_bweight_bab9 <- bab9 %>%
  group_by(sex) %>%
  summarize(mean_bweight = mean(bweight, na.rm = TRUE))

# ensure that sex column is factored (discrete)
# map 1 to male and 2 to female

# boxplot of birthweight by sex

bweight_by_sex_bplot <- ggplot(bab9, aes(x=factor(sex), y=bweight)) +
  geom_boxplot() +
  labs(title = "Boxplot of Birthweight by Sex", x = "Sex", y = "Birthweight") +
  theme_minimal()

bweight_by_sex_bplot

# discussion: 
# are distributions roughly normal with similar spread of values? 
# are sample sizes still reasonably large between the two groups?
# answer:
# the data are skewed, or not normally distributed. 
# the shaped of the boxplots and histogram sare indicative of this
# the data sets are both reasonably large (n=315 for males, and 326 for females)

# COMPARING MEANS

# In R, there is a function `t.test` to compare two means
# 't.test' has options that allow the standard deviations in the two groups to be different 
# or to require them to be the same (the default)

# one sample t-test

t.test(bab9$bweight, mu = 3300)

# what do you conclude from this test?

# we want to test the hypothesis that the mean birth weight is not 3300g 
# (can be a two-sided p-value, either extreme is interesting to us)
# the p-value is extremely small, meaning that the null hypothesis can be rejected
# the 95% CI is (3078.507, 3179.767) which does not include null value,
# with he best estimate being 3129.137

# Two-sample t-test
# comparing two means

t.test(bab9$bweight ~ bab9$sex)

# we want to test the hypothesis that the mean birth weights between males and females are equal
# (can be a two-sided p-value, either extreme is interesting to us)
# the p-value is <0.002, meaning that the null hypothesis can be rejected
# the 95% CI is (-267.57246, -66.73186) which does not include null value of 0
# two means are 044.127 and 3211.279, respectively


# discuss:
# what do you conclude about the difference in birth weight between boys and girls?
# there is a statistically significant difference in the mean birth weights between males and females

# this test assumed ^ approximately equal standard deviations between the two groups
# this assumption can be tested by this F test to compare two variances:

# F test to compare two variances
# WE DID NOT DO THIS IN CLASS!!!!!!!!!!!!!! REVISE ASSUMPTIONS OF T-TEST

var.test(bab9$bweight ~ bab9$sex)

# test calculates the ratio of the standard deviation of birthweight among males divided by the SD of birthweight among females
# divide because were finding a ratio, and hope it should be 1
# the null hypothesis is that the SDs are the same in both groups
# therefore, because the p-value = 0.3037, the null hypothesis cannot be rejected
# so we cannot reject the null hypothesis that the SDs are the same in each group
# SDs are spread of data, and if one have small spread and the other has big
# t-tests assume that the SDs are the same

# IF SDs were statistically significantly different, we could use this:

t.test(bab9$bweight ~ bab9$sex, var.equal = FALSE)

# allows for unequal standard deviations when it calculates the standard error of the difference
# or we can do a non-parametric test...

# discuss:
# was it reasonable to use the t-test with equal standard deviations to compare birthweights between boys and girls?
# yes, because there was not a statistically dignificant difference in the standard deviations between the boys and the girls groups
# so the t-test was applicable
# however, we could have used the Z test due to the large sample sizes




