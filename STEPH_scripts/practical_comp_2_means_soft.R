# Practical comparing two means with software
# 23/10/2025
# Compare two means but with software... seems so similar to the last class but whatever
# always using t-test with statistical software

###################################
# library calls

# Load necessary libraries
# install.packages("psych")
# install.packages("tmvnsim")
library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)
library(haven)  # for loading .dta files

###################################
# global variables

setwd("~/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/LSHTM_STEPH/raw_STEPH_data")
# getwd()
bab9 <- read.csv("bab9.csv")

###################################
# script

# DESCRIBING A CONT. VARIABLE

str(bab9)
summary(bab9)
head(bab9)
summary(bab9$bweight, detail = TRUE)

# describe() from psych package is more descriptive

describe(bab9$bweight)

# histogram of bweights in ggplot2

ggplot(bab9, aes(x=bweight)) +
  geom_histogram(bins = 12, color = "black", fill = "#648FFF") +
  ggtitle("Histogram of Birthweights") +
  theme(plot.title = element_text(size=15)) +
  xlab("Birthweight") +
  ylab("Counts")

# histogram of bweights with a normal distribution curve laid on top in ggplot2

ggplot(bab9, aes(x=bweight)) +
  geom_histogram(aes(y = ..density..), bins = 12, color = "black", fill = "#648FFF") +
  stat_function(fun=dnorm, args=list(mean=mean(bab9$bweight, na.rm = TRUE), 
                                     sd = sd(bab9$bweight, na.rm = TRUE)), color = "red", size = 1) +
  ggtitle("Histogram of Birthweights with Normal Distribution") +
  theme(plot.title = element_text(size=15)) +
  xlab("Birthweight") +
  ylab("Density")

# histograms in birthweight in base R

hist(bab9$bweight, main = "Frequency histogram", xlab = "birthweight (g)")
hist(bab9$bweight, main = "Density histogram", xlab = "birthweight (g)",freq = FALSE)
curve(dnorm(x, mean = mean(bab9$bweight), sd = sd(bab9$bweight)), add = TRUE)

# look at distributions only by type... base R pipeline

bab9 |>
  filter(sex=="1") |>
  pull(bweight) |>
  hist(, main = "Frequency histogram (boys)", xlab = "birthweight (g)")

# graph boy and girls alongside each other using base R

par(mfrow = c(1,2))
bab9 |>
  filter(sex=="1") |>
  pull(bweight) |>
  hist(, main = "Frequency histogram (boys)", xlab = "birthweight (g)")

bab9 |>
  filter(sex=="2") |>
  pull(bweight) |>
  hist(ylim = c(0,120), main = "Frequency histogram (girls)", xlab = "birthweight (g)")

# build a function to do confidence intervals

# CONFIDENCE INTERVALS FOR THE MEAN

mean_ci <- function(x, conf.level = 0.95) {
  n <- length(x)
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  error <- qt(conf.level / 2 + 0.5, n - 1) * se # this is because we are doing a two-sided CI
  c(lower = m - error, mean = m, upper = m + error)
}

# standard 95% CI

mean_ci(bab9$bweight)
mean_ci(bab9$gestwks)
mean_ci(bab9$m_age)

# change conf.level argument to be a 99% CI

mean_ci(bab9$bweight, conf.level = 0.99)
mean_ci(bab9$gestwks, conf.level = 0.99)
mean_ci(bab9$m_age, conf.level = 0.99)

# boxplot of bweight by boys vs girls

boxplot(bweight ~ sex, data = bab9, ylab = "Birthweight (g)")
title(main = "Boxplots of Birthweights Between Males and Female Infants")

# t-test for the difference between means in bweights bw the boys and girls

t.test(bab9$bweight, mu = 3300)

# t = -6.6269, df = 640, p-value = 7.284e-11

## DISCUSS: Do you think the distributions are roughly normal with a similar spread of values? 
## Are the sample sizes still reasonably large in each of the two groups?
## ANSWER: The distributions are roughly normal with a similar spread of values bw boys and girls
## The sample sizes are quite large between the two groups

# DIST OF VARS ACROSS SUBGROUPS

bab9 |>
  group_by(sex) |>
  summarise(mean_bweight = mean(bweight, na.rm = TRUE))

# COMPARING TWO MEANS

# alternative hypothesis: true mean is not equal to 3300
# t = -7, df = 640, p-value = 7e-11
# p-value < 0.001

## DISCUSS: What do you conclude from this test?
## ANSWER: There is a very low p value (<0.001) 
## meaning that there is a strong evidence to reject the null hypothesis 
## that there is no difference between the two groups

t.test(bab9$bweight ~bab9$sex)

# t = -3.2686, df = 638.65, p-value = 0.001139

## DISCUSS: What do you conclude about the difference in birthweight between boys and girls?
## ANSWER: There is statistically significant evidence to suggest 
## that there is a difference in the mean bweights bw boys and girls

# now, we can test the levels of variance 
# we assume tha tthe levels are variance are the same in the two populations, but this may not be true
# we can test this assumption statistically

var.test(bab9$bweight ~ bab9$sex)

# this tests the ratio of the standard deviations between the two groups, and seeing if they are not equal to one
# a ratio of variances equal to one would mean that the two groups' variances are not statistically significantly different
# F = 0.89107, num df = 314, denom df = 325, p-value = 0.3037
# meaning, that we cannot reject the null hypothesis that there is no difference in variances between the two pops

# not in this case, but IF the p-value was small, and there was sufficient evidence to reject the null hypothesis
# meaning that the difference in variances WAS significantly different...
# Then we could redo the t-test with this t-test code

t.test(bab9$bweight ~ bab9$sex, var.equal = FALSE)

## DISCUSS: Was it reasonable to use the t-test with equal standard deviations to compare birth weights between boys and girls?
## ANSWER: The ratio of standard deviations bw the two groups was significantly different (not equal to one)
## meaning that is was appropriate to use the t-test without the var.equal = FALSE argument
# the results were very similar to the original t-test without the var.equal = FALSE
# t = -3.2686, df = 638.65, p-value = 0.001139



