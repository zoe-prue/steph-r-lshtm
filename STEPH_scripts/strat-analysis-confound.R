# Statified analysis and confounding practical
# 14/11/2025
# Explore stratified analysis of 2x2 tables

###################################
# library calls

library(rio)
library(tidyr)
library(DescTools)
library(dplyr)
library(gmodels)

###################################
# global variables

setwd("~/Desktop/steph-r-lshtm/raw_STEPH_data")
creche <- read.csv("creche.csv")

###################################
# script

# explore data

head(creche)
nrow(creche)

# cross tabulate the data

creche_lrti_table <- CrossTable(creche$lrti, creche$creche)

# perform chi-squared test

chisq.test(creche$lrti, creche$creche)

# Q1. Is there statistical evidence of an association between creche attendance and lrti?
# There is strong statistical evidence to reject the null hypothesis that there is no association 
# between creche attendance and lrti, due to the p-value being 1.038e-07

## Statified analysis

creche_socio_table <- CrossTable(creche$socio, creche$creche)

# chi-squared test for association between SES and chreche attendance

chisq.test(creche$socio, creche$creche) 

# Q2. What do you conclude?
# there is strong evidence (p-value = 0.0002324) for an association between
# creche attendance and SES, meaning that this cound be a confounder
# because creche is related to SES, but SES is not on the causal pathway between creche and lrti

# filter the dataset creche to separate it into two 2x2 tables;
# one 2x2 table contains the creche and lrti data from low SES children
# and one 2x2 tbale contain the creche and lrti data from average SES children

creche_low <-creche|>
  filter(socio==1)
CrossTable(creche_low$lrti, creche_low$creche, prop.r = TRUE, chisq = TRUE)
?CrossTable()

creche_avg <-creche|>
  filter(socio==0)
CrossTable(creche_avg$lrti, creche_avg$creche, prop.r = TRUE, chisq = TRUE)

# Q3. What percentage of children attending a creche had lower respiratory tract infection 
# if they were from a higher socio-economic class?  
# 12.3% of children with lrti who attended chreche were from an average SES background

# Q4. What percentage of children attending a creche had lower respiratory tract infection 
# if they were from a lower socio-economic class?
# 79.2% of children with lrti who attended creche were from a lower SES background

## Using R to calculate odds ratios

# create a 2x2 table of frequencies

table(creche$lrti, creche$creche)

# and calculating the odds by hand, using R as a calculator

# Odds among those attending creche:
odds_creche <- 27/62
odds_creche

# Odds among those not at creche
odds_no_creche <- 53 / 499
odds_no_creche

# Odds ratio
odds_ratio <- odds_creche / odds_no_creche
odds_ratio

# Q5. What are the odds of having lower respiratory tract infection 
# for each of the two creche attendance groups? What is the odds ratio? 
# What does this mean?
# The odds of having lrti with creche attendance is 0.4354839
# The odds of having lrti without creche attendance is 0.1062124
# The odds of having lrti with creche attendance is 4.1x that of children without creche attendance

# checking odds ratio twice with another command

DescTools::OddsRatio(table(creche$lrti, creche$creche), conf.level = 0.95)

# looks at odds ratios on creche attendance stratified by SES:

DescTools::OddsRatio(creche_low$lrti, creche_low$creche, conf.level = 0.95)
DescTools::OddsRatio(creche_avg$lrti, creche_avg$creche, conf.level = 0.95)

# note that they are similar -> points to confounding

# now calculate the pooled odds ratio (also called common odds ratio) and test with M-H test

mantelhaen.test(creche$lrti, creche$creche, creche$socio, conf.level = 0.95)

# 3.460834 is the pooled odds ratio -> similar to the individual strata

# test for homogeneity of odds ratios
# test the hypothesis that the ORs in the two social class strata are the same

# This is testing if the values “3.457064” and “3.464706” are from populations with truly different odds ratios

tab1 <-xtabs( ~ lrti + creche + socio, data = creche)
WoolfTest(tab1)

# Q6. What is the MH odds ratio controlling for socio-economic status? What does it mean?
# It's controlling for SES as a confounder -  essentially taking out the effect of SES on lrti, 
# making the association between creche and lrti more direct and clear (taking out SES as "noise")

# Q7. How does this differ from the odds ratio for the effect of creche attendance 
# on lower respiratory tract infection when socio-economic status was ignored? 
# The OR was lowered when the SES factor was removed. 
# This means that SES was acting as a confounder and contributing to the association between
# creche attendance and lrti (SES was a positive? confounder)
# pathway: low SES -> creche AND SES -> lrti AND creche -> lrti

# Q8. Why has it decreased?
# SES was a negative confounder, which means it added to the association between creche and lrti
# falsely making the relationship look stronger between the two

# Q9. Interpret the confidence interval for the MH odds ratio
# the 95% CI (1.720534, 6.961425) gives a wide range for plausible values 
# where the true population odds rationmay fall.
# this suggests a high degree of uncertainty, due to the OR being very wide.
# 1 in 20 times, the true population OR will fall in this range
# the OR range does not include the null value of 1 (which would mean no association between creche and lrti)
# which does mean that there is good evidence to reject the null hypothesis

## OPTIONAL MATERIAL ##

# analyzing frequency records

bednets <- read.csv("bednets_optional.csv")

# data here in bednets are coded: 1 is yes, 0 is no

# expand frequency records to become individual records
# using tidyr uncount() function

expanded_data <- bednets |>
  uncount(freq)

# drops freq afterwords bc it becomes redundant 

CrossTable(expanded_data$bednet, expanded_data$spleen, prop.r = TRUE, chisq = TRUE)

# stratify by village for futher analysis of confounding

expanded_data_v1 <-expanded_data |>
  filter(village==1)
CrossTable(expanded_data_v1$bednet, expanded_data_v1$spleen, prop.r = TRUE, chisq = TRUE)

expanded_data_v2 <-expanded_data |>
  filter(village==2)
CrossTable(expanded_data_v2$bednet, expanded_data_v2$spleen, prop.r = TRUE, chisq = TRUE)

# Q10. Examine the association between bednets and having an enlarged spleen, 
# and investigate if it is confounded by village.
# with the villages combined, the p-value is < 0.002, meaning that there is strong evidence 
# to reject the null hypothesis that there is no association between bednet usage and spleen enlargment.
# this means there appears to be a strong association.
# the odds ratio could also be calcuated to further confirm this and help assess confounding later.
# when stratified by village, village 1 shows a p-value of 0.434, meaning that there is insifficient evidence
# to reject the null hypothesis that in village 1, bednet usage is not associated with spleen enlargement.
# this means that there does not appear to be an association between bednet usage and spleen enlargement in village 1.
# Furthermore, village 2 shows a p-value of 0.823, meaning that there is also insufficient evidence
# to reject the null hypothesis that in village 2, bednet usage is not associated with spleen enlargement.
# this means that there does not appear to be an association between bednet usage and spleen enlargement in village 2 as well.
# Overall, this provides evidence for the idea that village is a confounder for bednet usage and spleen enlargment







