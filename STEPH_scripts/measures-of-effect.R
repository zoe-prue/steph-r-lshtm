# Measures of effect practical
# 29/10/2025
# Explore how to use measures of effect in R

###################################
# library calls

install.packages("DescTools")
library(DescTools) 

###################################
# global variables

###################################
# script

# create a matrix of the net usage data and malaria occurence

net_matrix <- matrix(c(27, 64, 46, 41), nrow = 2, byrow = TRUE)
rownames(net_matrix) <- c("Permethrin net", "No permethrin net")
colnames(net_matrix) <- c("Enlarged spleen", "Normal spleen")
net_matrix

# calculate row percentages

prop.table(net_matrix, margin =  1)

# ‘margin=1’ means row percentages
# column percentages would be calculated with ‘margin=2’.

# perform chi-squared test

chisq.test(net_matrix)

# X-squared = 8.9625, df = 1, p-value = 0.002756

## Look at each effect in turn:

# A. The absolute increase in risk (risk difference) of enlarged spleen
# in those who slept without nets compared to those who slept under nets

# will use BinomDiffCI from DescTools package
# enter x1, n1, x2, n2, which are:
# exposed with outcome, exposed, unexposed with outcome, unexposed
# uses the Wald method to calculate the confidence intervals

BinomDiffCI(27, 91, 46, 87, method=c("wald"))

# estimated population mean is -0.2320323
# (-0.3727839, -0.0912808) 95% CI
# better to report as “We found that Permethrin bednets were associated with
# a reduction in the risk of enlarged spleen by 23% (95% CI: 9%, 37%)"

# B. The relative decrease (risk ratio) in risk of enlarged spleen 
# in those who slept under nets compared to those who slept without them.
# RelRisk() from DescTools package

RelRisk(net_matrix, conf.level = 0.95, method = 'wald')

# estimated relative risk is 0.5611562
# (0.3881177, 0.8113424) 95% CI

# C. The relative decrease in odds (odds ratio) of enlarged spleen 
# in those who slept under nets compared to those who slept without them.

# OddsRatio() from DescTools package

OddsRatio(net_matrix, conf.level = 0.95)

# estimated odds ratio is 0.3760190
# (0.2030803, 0.6962286) 95% CI

# Now create a new matrix and use the same functions to calculate the risk of sleeping without a net, 
# rather than the protective effect of sleeping under a net. 

net_matrix_risk <- matrix(c(46,41,27,65), nrow = 2, byrow = TRUE)
rownames(net_matrix) <- c("No permethrin net", "Permethrin net")
colnames(net_matrix) <- c("Enlarged spleen", "Normal spleen")
net_matrix_risk

# risk difference for risk of not using nets 

BinomDiffCI(46, 87,27, 91, method=c("wald"))

# # estimated odds ratio is 0.2320323
# (0.0912808, 0.3727839) 95% CI

# relative risk

RelRisk(net_matrix_risk, conf.level = 0.95, method = "wald")

# estimated relative risk is 1.801618
# (1.245295, 2.606472) 95% CI

# odds ratio

OddsRatio(net_matrix_risk, conf.level = 0.95)

# estimated odds ratio is 2.700994 - notice this is the inverse of the original!
# (1.459847, 4.997351) 95% CI

# E. Paired Data

paired_data <- matrix(c(28, 21, 8, 61), nrow = 2, byrow = TRUE)
rownames(paired_data) <- c("No restrictions", "Some restrictions")
colnames(paired_data) <- c("No restrictions", "Some restrictions")
paired_data

# calculating odds ratio for discordant pairs
# got 2.625 on pen and paper
# we don't have a package provided in the practical to calculate the odds ratio in R

# measurements are more similar, have to take into account when calculating

# Perform McNemar's test without continuity correction for chi-squared approximation

mcnemar_exact <- mcnemar.test(paired_data, correct = FALSE)

# note that correct = FALSE is telling the function not to use continuity correction
# continuity correction is an adjustment made in a chi-square test for 2x2 contingency tables 
# to account for the fact that the chi-square distribution is continuous while the data is discrete.
# it is applied when sample size is small, and is more conservative of an approach than without
# I don't know how to do continuity correction on pen and paper, it seems

# Perform McNemar's test with continuity correction for chi-squared approximation
# very standard to calculate this way

mcnemar_chi <- mcnemar.test(paired_data, correct = TRUE)

# got chi-squared equals 4.965517, same as on pen and paper (4.966)

# everything aligns with the answers I got on pen and paper!

