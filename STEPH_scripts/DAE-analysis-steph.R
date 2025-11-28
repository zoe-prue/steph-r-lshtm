# DAE data analysis
# 15/11/2025
# DAE analysis script for generating calculations and creating graphs
# The Ghana Vitamin A Supplementation Trials Child Health Study 
# is a large-scale field trial of the effects of large doses of Vitamin A 
# given every 4 months to children under 5 in a rural area of Northern Ghana. 
# The data available for these sessions are drawn from the baseline survey 
# (i.e. prior to the intervention) conducted in the second quarter of 1990. 
# This involved household interviews with the child's main carer, 
# as well as a clinical examination at a central point. 
# Complete data were obtained on 1137 children under five years of age. 

###################################
# library calls

library(dyplyr)
library(DescTools)
library(gmodels)
library(rio)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(gt)
library(knitr)
library(kableExtra)

###################################
# global variables

setwd("~/Desktop/steph-r-lshtm/raw_STEPH_data")
vascat <- read.csv("vastcat_2025.csv")
total_n = 1137

###################################
# script

head(vascat)
nrow(vascat)
summary(vascat)

## summarizing the population by socio-demographic and socio-economic variables ##

vascat %>%
  count(agegp) %>%
  arrange(desc(n)) %>%
  mutate(prop_age = n/total_n)

vascat %>%
  count(sex) %>%
  arrange(desc(n)) %>%
  mutate(prop_sex = n/total_n)

vascat %>%
  count(motheduc) %>%
  arrange(desc(n)) %>%
  mutate(prop_motheduc = n/total_n)

vascat %>%
  count(handpump) %>%
  arrange(desc(n)) %>%
  mutate(prop_handpump = n/total_n)

## summarizing the population by health-related variables ##

vascat %>%
  count(bcgscar) %>%
  arrange(desc(n)) %>%
  mutate(prop_bcgscar = n/total_n)

vascat %>%
  count(measles) %>%
  arrange(desc(n)) %>%
  mutate(prop_measles = n/total_n)

vascat %>%
  count(admitted) %>%
  arrange(desc(n)) %>%
  mutate(prop_admitted = n/total_n)

vascat %>%
  count(anaemia) %>%
  arrange(desc(n)) %>%
  mutate(prop_anaemia = n/total_n)

## chi-squared test comparing two proportions ##
# first, calculated the expected values in each cell

vascat_2by2 <- table(vascat$currbf, vascat$vita)
vascat_2by2
# prop.table(table(vascat$vita, vascat$currbf), margin =  2)

chisq.test(vascat$currbf, vascat$vita, correct = FALSE)

## stratified analysis for confounding (e.g. age group) ##
# age group is a potential confounder because it can be associated with breastfeeding
# as well as associated with vitamin A deficiency
# but does not exist on the causal pathway between the two

CrossTable(vascat$agegp, vascat$currbf)

# chi-squared test for association between age group and breastfeeding

chisq.test(vascat$agegp, vascat$currbf, correct=FALSE) 

# the p-value < 2.2e-16 shows that agegroup is highly associated with breastfeeding
# meaning that is it highly unlikely values this extreme would exist
# if the null hypothesis (that there is no association) were true

## crude analysis vs stratified analysis ##

# 2x2 table

vascat_2by2 <- table(vascat$currbf, vascat$vita)
vascat_2by2

# Rename rows and columns
rownames(vascat_2by2) <- c("Breastfeeding: No", "Breastfeeding: Yes")
colnames(vascat_2by2) <- c("Vitamin A Deficient", "Vitamin A Normal")
vascat_2by2

# add row and column totals

vascat_2by2_totals <- addmargins(vascat_2by2)

# for PNG with title

setwd("~/Desktop/steph-r-lshtm/STEPH-output")
png("vascat_2by2.png", width = 700, height = 450, res = 120)
grid.newpage()
grid.text("Breastfeeding vs. Vitamin A Deficiency in Ghanian Children", 
          y = 0.7, gp = gpar(fontsize = 14, fontface = "bold"))
grid.table(vascat_2by2_totals, vp = viewport(y = 0.48, height = 0.7))
dev.off()

# odds ratio (crude)

# odds among those not breastfeeding

odds_nobf <- 270/74

# the odds of vitamin A deficiency in those who do not breastfeed is 3.648649

# odds among those breastfeeding

odds_bf <- 565/228

# the odds of vitamin A deficiency in those who breastfeed is 2.47807

# odds ratio of those who do and do not breastfeed in having vitamin A deficiency

crude_odds <- odds_nobf/odds_bf
crude_odds

# the odds ratio is 1.472375
# the odds of having vitamin A deficiency was 47.23% high in children who do not breastfeed compared to children who do breastfeed

crude_OR <- DescTools::OddsRatio(table(vascat$vita, vascat$currbf), conf.level = 0.95)

# STRAT ANALYSIS AGEGP #

# stratify by age group

vascat_1_11 <-vascat|>
  filter(agegp=="1-11") %>%
  select(c(vita, currbf, agegp))

vascat_12_23 <-vascat|>
  filter(agegp=="12-23") %>%
  select(c(vita, currbf, agegp))

vascat_24_35 <-vascat|>
  filter(agegp=="24-35") %>%
  select(c(vita, currbf, agegp))

vascat_36_47 <-vascat|>
  filter(agegp=="36-47") %>%
  select(c(vita, currbf, agegp))

vascat_48_59 <-vascat|>
  filter(agegp=="48-59") %>%
  select(c(vita, currbf, agegp))

OR_1_11

OR_1_11 <- DescTools::OddsRatio(vascat_1_11$vita, vascat_1_11$currbf, conf.level = 0.95)
OR_12_23 <- DescTools::OddsRatio(vascat_12_23$vita, vascat_12_23$currbf, conf.level = 0.95)
OR_24_35 <- DescTools::OddsRatio(vascat_24_35$vita, vascat_24_35$currbf, conf.level = 0.95)
OR_36_47 <- DescTools::OddsRatio(vascat_36_47$vita, vascat_36_47$currbf, conf.level = 0.95)
OR_48_59 <- DescTools::OddsRatio(vascat_48_59$vita, vascat_48_59$currbf, conf.level = 0.95)

strat_OR_age <- data.frame(category = c("1-11", "12-23", "24-35", "36-37", "48-59"), 
                       OR = c(OR_1_11[1], OR_12_23[1], OR_24_35[1], OR_36_47[1], OR_48_59[1]), 
                       lower_ci_95 = c(OR_1_11[2], OR_12_23[2], OR_24_35[2], OR_36_47[2], OR_48_59[2]),
                       upper_ci_95 = c(OR_1_11[3], OR_12_23[3], OR_24_35[3], OR_36_47[3], OR_48_59[3]))
strat_OR_age$Group <- "Age Range"
pooled_OR_age <- mantelhaen.test(vascat$vita, vascat$currbf, vascat$agegp, conf.level = 0.95)
strat_OR_age$pooled_OR <- pooled_OR_age$estimate
  
# STRAT ANALYSIS SEX #

vascat_sex_f <-vascat|>
  filter(sex == "Female") %>%
  select(c(vita, currbf, sex))

vascat_sex_m <-vascat|>
  filter(sex == "Male") %>%
  select(c(vita, currbf, sex))

OR_f <- DescTools::OddsRatio(vascat_sex_f$vita, vascat_sex_f$currbf, conf.level = 0.95)
OR_m <- DescTools::OddsRatio(vascat_sex_m$vita, vascat_sex_m$currbf, conf.level = 0.95)

strat_OR_sex <- data.frame(category = c("Female", "Male"), 
                           OR = c(OR_f[1], OR_m[1]), 
                           lower_ci_95 = c(OR_f[2], OR_m[2]),
                           upper_ci_95 = c(OR_f[3], OR_m[3]))
strat_OR_sex$Group <- "Sex"
pooled_OR_sex <- mantelhaen.test(vascat$vita, vascat$currbf, vascat$sex, conf.level = 0.95)
strat_OR_sex$pooled_OR <- pooled_OR_sex$estimate

# STRAT ANALYSIS MOTHER EDUCATION #

vascat_motheduc_n <-vascat|>
  filter(motheduc == "No") %>%
  select(c(vita, currbf, motheduc))

vascat_motheduc_y <-vascat|>
  filter(motheduc == "Yes") %>%
  select(c(vita, currbf, motheduc))

OR_motheduc_n <- DescTools::OddsRatio(vascat_motheduc_n$vita, vascat_motheduc_n$currbf, conf.level = 0.95)
OR_motheduc_y <- DescTools::OddsRatio(vascat_motheduc_y$vita, vascat_motheduc_y$currbf, conf.level = 0.95)

strat_OR_motheduc <- data.frame(category = c("No", "Yes"), 
                           OR = c(OR_motheduc_n[1], OR_motheduc_y[1]), 
                           lower_ci_95 = c(OR_motheduc_n[2], OR_motheduc_y[2]),
                           upper_ci_95 = c(OR_motheduc_n[3], OR_motheduc_y[3]))
strat_OR_motheduc$Group <- "Mother's Education Status"
pooled_OR_motheduc <- mantelhaen.test(vascat$vita, vascat$currbf, vascat$motheduc, conf.level = 0.95)
strat_OR_motheduc$pooled_OR <- pooled_OR_motheduc$estimate

mantelhaen.test(vascat$vita, vascat$currbf, vascat$motheduc, conf.level = 0.95)

# STRAT ANALYSIS HANDPUMP #

vascat_handpump_n <- vascat|>
  filter(handpump == "No") %>%
  select(c(vita, currbf, handpump))

vascat_handpump_n_function <- vascat|>
  filter(handpump == "Not functioning") %>%
  select(c(vita, currbf, handpump))

vascat_handpump_y_function <- vascat|>
  filter(handpump == "Yes, functioning") %>%
  select(c(vita, currbf, handpump))

OR_handpump_n <- DescTools::OddsRatio(vascat_handpump_n$vita, vascat_handpump_n$currbf, conf.level = 0.95)
OR_handpump_n_function <- DescTools::OddsRatio(vascat_handpump_n_function$vita, vascat_handpump_n_function$currbf, conf.level = 0.95)
OR_handpump_y_function <- DescTools::OddsRatio(vascat_handpump_y_function$vita, vascat_handpump_y_function$currbf, conf.level = 0.95)

strat_OR_handpump <- data.frame(category = c("No", "Not functioning", "Yes, functioning"), 
                                OR = c(OR_handpump_n[1], OR_handpump_n_function[1], OR_handpump_y_function[1]), 
                                lower_ci_95 = c(OR_handpump_n[2], OR_handpump_n_function[2], OR_handpump_y_function[2]),
                                upper_ci_95 = c(OR_handpump_n[3], OR_handpump_n_function[3], OR_handpump_y_function[3]))
strat_OR_handpump$Group <- "Water Handpump Access (Y/N)"
pooled_OR_handpump <- mantelhaen.test(vascat$vita, vascat$currbf, vascat$handpump, conf.level = 0.95)
strat_OR_handpump$pooled_OR <- pooled_OR_handpump$estimate

# STRAT ANALYSIS BCG VACCINE #

vascat_bcg_n <-vascat|>
  filter(bcgscar == "No") %>%
  select(c(vita, currbf, bcgscar))

vascat_bcg_y <-vascat|>
  filter(bcgscar == "Yes") %>%
  select(c(vita, currbf, bcgscar))

OR_bcg_n <- DescTools::OddsRatio(vascat_bcg_n$vita, vascat_bcg_n$currbf, conf.level = 0.95)
OR_bcg_y <- DescTools::OddsRatio(vascat_bcg_y$vita, vascat_bcg_y$currbf, conf.level = 0.95)

strat_OR_bcg <- data.frame(category = c("No", "Yes"), 
                                OR = c(OR_bcg_n[1], OR_bcg_y[1]), 
                                lower_ci_95 = c(OR_bcg_n[2], OR_bcg_y[2]),
                                upper_ci_95 = c(OR_bcg_n[3], OR_bcg_y[3]))
strat_OR_bcg$Group <- "BCG Vaccination Scar (Y/N)"
pooled_OR_bcg <- mantelhaen.test(vascat$vita, vascat$currbf, vascat$bcg, conf.level = 0.95)
strat_OR_bcg$pooled_OR <- pooled_OR_bcg$estimate

# STRAT ANALYSIS MEASLES #

vascat_measles_n <-vascat|>
  filter(measles == "No") %>%
  select(c(vita, currbf, measles))

vascat_measles_y <-vascat|>
  filter(measles == "Yes") %>%
  select(c(vita, currbf, measles))

OR_measles_n <- DescTools::OddsRatio(vascat_measles_n$vita, vascat_measles_n$currbf, conf.level = 0.95)
OR_measles_y <- DescTools::OddsRatio(vascat_measles_y$vita, vascat_measles_y$currbf, conf.level = 0.95)

strat_OR_measles <- data.frame(category = c("No", "Yes"), 
                           OR = c(OR_measles_n[1], OR_measles_y[1]), 
                           lower_ci_95 = c(OR_measles_n[2], OR_measles_y[2]),
                           upper_ci_95 = c(OR_measles_n[3], OR_measles_y[3]))
strat_OR_measles$Group <- "Past Measles Infection (Y/N)"
pooled_OR_measles <- mantelhaen.test(vascat$vita, vascat$currbf, vascat$measles, conf.level = 0.95)
strat_OR_measles$pooled_OR <- pooled_OR_measles$estimate

# STRAT ANALYSIS HOSPITAL #

vascat_admitted_n <-vascat|>
  filter(admitted == "No") %>%
  select(c(vita, currbf, admitted))

vascat_admitted_y <-vascat|>
  filter(admitted == "Yes") %>%
  select(c(vita, currbf, admitted))

OR_admitted_n <- DescTools::OddsRatio(vascat_admitted_n$vita, vascat_admitted_n$currbf, conf.level = 0.95)
OR_admitted_y <- DescTools::OddsRatio(vascat_admitted_y$vita, vascat_admitted_y$currbf, conf.level = 0.95)

strat_OR_admitted <- data.frame(category = c("No", "Yes"), 
                           OR = c(OR_admitted_n[1], OR_admitted_y[1]), 
                           lower_ci_95 = c(OR_admitted_n[2], OR_admitted_y[2]),
                           upper_ci_95 = c(OR_admitted_n[3], OR_admitted_y[3]))
strat_OR_admitted$Group <- "Hospital Admission in Past Year (Y/N)"
pooled_OR_admitted <- mantelhaen.test(vascat$vita, vascat$currbf, vascat$admitted, conf.level = 0.95)
strat_OR_admitted$pooled_OR <- pooled_OR_admitted$estimate

# STRAT ANALYSIS ANAEMIA #

vascat_anaemia_n <-vascat|>
  filter(anaemia == "No") %>%
  select(c(vita, currbf, anaemia))

vascat_anaemia_y <-vascat|>
  filter(anaemia == "Yes") %>%
  select(c(vita, currbf, anaemia))

OR_anaemia_n <- DescTools::OddsRatio(vascat_anaemia_n$vita, vascat_anaemia_n$currbf, conf.level = 0.95)
OR_anaemia_y <- DescTools::OddsRatio(vascat_anaemia_y$vita, vascat_anaemia_y$currbf, conf.level = 0.95)

strat_OR_anaemia <- data.frame(category = c("No", "Yes"), 
                                OR = c(OR_anaemia_n[1], OR_anaemia_y[1]), 
                                lower_ci_95 = c(OR_anaemia_n[2], OR_anaemia_y[2]),
                                upper_ci_95 = c(OR_anaemia_n[3], OR_anaemia_y[3]))
strat_OR_anaemia$Group <- "Severe Anaemia (Y/N)"
pooled_OR_anaemia <- mantelhaen.test(vascat$vita, vascat$currbf, vascat$anaemia, conf.level = 0.95)
strat_OR_anaemia$pooled_OR <- pooled_OR_anaemia$estimate

# combining all strat_OR data frames

strat_OR_all <- bind_rows(strat_OR_admitted, strat_OR_age, strat_OR_anaemia, 
                          strat_OR_bcg, strat_OR_handpump, strat_OR_measles, 
                          strat_OR_motheduc, strat_OR_sex) %>%
  select(Group, category, OR, lower_ci_95, upper_ci_95, pooled_OR)

# export this table of stratified analysis of ORs

setwd("~/Desktop/steph-r-lshtm/STEPH-output")
png("strat_OR_all.png", width = 1200, height = 1000, res = 120)
grid.newpage()
grid.text("Stratified Odds Ratios of Potential Confounders of Not Breastfeeding vs Vitamin A Deficiency", 
          y = 0.88, gp = gpar(fontsize = 14, fontface = "bold"))
grid.table(strat_OR_all, vp = viewport(y = 0.48, height = 0.7))
dev.off()

# further Woolf testing to see if odds ratios between strata are truly disctinct

woolf <-xtabs( ~ vita + currbf + admitted, data = vascat)
WoolfTest(woolf)

woolf <-xtabs( ~ vita + currbf + agegp, data = vascat)
WoolfTest(woolf)

woolf <-xtabs( ~ vita + currbf + anaemia, data = vascat)
WoolfTest(woolf)

woolf <-xtabs( ~ vita + currbf + bcgscar, data = vascat)
WoolfTest(woolf)

woolf <-xtabs( ~ vita + currbf + handpump, data = vascat)
WoolfTest(woolf)

woolf <-xtabs( ~ vita + currbf + measles, data = vascat)
WoolfTest(woolf)

woolf <-xtabs( ~ vita + currbf + motheduc, data = vascat)
WoolfTest(woolf)

woolf <-xtabs( ~ vita + currbf + sex, data = vascat)
WoolfTest(woolf)


citation() 







