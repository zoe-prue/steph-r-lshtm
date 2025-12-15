# Power and sample size practical
# 5/12/2025
# explore power and ssample size calculations

###################################
# library calls

install.packages("MESS")
library(MESS)

###################################
# global variables

###################################
# script

## COMPARING TWO PROPORTIONS ##

# can conduct most kinds of power calculation using the power.prop.test function 
# specify the power and significance level

power.prop.test(n=NULL, power=0.8, p1=0.1, p2=0.15, sig.level=0.05)

# putting in proportions the other way around

power.prop.test(n=NULL, p1=0.15, p2=0.1, power = 0.8, sig.level = 0.05)

# 686 participants needed

# get same n

# what if we changed the power to 0.90?
# Q1. Do you expect the number of participants needed will decrease or increase from the 1372 needed for 80% power?
# i expect the number of individuals would be higher, as n generally increases with power
# this is because you need more individuals to detect a clinically significant difference between two groups
# as you decrease the probability of a type II error (not rejecting the null hypothesis when a difference truly exists)

power.prop.test(n=NULL, power=0.9, p1=0.1, p2=0.15, sig.level=0.05)

# 918 participants needed - rounded up

# What sample sizes would be required to give 95% power?

power.prop.test(n=NULL, power=0.95, p1=0.1, p2=0.15, sig.level=0.05)

# 1135 participants

# What sample sizes would be required to give 85% power?

power.prop.test(n=NULL, power=0.85, p1=0.1, p2=0.15, sig.level=0.05)

# 785 participants

# 80% power, 0.01 significance rate

power.prop.test(n=NULL, power=0.8, p1=0.1, p2=0.15, sig.level=0.01)

# 1021 people needed

## COMPARING TWO PROPORTIONS: DIFFERENT GROUP SIZES ## 

# what if the arms of the trial have different numbers of participants?
# note that here, ratio = (second group size) / (first group size)

# let's say there's a ratio of 2:1 in the 2 arms of the trial

power_prop_test(n=NULL, power=0.9, p1=0.1, p2=0.15, sig.level=0.05, ratio=2)

# let's say a 1:2 ratio between the two arms of the trial:

power_prop_test(n=NULL, power=0.9, p1=0.1, p2=0.15, sig.level=0.05, ratio=0.5)

# the total sample size will be smaller when we have a greater proportion in the group with the lower expected rate
# e.g. greater proportion of trial participants in the 0.15 group when compared to the 0.1 group

## COMPARING 2 MEANS ##

# estimate sample sizes required to detect a difference between two means, 
# we need to use the power.t.tests function, 
# and specify the difference between the expected mean values in each group 
# and the standard deviation (SD) of each variable

# E.g. comparing birth weights from a group of boys born from HIV-positive mothers vs negative ones
# different in means between two groups: 2.8 kg in the first group and 3.0 kg in the second group -> delta = 0.2
# SD expected in each group: previous studies says that SD can be 0.4 kg (i.e. 400 g) in each group

# power = 90%, difference in means (delta) = 0.2 kg, SD = 0.4 kg

power.t.test(n = NULL, delta = 0.2, sd=0.4, power = 0.9, sig.level = 0.05)

# Q2. What would happen if you wanted to detect a difference of 0.1kg? 
# Would the required sample size go up or down? Can you estimate by how much?
# The smaller clinical difference (delta) should required more participants to power the study
# i.e. a smaller difference in means should increase n

power.t.test(n = NULL, delta = 0.1, sd=0.4, power = 0.9, sig.level = 0.05)

# i was right, in increases n

# Q3. If, when you collected your data, you noticed that the standard deviation in both groups was 0.2, 
# not 0.4 as you had estimated. What effect will this have on your power?
# SD^2 is in the numerator of the equation for n
# meaning that a decrease in the SD should decrease the n calculated

power.t.test(n = NULL, delta = 0.1, sd=0.2, power = 0.9, sig.level = 0.05)

# i does, i was correct

## COMPARING TWO MEANS - POWER AND RATIO OF GROUP SIZES SPECIFICED ##

# We can use the power_t_test function in the MESS package 
# to change the power and the ratio of the group sizes 
# (in the same way as we used power_prop_test for comparing two proportions)

power_t_test(n = NULL, delta = 0.2, sd=0.4, power = 0.8, sig.level = 0.05, ratio = 3)

## ESTIMATING POWER WHEN SAMPLE SIZES ARE KNWON ##

# sometimes we just want to work backwards!


# no but really funding can be tight and you might not be able to recruit as many ppl as wanted
# and you have to see how powered the study could be

# simple put the power argument as NULL

power.prop.test(n=600, p1=0.1, p2=0.15, power = NULL, sig.level = 0.05)

# power would only be about 75%
# 25% chance of a false negative - that your study would not find evidence for a difference 
# between the sample proportions even if there was such a difference between the groups

# what about with a larger clinical differnece (delta) to be observed?

power.prop.test(n=600, p1=0.1, p2=0.2, power = NULL, sig.level = 0.05)



# see that the power increases, because its easier to detect this difference, 
# and therefore the risk of a type II error is lower

# lets see a range of values for p2 ranging from 0.11 to 0.21, using seq() function

power.prop.test(n=600, p1=0.1, p2=seq(from=0.11, to=0.21, by=0.01), power = NULL, sig.level = 0.05)

# can do a similar power calculation for studies comparing two means (delta)

power.t.test(n=100, delta=0.2,sd=0.4, power=NULL,sig.level=0.05)

## EXTENDED EXERCISE ##

# two types of pain reflief offered to women in labor
# pethidine is standard, but ppl are worried about effectiveness and sedative effect on infant
# diamorphine (or heroin) to be tested instead as a new and different intervention
# two outcomes of interest: (i)	pain relief to labouring women and (ii)	the need for resuscitation of the baby

# in trial, 40% of women report poor pain relief with pethidine
# and 6% of babies need to be resuscitated on pethidine
# 25% proportionate reduction in either of these outcomes would be clinically significant

# Q4. Calculate (either by hand or using R) the size of study necessary 
# in order to have 80% statistical power to detect this reduction in the proportion of women 
# who perceive their pain relief to be poor using a 5% significance level (based on a two-sided test).
# so i need a reduction in 25%, meaning 40% to 30% thinking their pain relief is poor

power.prop.test(n=NULL, power=0.8, p1=0.4, p2=0.3, sig.level=0.05)

# answer: n = 365 participants

# Q5. Suppose someone suggests that even an absolute 5% reduction in pain relief
# would be important to detect (i.e. from 40% to 35%). 
# Assuming the power and significance are as in Q4, how many women need to be recruited?

power.prop.test(n=NULL, power=0.8, p1=0.4, p2=0.35, sig.level=0.05)

# answer: n = 1471 participants

# Q6. How many women would be required if the assumptions are as in (a) but with the power increased to 90%?

power.prop.test(n=NULL, power=0.9, p1=0.4, p2=0.3, sig.level=0.05)

# answer: n = 477

# Q7. How many women would be required if the assumptions are as in (a) but with the significance level set at 1%?

power.prop.test(n=NULL, power=0.9, p1=0.4, p2=0.3, sig.level=0.01)

# answer: 675 participants

# Q8. Calculate the size of study necessary in order to have 80% statistical power 
# to detect a 25% reduction in the proportion of babies needing resuscitation using a 5% significance level.
# remember that 6% of babies need resuscitation

power.prop.test(n=NULL, power=0.8, p1=0.06, p2=0.045, sig.level=0.05)

# answer: 3470 participants

# Q9. What advice would you offer to the clinicians contemplating setting up this trial? 
# i would advise them to account for loss to follow up or people dropping out of the trial.
# i would also encourage them to not make the trial too large as this can waste money and resources
# i would also encourage them to commit to a method of sampling that is appropriate for this trial and truly random
# consider the primary outcome: which one do we care about matching the power to?
# likely the resuscitation of babies because it is a more serious outcome
# meaning that this trial may be a multi-center trial

# Q10. What would your advice be if they said they only had the resources for 500 patients?

power.prop.test(n=600, p1=0.06, p2=0.045, power = NULL, sig.level = 0.05)

# the power with these conditions would be 0.21, meaning there is an 80% chance of a type II error
# this is extremely dangerous and futile
# i would encourage them to not pursue the trial 
# unless a larger sample size could be recruited
# this trial could only be acceptable if the clinical difference they were looking for became a lot bigger

  



  







