##### SCRIPT INFO #####

# This script cleans the session 1 and session 2 experiment data
# and then conducts multilevel logistic regressions to analyze
# session 2 accuracy.

# Author: Ruth A. Shaffer

# Analysis: 

# Multilevel logistic regression: 
# Effect of age group, initial learning condition, and delay on
# the probability of a hit on the final recognition test

# The analysis began with a full model including all fixed effects 
# of interest (initial learning condition, age group, and delay) and 
# all random effects of subject and stimulus (random intercepts and random slopes). 
# Random effects that did not explain significant variance were removed. In addition, 
# random effects that were perfectly correlated with other random effects in the
# model were removed. Fixed effects analysis was then conducted using the random 
# effects structure from this step.

# Model comparisons with Likelihood-Ratio Chi-Squared Tests were used to estimate 
# the significance of fixed and random effects.  To test the significance of a given 
# fixed effect with model comparison, a model including the effect of interest and
# all effects of the same and lower order was compared to a model excluding the 
# effect of interest but including all effects of the same and lower order. 

##### SET WD TO FILE PATH ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### LOAD PACKAGES ####
library(lme4)
library(car)
library(lsmeans)
library(merTools)
library(tidyverse)
library("readxl")

##################### CLEAN DATA ##################### 

##### IMPORT DATA ####

##### import session 1 data ####
SESSION1 = as.data.frame(read_excel("../../DATA/SESSION1/COMBINED_S1_LENIENT_SCORING.xlsx"))

##### import session 2 data ####
SESSION2_OA_0D_TEMP=as.data.frame(read.csv("../../DATA/SESSION2/OA_0D_S2.csv",header=TRUE,sep=","))
SESSION2_OA_1D_TEMP=as.data.frame(read.csv("../../DATA/SESSION2/OA_1D_S2.csv",header=TRUE,sep=","))
SESSION2_YA_0D_TEMP=as.data.frame(read.csv("../../DATA/SESSION2/YA_0D_S2.csv",header=TRUE,sep=","))
SESSION2_YA_1D_TEMP=as.data.frame(read.csv("../../DATA/SESSION2/YA_1D_S2.csv",header=TRUE,sep=","))

##### add group labels to session 2 data ####

# OA_0D
OA_0D_GROUP = as.data.frame(cbind(rep("older",length(SESSION2_OA_0D_TEMP$group)),rep("delay0",length(SESSION2_OA_0D_TEMP$group))))
colnames(OA_0D_GROUP) = c("GROUP","DELAY")
SESSION2_OA_0D = cbind(OA_0D_GROUP,SESSION2_OA_0D_TEMP)

# OA_1D
OA_1D_GROUP = as.data.frame(cbind(rep("older",length(SESSION2_OA_1D_TEMP$group)),rep("delay1",length(SESSION2_OA_1D_TEMP$group))))
colnames(OA_1D_GROUP) = c("GROUP","DELAY")
SESSION2_OA_1D = cbind(OA_1D_GROUP,SESSION2_OA_1D_TEMP)

# YA_0D
YA_0D_GROUP = as.data.frame(cbind(rep("younger",length(SESSION2_YA_0D_TEMP$group)),rep("delay0",length(SESSION2_YA_0D_TEMP$group))))
colnames(YA_0D_GROUP) = c("GROUP","DELAY")
SESSION2_YA_0D = cbind(YA_0D_GROUP,SESSION2_YA_0D_TEMP)

# YA_1D
YA_1D_GROUP = as.data.frame(cbind(rep("younger",length(SESSION2_YA_1D_TEMP$group)),rep("delay1",length(SESSION2_YA_1D_TEMP$group))))
colnames(YA_1D_GROUP) = c("GROUP","DELAY")
SESSION2_YA_1D = cbind(YA_1D_GROUP,SESSION2_YA_1D_TEMP)

##### combine session 2 data ####

SESSION2 = rbind(SESSION2_OA_0D,SESSION2_OA_1D,SESSION2_YA_0D,SESSION2_YA_1D)

##### CLEAN / ORGANIZE DATA ####

##### import subject info sheets ####
SUB_INFO_OLDER = as.data.frame(read_excel("../../DATA/SUBJECT_INFO/OLDER_ADULT_SUB_INFO.xlsx"))
SUB_INFO_YOUNGER = as.data.frame(read_excel("../../DATA/SUBJECT_INFO/YOUNGER_ADULT_SUB_INFO.xlsx"))

##### remove trials from false start for 2 subjects ####

# o005 false start (restarted after 2 trials as o005b for entire task; remove the 2 trials from o005 false start from session 1 and 2 for o005b)
stimuli_to_remove_o005 = SESSION1[SESSION1$subjectID=="o005" & SESSION1$experimentCondition == "study_list1",]$stimulus # the trials seen in false start
SESSION1 = SESSION1[!(SESSION1$subjectID == "o005b" & SESSION1$experimentCondition == "study_list1" & SESSION1$stimulus %in% stimuli_to_remove_o005), ] # remove the 2 trials from o005b study
SESSION1 = SESSION1[!(SESSION1$subjectID == "o005b" & SESSION1$experimentCondition == "CRtest_list1" & SESSION1$full_word %in% c("RAISED","GUEST")), ] # remove the 2 trials from o005b test
SESSION2 = SESSION2[!(SESSION2$subjectID == "o005b" & SESSION2$formatted_stimulus %in% c("RAISED","GUEST")), ] # remove the 2 trials from o005b final test
SESSION1 = SESSION1[!(SESSION1$subjectID == "o005"),] # remove false start 

# y032 false start. 1st word only - see subject info file.
stimulus_to_remove_y032 = SESSION1[SESSION1$subjectID=="y032" & SESSION1$experimentCondition == "study_list1",]$stimulus[1] # the trials seen in false start
SESSION1 = SESSION1[!(SESSION1$subjectID == "y032b" & SESSION1$experimentCondition == "study_list1" & SESSION1$stimulus %in% stimulus_to_remove_y032), ] # remove the trial from y032b study
SESSION1 = SESSION1[!(SESSION1$subjectID == "y032b" & SESSION1$experimentCondition == "CRtest_list1" & SESSION1$full_word %in% c("MERE")), ] # remove the trial from y032b test
SESSION2 = SESSION2[!(SESSION2$subjectID == "y032b" & SESSION2$formatted_stimulus %in% c("MERE")), ] # remove the trial from y032b final test
SESSION1 = SESSION1[!(SESSION1$subjectID == "y032"),] # remove false start 

##### rename 1 subject session 2 ####

# remove 2 non-experiment trials session 2 o019 (welcome and begin trials)
SESSION2 = SESSION2[!(SESSION2$subjectID == "o019"),]

SESSION2[SESSION2$subjectID == "o019b",]$subjectID = "o019"

##### remove subjects that do not meet inclusion criteria ####

INCLUDE = c(SUB_INFO_OLDER[SUB_INFO_OLDER$INCLUDE_EXCLUDE==1,]$SUBID,SUB_INFO_YOUNGER[SUB_INFO_YOUNGER$INCLUDE_EXCLUDE==1,]$SUBID) # IDs to include
EXCLUDE = c(SUB_INFO_OLDER[SUB_INFO_OLDER$INCLUDE_EXCLUDE==0,]$SUBID,SUB_INFO_YOUNGER[SUB_INFO_YOUNGER$INCLUDE_EXCLUDE==0,]$SUBID) # IDs to exclude

# hold session 1 and 2 info in temp variable for use later
SESSION1_TEMP = SESSION1
SESSION2_TEMP = SESSION2

# included subjects only
SESSION1 = SESSION1[SESSION1$subjectID %in% INCLUDE,]
SESSION2 = SESSION2[SESSION2$subjectID %in% INCLUDE,]

# match between list of included subjects and subjects in data after inclusion criterion added?
as.character(unique(SESSION1$subjectID)) == INCLUDE
as.character(unique(SESSION2$subjectID)) == INCLUDE

# excluded subjects 
SESSION1_exluded = SESSION1_TEMP[!(SESSION1_TEMP$subjectID %in% INCLUDE),] # all subjects in the session 1 data that are not in the included list
SESSION2_exluded = SESSION2_TEMP[!(SESSION2_TEMP$subjectID %in% INCLUDE),] # all subjects in the session 2 data that are not in the included list

# match between excluded subjects in list and session 1 data subjects not in the included list?
EXCLUDE
as.character(unique(SESSION1_exluded$subjectID))
as.character(unique(SESSION2_exluded$subjectID)) # does not have "o053" because "o053" excluded as subject was not able to attend session 2 (and so does not appear in the session 2 data)


##### VARIABLES: CLEANED DATA ####
SESSION1
SESSION2
INCLUDE #subjects included in analyses

##################### ITEM-LEVEL ORGANIZATION ##################### 

# final test data, rt >= 250ms
SESSION2_FILTERED = subset(SESSION2, experimentCondition == "finalTest_session2" & rt >= 250)

# subset and factor variables
SESSION2_FILTERED = 
  SESSION2_FILTERED %>%
  subset(prior_condition %in% c("Old_noTest1","Old_noTest2","Old_Test1","Old_Test2")) %>%
  mutate(prior_condition_combo = ifelse(prior_condition %in% c("Old_noTest1","Old_noTest2"),"NOTEST", ifelse(prior_condition %in% c("Old_Test1","Old_Test2"),"TEST",NA))) %>%
  mutate(
    subjectID = factor(subjectID),
    experimentCondition = factor(experimentCondition),
    prior_condition_combo = factor(prior_condition_combo),
    formatted_stimulus = factor(formatted_stimulus),
    GROUP = factor(GROUP,levels=c("younger","older")),
    correct_coded = factor(correct_coded)) %>%
  mutate(correct_binary = ifelse(correct_coded == "hit",1,ifelse(correct_coded=="miss",0,NA)))

##################### ITEM LEVEL ANALYSIS ##################### 

##### OPTIMIZERS #####
cl1 = glmerControl(optimizer = "bobyqa",calc.derivs=FALSE,
                    optCtrl=list(maxfun=1e9),
                    check.conv.grad = .makeCC("warning", tol = 1e-3, relTol = NULL),
                    check.conv.singular = .makeCC(action = "message",  tol = 1e-9),
                    check.conv.hess = .makeCC(action = "warning", tol = 1e-6))
cl2 = glmerControl(optimizer = "Nelder_Mead",calc.derivs=FALSE,
                    optCtrl=list(maxfun=1e9),
                    check.conv.grad = .makeCC("warning", tol = 1e-3, relTol = NULL),
                    check.conv.singular = .makeCC(action = "message",  tol = 1e-9),
                    check.conv.hess = .makeCC(action = "warning", tol = 1e-6))

optimizer3 = glmerControl(optCtrl=list(maxfun=2e4), optimizer="bobyqa")

##### CHECK LEVELS #####
levels(SESSION2_FILTERED$subjectID)
levels(SESSION2_FILTERED$formatted_stimulus)
levels(SESSION2_FILTERED$prior_condition_combo)
levels(SESSION2_FILTERED$GROUP)
levels(SESSION2_FILTERED$DELAY)

##### MODEL COMPARISONS #####

# full model
mod1 = glmer(correct_binary ~ 
                prior_condition_combo*GROUP*DELAY +
                (1 + prior_condition_combo | subjectID) + (1 + prior_condition_combo | formatted_stimulus), 
              data=SESSION2_FILTERED,
              family = binomial, 
              control=optimizer3)
Anova(mod1)
summary(mod1)

# test random effects in the following 4 models
mod2 = glmer(correct_binary ~ 
                prior_condition_combo*GROUP*DELAY +
                (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), 
              data=SESSION2_FILTERED,
              family = binomial, 
              control=optimizer3)
Anova(mod2)
summary(mod2)
anova(mod1,mod2)

mod3 = glmer(correct_binary ~ 
                prior_condition_combo*GROUP*DELAY +
                (1 | subjectID) + (1 + prior_condition_combo | formatted_stimulus), 
              data=SESSION2_FILTERED,
              family = binomial, 
              control=optimizer3)
Anova(mod3)
summary(mod3)
anova(mod1,mod3)

mod4 = glmer(correct_binary ~ 
                prior_condition_combo*GROUP*DELAY +
                (1 + prior_condition_combo | subjectID), 
              data=SESSION2_FILTERED,
              family = binomial, 
              control=optimizer3)
Anova(mod4)
summary(mod4)
anova(mod2,mod4)

mod5 = glmer(correct_binary ~ 
                prior_condition_combo*GROUP*DELAY +
                (1 + prior_condition_combo | formatted_stimulus), 
              data=SESSION2_FILTERED,
              family = binomial, 
              control=optimizer3)
Anova(mod5)
summary(mod5)
anova(mod3,mod5)

# use mod2 random effects structure to test fixed effects
Anova(mod2)
summary(mod2)

# test for 3 way interaction
mod2A = glmer(correct_binary ~ 
                prior_condition_combo + GROUP + DELAY +
                prior_condition_combo:GROUP + prior_condition_combo:DELAY + GROUP:DELAY + 
                (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), 
              data=SESSION2_FILTERED,
              family = binomial, 
              control=optimizer3)
Anova(mod2A)
summary(mod2A)
anova(mod2,mod2A)

# test for the significance of each 2-way interaction in the following 3 models
mod2B = glmer(correct_binary ~ 
                 prior_condition_combo + GROUP + DELAY +
                 prior_condition_combo:GROUP + prior_condition_combo:DELAY +
                 (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), 
               data=SESSION2_FILTERED,
               family = binomial, 
               control=optimizer3)
Anova(mod2B)
summary(mod2B)
anova(mod2A,mod2B)

mod2C = glmer(correct_binary ~ 
                 prior_condition_combo + GROUP + DELAY +
                 prior_condition_combo:GROUP + GROUP:DELAY + 
                 (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), 
               data=SESSION2_FILTERED,
               family = binomial, 
               control=optimizer3)
Anova(mod2C)
summary(mod2C)
anova(mod2A,mod2C)

mod2D = glmer(correct_binary ~ 
                 prior_condition_combo + GROUP + DELAY +
                 prior_condition_combo:DELAY + GROUP:DELAY + 
                 (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), 
               data=SESSION2_FILTERED,
               family = binomial, 
               control=optimizer3)
Anova(mod2D)
summary(mod2D)
anova(mod2A,mod2D)

# followup on trend
mod2.trend = glmer(correct_binary ~ 
                 prior_condition_combo + GROUP +
                 prior_condition_combo:GROUP + 
                 (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), 
               data=SESSION2_FILTERED,
               family = binomial, 
               control=optimizer3)
summary(mod2.trend)
fixef(mod2.trend)

# probability hit younger adult no test 
young_notest = fixef(mod2.trend)["(Intercept)"]
round(exp(young_notest) / (1+exp(young_notest)),2)

# probability hit younger test
young_test = fixef(mod2.trend)["(Intercept)"] + fixef(mod2.trend)["prior_condition_comboTEST"]
round(exp(young_test) / (1+exp(young_test)),2)

# probability hit older adult no test 
old_notest = fixef(mod2.trend)["(Intercept)"] + fixef(mod2.trend)["GROUPolder"]
round(exp(old_notest) / (1+exp(old_notest)),2)

# probability hit older test
old_test = fixef(mod2.trend)["(Intercept)"] + fixef(mod2.trend)["GROUPolder"] + fixef(mod2.trend)["prior_condition_comboTEST"] + fixef(mod2.trend)["prior_condition_comboTEST:GROUPolder"]
round(exp(old_test) / (1+exp(old_test)),2)

# obtain means
SESSION2_FILTERED %>%
  group_by(GROUP,prior_condition_combo) %>%
  summarise(mean = round(mean(correct_binary,na.rm=TRUE),2))

# test for the significance of the three 2-way interactions, combined
mod2E = glmer(correct_binary ~ 
                 prior_condition_combo + GROUP + DELAY +
                 (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), 
               data=SESSION2_FILTERED,
               family = binomial, 
               control=optimizer3)
Anova(mod2E)
summary(mod2E)
anova(mod2A,mod2E)

# test for main effects in the following three models
mod2F = glmer(correct_binary ~ 
                 prior_condition_combo + GROUP +
                 (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), 
               data=SESSION2_FILTERED,
               family = binomial, 
               control=optimizer3)
Anova(mod2F)
summary(mod2F)
anova(mod2E,mod2F)

mod2G = glmer(correct_binary ~ 
                 prior_condition_combo + DELAY +
                 (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), 
               data=SESSION2_FILTERED,
               family = binomial, 
               control=optimizer3)
Anova(mod2G)
summary(mod2G)
anova(mod2E,mod2G)

mod2H = glmer(correct_binary ~ 
                 GROUP + DELAY +
                 (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), 
               data=SESSION2_FILTERED,
               family = binomial, 
               control=optimizer3)
Anova(mod2H)
summary(mod2H)
anova(mod2E,mod2H)

# final parsimonious model
summary(mod2G)
Anova(mod2G)

