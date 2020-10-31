##### SCRIPT INFO #####

# This script cleans, organizes, and then analyzes response time (RT)
# data via multilevel models.

# Author: Ruth A. Shaffer
# For outlier analysis, modification of code from Violet Brown

# Analysis:

# As a manipulation check, response times (RTs) on the final recognition 
# test were examined to ensure that the expected patterns were exhibited 
# regarding the effect of age group (younger adults < older adults), 
# response type (Remember < Know), and initial learning condition (test < no test).

# For each of the models below, the analysis began with a full model including
# the fixed effects of interest and all random effects of subject and stimulus 
# (i.e., random intercepts and random slopes). Random effects that did not explain
# significant variance were removed. In addition, random effects that were perfectly 
# correlated with other random effects in the model were removed. Fixed effects 
# analysis was then conducted using the random effects structure from this step.

# Model comparisons with Likelihood-Ratio Chi-Squared Tests were used to estimate 
# the significance of fixed and random effects. To test the significance of a given
# fixed effect with model comparison, a model including the effect of interest and 
# all effects of the same and lower order was compared to a model excluding the 
# effect of interest but including all effects of the same and lower order.

##### SET WD TO FILE PATH ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### LOAD PACKAGES ####
library(tidyverse)
library(lme4)
library(car)
library(lmerTest)
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

##### CLEAN DATA ####

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
as.character(unique(SESSION2_exluded$subjectID)) # does not have "o053" because "o053" excluded because subject was not able to attend session 2 (and so does not appear in the session 2 data)

##### VARIABLES: CLEANED DATA ####
SESSION1
SESSION2
INCLUDE #subjects included in analyses

##################### OUTLIERS #######################

##### SESSION 2 REMOVE TRIALS RT < 250ms #####
SESSION2 = subset(SESSION2, experimentCondition == "finalTest_session2" & rt >= 250)

##### SESSION 2 PRIOR CONDITION #####
SESSION2 = 
  SESSION2 %>%
  mutate(prior_condition_combo = prior_condition) %>%
  mutate(prior_condition_combo = factor(prior_condition_combo)) %>%
  mutate(prior_condition_combo = ifelse(prior_condition_combo=="New","new",
                                        ifelse(prior_condition_combo == "Old_noTest1" | prior_condition_combo == "Old_noTest2", "notest",
                                               ifelse(prior_condition_combo == "Old_Test1" | prior_condition_combo == "Old_Test2", "test",
                                                      as.character(prior_condition_combo)))))

# check relabling
table(SESSION2$prior_condition,SESSION2$prior_condition_combo)

##### SESSION 2 RT OUTLIER SUBJECTS #####

##### REMOVE INDIVIDUAL TRIALS FOR SUBJECTS WHEN >3SDs FROM SUBJECT MEAN FOR THE GIVEN CONDITION/RESPONSE TYPE #####

# Below is modified from outlier RT code from Violet Brown:

# first, remove 3SD above/below outliers for a specific condition for subjects
mean_sd_RT_sub_per_condition = 
  SESSION2 %>%
  group_by(subjectID,GROUP,DELAY,prior_condition_combo,formatted_response_confidence_RKN) %>%
  summarise(SUBJECT_MEAN = mean(rt,na.rm=TRUE),
            SUBJECT_SD = sd(rt,na.rm=TRUE))

# join with overall data frame
SESSION2_temp = NA
SESSION2_temp = left_join(SESSION2, mean_sd_RT_sub_per_condition, by = c("subjectID","GROUP","DELAY","prior_condition_combo","formatted_response_confidence_RKN"))

# set min/max per sub/trial type
SESSION2_temp$min = SESSION2_temp$SUBJECT_MEAN - (3*SESSION2_temp$SUBJECT_SD)
SESSION2_temp$max = SESSION2_temp$SUBJECT_MEAN + (3*SESSION2_temp$SUBJECT_SD)

# flag
SESSION2_temp$flag = ifelse((SESSION2_temp$rt < SESSION2_temp$min) | (SESSION2_temp$rt > SESSION2_temp$max), 1, 0)

# for each participant, add up the number of flagged trials
rt_flag = 
  SESSION2_temp%>%
  group_by(subjectID) %>%
  summarise(num_flagged_trials=sum(flag))

# info on subjects with removed trials
rt_exclude = subset(rt_flag, rt_flag$num_flagged_trials != 0) 

# remove the flagged trials from the dataframe
SESSION2_filter = subset(SESSION2_temp,flag != 1)

##### REMOVE SUBJECTS WHO'S MEAN RT (AFTER REMOVING THEIR INDIVIDUAL OUTLIERS ABOVE) #####
##### IS > 3 SD FROM GRAND MEAN RT FOR PARTICULAR GROUP/CONDITION #####

# below is modified from outlier RT code from Violet Brown:

mean_RT_sub_per_condition = 
  SESSION2_filter %>%
  group_by(subjectID,GROUP,DELAY,prior_condition_combo,formatted_response_confidence_RKN) %>%
  summarise(SUBJECT_MEAN = mean(rt,na.rm=TRUE))

mean_sd_RT_group_per_condition = 
  mean_RT_sub_per_condition %>%
  group_by(GROUP,DELAY,prior_condition_combo,formatted_response_confidence_RKN) %>%
  summarise(GROUP_MEAN = mean(SUBJECT_MEAN,na.rm=TRUE),
            GROUP_SD = sd(SUBJECT_MEAN,na.rm=TRUE))

mean_sd_RT_group_per_condition$min = mean_sd_RT_group_per_condition$GROUP_MEAN - (3*mean_sd_RT_group_per_condition$GROUP_SD)
mean_sd_RT_group_per_condition$max = mean_sd_RT_group_per_condition$GROUP_MEAN + (3*mean_sd_RT_group_per_condition$GROUP_SD)

# join the dataframes so each row has the mean RT for the condition, the mean RT for a given participant, and the min and max RTs.
combo = left_join(mean_RT_sub_per_condition, mean_sd_RT_group_per_condition, by = c("GROUP","DELAY","prior_condition_combo","formatted_response_confidence_RKN"))

# add a column that flags conditions where the RT is less than the minimum or greater than the maximum
combo$flag = ifelse((combo$SUBJECT_MEAN < combo$min) | (combo$SUBJECT_MEAN > combo$max), 1, 0)

# flags per participant
rt_flag = NA
rt_flag = 
  combo %>%
  group_by(subjectID) %>%
  summarize(num_flags=sum(flag))

# list of all subject IDs that need to be excluded for having a mean in a condition that is > or < 3SD from the group mean for the corresponding condition
rt_exclude = NA
rt_exclude = subset(rt_flag, rt_flag$num_flags != 0)

# remove the subjects
SESSION2_RT_FINAL = subset(SESSION2_filter, !(subjectID %in% rt_exclude$subjectID))

# refactor
SESSION2_RT_FINAL$subjectID = factor(SESSION2_RT_FINAL$subjectID)
SESSION2_RT_FINAL$GROUP = factor(SESSION2_RT_FINAL$GROUP)
SESSION2_RT_FINAL$formatted_response_confidence_RKN = factor(SESSION2_RT_FINAL$formatted_response_confidence_RKN)
SESSION2_RT_FINAL$correct_coded = factor(SESSION2_RT_FINAL$correct_coded)

SESSION2_RT_FINAL$GROUP = factor(SESSION2_RT_FINAL$GROUP, levels = c("younger","older"))
SESSION2_RT_FINAL$formatted_response_confidence_RKN = factor(SESSION2_RT_FINAL$formatted_response_confidence_RKN, levels = c("N","R","K"))
SESSION2_RT_FINAL$correct_coded = factor(SESSION2_RT_FINAL$correct_coded, levels = c("hit","CR","FA","miss"))


##################### MODEL COMPARISONS #######################
##### MLM #####

##### OPTIMIZERS #####
cl_a = lmerControl(optimizer = "bobyqa",
                    optCtrl=list(maxfun=1e9))
cl_b = lmerControl(optimizer = "Nelder_Mead",
                    optCtrl=list(maxfun=1e9))
cl_c = lmerControl(optimizer = "optimx",
                    optCtrl=list(method="nlminb",maxiter=1e9))
cl_d = lmerControl(optimizer = "nloptwrap",
                    optCtrl=list(maxfun=1e9))
cl_a_calcderiv = lmerControl(optimizer = "bobyqa",
                             optCtrl=list(maxfun=1e9),
                             calc.derivs = FALSE)

##### Older vs. younger X hit,CR,FA,miss #####

# descriptives (means)
SESSION2_RT_FINAL %>%
  group_by(correct_coded,GROUP) %>%
  summarise(mean = mean(rt,na.rm=TRUE))

# full model
mod1 = lmer(rt ~ GROUP*correct_coded + (1 + correct_coded | subjectID) + (1 + correct_coded | formatted_stimulus), data = SESSION2_RT_FINAL, control = cl_a_calcderiv)
summary(mod1)
Anova(mod1)

# testing random effects in following 4 models
mod2 = lmer(rt ~ GROUP*correct_coded + (1 + correct_coded | subjectID) + (1 | formatted_stimulus), data = SESSION2_RT_FINAL, control = cl_a_calcderiv)
summary(mod2)
Anova(mod2)
r.slope.stim.code = anova(mod1,mod2)
r.slope.stim.code

mod3 = lmer(rt ~ GROUP*correct_coded + (1 | subjectID) + (1 + correct_coded | formatted_stimulus), data = SESSION2_RT_FINAL, control = cl_a_calcderiv)
summary(mod3)
Anova(mod3)
r.slope.sub.code = anova(mod1,mod3)
r.slope.sub.code

mod4 = lmer(rt ~ GROUP*correct_coded + (1 + correct_coded | subjectID), data = SESSION2_RT_FINAL, control = cl_a_calcderiv)
summary(mod4)
Anova(mod4)
r.int.stim.code = anova(mod2,mod4)
r.int.stim.code

mod5 = lmer(rt ~ GROUP*correct_coded + (1 + correct_coded | formatted_stimulus), data = SESSION2_RT_FINAL, control = cl_a_calcderiv)
summary(mod5)
Anova(mod5)
r.int.sub.code = anova(mod3,mod5)
r.int.sub.code

# testing fixed effects
# main model includes all random effects
summary(mod1)
Anova(mod1)

# interaction
mod6 = lmer(rt ~ GROUP + correct_coded + (1 + correct_coded | subjectID) + (1 + correct_coded | formatted_stimulus), data = SESSION2_RT_FINAL, control = cl_a_calcderiv)
summary(mod6)
Anova(mod6)
f.interact.group.code = anova(mod1,mod6)
f.interact.group.code

# get regression coefficients for effect of age group in hit, CR, FA, miss (recode so reference group is each one)
hit.recode =
  SESSION2_RT_FINAL %>%
  mutate(correct_coded = factor(correct_coded,levels = c("hit","CR","FA","miss")))
mod1.hit = lmer(rt ~ GROUP*correct_coded + (1 + correct_coded | subjectID) + (1 + correct_coded | formatted_stimulus), data = hit.recode, control = cl_a_calcderiv)
summary(mod1.hit)
Anova(mod1.hit)

CR.recode =
  SESSION2_RT_FINAL %>%
  mutate(correct_coded = factor(correct_coded,levels = c("CR","FA","miss","hit")))
mod1.CR = lmer(rt ~ GROUP*correct_coded + (1 + correct_coded | subjectID) + (1 + correct_coded | formatted_stimulus), data = CR.recode, control = cl_a_calcderiv)
summary(mod1.CR)
Anova(mod1.CR)

FA.recode =
  SESSION2_RT_FINAL %>%
  mutate(correct_coded = factor(correct_coded,levels = c("FA","miss","hit","CR")))
mod1.FA = lmer(rt ~ GROUP*correct_coded + (1 + correct_coded | subjectID) + (1 + correct_coded | formatted_stimulus), data = FA.recode, control = cl_a_calcderiv)
summary(mod1.FA)
Anova(mod1.FA)

miss.recode =
  SESSION2_RT_FINAL %>%
  mutate(correct_coded = factor(correct_coded,levels = c("miss","hit","CR","FA")))
mod1.miss = lmer(rt ~ GROUP*correct_coded + (1 + correct_coded | subjectID) + (1 + correct_coded | formatted_stimulus), data = miss.recode, control = cl_a_calcderiv)
summary(mod1.miss)
Anova(mod1.miss)


##### Remember hit vs. Know hit #####

# select Remember and Know hits only
data = subset(SESSION2_RT_FINAL,correct_coded == "hit" & (formatted_response_confidence_RKN == "R" | formatted_response_confidence_RKN == "K"))
data$formatted_response_confidence_RKN = factor(data$formatted_response_confidence_RKN, levels = c("K","R"))

# descriptives (means)
data %>%
  group_by(formatted_response_confidence_RKN,GROUP) %>%
  summarise(mean = mean(rt,na.rm=TRUE))

# full model
mod1a = lmer(rt ~ GROUP*formatted_response_confidence_RKN + (1 + formatted_response_confidence_RKN | subjectID) + (1 + formatted_response_confidence_RKN | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod1a)
Anova(mod1a)

# testing random effects in following 4 models
mod2a = lmer(rt ~ GROUP*formatted_response_confidence_RKN + (1 + formatted_response_confidence_RKN | subjectID) + (1 | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod2a)
Anova(mod2a)
r.slope.stim.RK = anova(mod1a,mod2a)
r.slope.stim.RK

mod3a = lmer(rt ~ GROUP*formatted_response_confidence_RKN + (1 | subjectID) + (1 + formatted_response_confidence_RKN | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod3a)
Anova(mod3a)
r.slope.sub.RK = anova(mod1a,mod3a)
r.slope.sub.RK

mod4a = lmer(rt ~ GROUP*formatted_response_confidence_RKN + (1 + formatted_response_confidence_RKN | subjectID), data = data, control = cl_a_calcderiv)
summary(mod4a)
Anova(mod4a)
r.int.stim.RK = anova(mod2a,mod4a)
r.int.stim.RK

mod5a = lmer(rt ~ GROUP*formatted_response_confidence_RKN + (1 + formatted_response_confidence_RKN | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod5a)
Anova(mod5a)
r.int.sub.RK = anova(mod3a,mod5a)
r.int.sub.RK

# testing fixed effects
# work with mod1a
summary(mod1a)
Anova(mod1a)

# interaction
mod6a = lmer(rt ~ GROUP + formatted_response_confidence_RKN + (1 + formatted_response_confidence_RKN | subjectID) + (1 + formatted_response_confidence_RKN | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod6a)
Anova(mod6a)
f.interact.group.RK = anova(mod1a,mod6a)
f.interact.group.RK

# main effects in following 2 models
mod7a = lmer(rt ~ GROUP + (1 + formatted_response_confidence_RKN | subjectID) + (1 + formatted_response_confidence_RKN | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod7a)
Anova(mod7a)
f.main.RK.RK = anova(mod6a,mod7a)
f.main.RK.RK

mod8a = lmer(rt ~ formatted_response_confidence_RKN + (1 + formatted_response_confidence_RKN | subjectID) + (1 + formatted_response_confidence_RKN | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod8a)
Anova(mod8a)
f.main.group.RK = anova(mod6a,mod8a)
f.main.group.RK

# most parsimonious model
summary(mod6a)

##### Test hit vs. no test hit #####

# select hits and FAs  only
data = subset(SESSION2_RT_FINAL,correct_coded == "hit")
data$prior_condition_combo = factor(data$prior_condition_combo)
data$prior_condition_combo = factor(data$prior_condition_combo, levels = c("notest","test"))

# descriptives (means)
data %>%
  group_by(prior_condition_combo,GROUP) %>%
  summarise(mean = mean(rt,na.rm=TRUE))

# full model
mod1b = lmer(rt ~ GROUP*prior_condition_combo + (1 + prior_condition_combo | subjectID) + (1 + prior_condition_combo | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod1b)
Anova(mod1b)

# testing random effects in following 4 models
mod2b = lmer(rt ~ GROUP*prior_condition_combo + (1 + prior_condition_combo | subjectID) + (1 | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod2b)
Anova(mod2b)
r.slope.stim.prior = anova(mod1b,mod2b)
r.slope.stim.prior

mod3b = lmer(rt ~ GROUP*prior_condition_combo + (1 | subjectID) + (1 + prior_condition_combo | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod3b)
Anova(mod3b)
r.slope.sub.prior = anova(mod1b,mod3b)
r.slope.sub.prior

mod4b = lmer(rt ~ GROUP*prior_condition_combo + (1 + prior_condition_combo | subjectID), data = data, control = cl_a_calcderiv)
summary(mod4b)
Anova(mod4b)
r.int.stim.prior = anova(mod2b,mod4b)
r.int.stim.prior

mod5b = lmer(rt ~ GROUP*prior_condition_combo + (1 + prior_condition_combo | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod5b)
Anova(mod5b)
r.int.sub.prior = anova(mod3b,mod5b)
r.int.sub.prior

# random effects used for models below
mod6b = lmer(rt ~ GROUP*prior_condition_combo + (1 | subjectID) + (1 | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod6b)
Anova(mod6b)
r.slope.sub.stim.prior = anova(mod1b,mod6b)
r.slope.sub.stim.prior

# testing fixed effects

# mod6b as comparison
summary(mod6b)
Anova(mod6b)

# interaction
mod7b = lmer(rt ~ GROUP + prior_condition_combo + (1 | subjectID) + (1 | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod7b)
Anova(mod7b)
f.interact.group.prior = anova(mod6b,mod7b)
f.interact.group.prior

# main effects in following 2 models
mod8b = lmer(rt ~ GROUP + (1 | subjectID) + (1 | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod8b)
Anova(mod8b)
f.main.prior.prior = anova(mod7b,mod8b)
f.main.prior.prior

mod9b = lmer(rt ~ prior_condition_combo + (1 | subjectID) + (1 | formatted_stimulus), data = data, control = cl_a_calcderiv)
summary(mod9b)
Anova(mod9b)
f.main.group.prior = anova(mod7b,mod9b)
f.main.group.prior

# most parsimonious model
summary(mod7b)

