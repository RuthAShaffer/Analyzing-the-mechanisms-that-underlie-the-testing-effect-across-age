##### SCRIPT INFO #####

# This script cleans the session 1 and session 2 data
# and calculates reliability estimates for Dual-Process
# parameter estimates.
# Author: Ruth A. Shaffer

##### SET WD TO FILE PATH ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### LOAD PACKAGES ####
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
as.character(unique(SESSION2_exluded$subjectID)) # does not have "o053" because "o053" excluded because subject was not able to attend session 2 (and so does not appear in the session 2 data)

##### VARIABLES: CLEANED DATA ####
SESSION1
SESSION2
INCLUDE #subjects included in analyses

#### SPLIT HALF RELIABILITIES ####
# Necessary to use split-half reliabilities with the Spearman-Brown 
# Prophecy formula rather than reliabilities obtained from 
# mixed-effects analysis due to the use of global estimates of
# familiarity (rather than raw item-level raw Know responding).

# data
SESSION2_RELIABILITY_DATA = subset(SESSION2, experimentCondition == "finalTest_session2" & rt >= 250)
SESSION2_RELIABILITY_DATA$prior_condition = factor(SESSION2_RELIABILITY_DATA$prior_condition)

# for each subject, label half of old items as group 1 and group 2
temp1 =
SESSION2_RELIABILITY_DATA %>%
  group_by(subjectID,prior_condition) %>%
  mutate(id = row_number()) %>%
  mutate(reliability_grouping = ifelse((id %% 2) == 0, "group2","group1")) %>%
  ungroup()

# create prior condition summary variable
temp2 = 
  temp1 %>%
  mutate(prior_condition_summary = ifelse(prior_condition == "New","New", ifelse(prior_condition == "Old_noTest1" | prior_condition == "Old_noTest2", "NoTest", ifelse(prior_condition == "Old_Test1" | prior_condition == "Old_Test2", "Test", NA))))

# calculate each raw estimate of interest
temp3 = 
  temp2 %>%
  group_by(subjectID) %>%
  summarise(
    remember_FA_1 = sum(formatted_response_confidence_RKN=="R" & prior_condition_summary == "New" & reliability_grouping == "group1")/sum(prior_condition_summary == "New" & reliability_grouping == "group1"),
    remember_FA_2 = sum(formatted_response_confidence_RKN=="R" & prior_condition_summary == "New" & reliability_grouping == "group2")/sum(prior_condition_summary == "New" & reliability_grouping == "group2"),
    know_FA_1 = sum(formatted_response_confidence_RKN=="K" & prior_condition_summary == "New" & reliability_grouping == "group1")/sum(prior_condition_summary == "New" & reliability_grouping == "group1"),
    know_FA_2 = sum(formatted_response_confidence_RKN=="K" & prior_condition_summary == "New" & reliability_grouping == "group2")/sum(prior_condition_summary == "New" & reliability_grouping == "group2"),
    remember_Test_1 = sum(formatted_response_confidence_RKN=="R" & prior_condition_summary == "Test" & reliability_grouping == "group1")/sum(prior_condition_summary == "Test" & reliability_grouping == "group1"),
    remember_Test_2 = sum(formatted_response_confidence_RKN=="R" & prior_condition_summary == "Test" & reliability_grouping == "group2")/sum(prior_condition_summary == "Test" & reliability_grouping == "group2"),
    know_Test_1 = sum(formatted_response_confidence_RKN=="K" & prior_condition_summary == "Test" & reliability_grouping == "group1")/sum(prior_condition_summary == "Test" & reliability_grouping == "group1"),
    know_Test_2 = sum(formatted_response_confidence_RKN=="K" & prior_condition_summary == "Test" & reliability_grouping == "group2")/sum(prior_condition_summary == "Test" & reliability_grouping == "group2"),
    remember_noTest_1 = sum(formatted_response_confidence_RKN=="R" & prior_condition_summary == "NoTest" & reliability_grouping == "group1")/sum(prior_condition_summary == "NoTest" & reliability_grouping == "group1"),
    remember_noTest_2 = sum(formatted_response_confidence_RKN=="R" & prior_condition_summary == "NoTest" & reliability_grouping == "group2")/sum(prior_condition_summary == "NoTest" & reliability_grouping == "group2"),
    know_noTest_1 = sum(formatted_response_confidence_RKN=="K" & prior_condition_summary == "NoTest" & reliability_grouping == "group1")/sum(prior_condition_summary == "NoTest" & reliability_grouping == "group1"),
    know_noTest_2 = sum(formatted_response_confidence_RKN=="K" & prior_condition_summary == "NoTest" & reliability_grouping == "group2")/sum(prior_condition_summary == "NoTest" & reliability_grouping == "group2"),
  ) %>%
  ungroup()

# calculate parameter estimates and the testing effect
temp4 = 
  temp3 %>%
  mutate(
    Rec_Test_1 = remember_Test_1 - remember_FA_1,
    Rec_Test_2 = remember_Test_2 - remember_FA_2,
    
    Fam_Test_1 = (know_Test_1/ (1-remember_Test_1)) - (know_FA_1/ (1-remember_FA_1)),
    Fam_Test_2 = (know_Test_2/ (1-remember_Test_2)) - (know_FA_2/ (1-remember_FA_2)),
    
    Rec_noTest_1 = remember_noTest_1 - remember_FA_1,
    Rec_noTest_2 = remember_noTest_2 - remember_FA_2,
    
    Fam_noTest_1 = (know_noTest_1/ (1-remember_noTest_1)) - (know_FA_1/ (1-remember_FA_1)),
    Fam_noTest_2 = (know_noTest_2/ (1-remember_noTest_2)) - (know_FA_2/ (1-remember_FA_2))
  ) %>%
  mutate(
    TE_Remember_1 = remember_Test_1 - remember_noTest_1,
    TE_Remember_2 = remember_Test_2 - remember_noTest_2,
    TE_Know_1 = know_Test_1 - know_noTest_1,
    TE_Know_2 = know_Test_2 - know_noTest_2,
    
    TE_Rec_1 = Rec_Test_1 - Rec_noTest_1,
    TE_Rec_2 = Rec_Test_2 - Rec_noTest_2,
    TE_Fam_1 = Fam_Test_1 - Fam_noTest_1,
    TE_Fam_2 = Fam_Test_2 - Fam_noTest_2
  )

# calculate split-half reliabilities using the Spearman- Brown Prophecy formula for each variable
correlations_split_half = cor(temp4[,-1], method = c("pearson"))
correlations_split_half_SBP = (2 * correlations_split_half) / (1 + correlations_split_half)

#### output files ####
write.csv(correlations_split_half, "../../R_OUTPUT/RELIABILITY_RawCorrelations.csv")
write.csv(correlations_split_half_SBP, "../../R_OUTPUT/RELIABILITY_SpearmanBrownProphecyFormula.csv")

