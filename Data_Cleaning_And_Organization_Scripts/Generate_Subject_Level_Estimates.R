##### SCRIPT INFO #####

# This script cleans the session 1 and session 2 data
# and then generates subject-level measures of session 1 and 2 performance.
# Author: Ruth A. Shaffer

##### SET WD TO FILE PATH ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### LOAD PACKAGES ####
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

##### CORGANIZE / LEAN DATA ####

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

##################### SUBJECT-LEVEL MEASURES ##################### 

##### MEASURES: SESSION 1 ####
##### construct an empty matrix to fill in results ####

SUBJECT_LEVEL_S1 = data.frame(matrix(ncol = 13, nrow = 0))
x = c("subid",
       "group",
       "delay",
       "condition",
       "S1_PROP_CORRECT",
       "S1_PROP_INCORRECT_BLANK",
       "S1_PROP_INCORRECT_RESPONSE",
       "S1_TEST1_PROP_CORRECT",
       "S1_TEST1_PROP_INCORRECT_BLANK",
       "S1_TEST1_PROP_INCORRECT_RESPONSE",
       "S1_TEST2_PROP_CORRECT",
       "S1_TEST2_PROP_INCORRECT_BLANK",
       "S1_TEST2_PROP_INCORRECT_RESPONSE") # column names

colnames(SUBJECT_LEVEL_S1) = x # set column names

##### get all unique subject ids ####
SESSION1_SUBJECTS = as.character(INCLUDE)

##### subject level measures summary ####

for (current_sub in SESSION1_SUBJECTS) {
  
  # start variable at NA
  subject_data = NA
  GROUP = NA
  DELAY = NA
  CONDITION = NA

  # get individual subject's rows
  subject_data = subset(SESSION1, subjectID == current_sub)
  
  # get group
  GROUP = subject_data$GROUP[1]
  
  # get delay
  DELAY = subject_data$DELAY[1]
  
  # get condition number
  CONDITION = subject_data$conditionNumber[1]
  
  # initialize variables
  proportion_correct = c()
  proportion_incorrect_blank = c()
  proportion_incorrect_response = c()
  
  # set to NA
  total_session1 = NA
  proportion_correct_session1 = NA
  proportion_incorrect_blank_session1 = NA
  proportion_incorrect_response_session1 = NA
  
  # get proportion correct, incorrect blank, and incorrect response overall in Session 1
  total_session1 = nrow(subset(subject_data, (experimentCondition == "CRtest_list1" | experimentCondition == "CRtest_list2")))
  proportion_correct_session1 = nrow(subset(subject_data, (experimentCondition == "CRtest_list1" | experimentCondition == "CRtest_list2") & correct == 1)) / total_session1
  proportion_incorrect_blank_session1 = nrow(subset(subject_data, (experimentCondition == "CRtest_list1" | experimentCondition == "CRtest_list2") & correct == -1)) / total_session1
  proportion_incorrect_response_session1 = nrow(subset(subject_data, (experimentCondition == "CRtest_list1" | experimentCondition == "CRtest_list2") & correct == 0)) / total_session1
  
  # add information
  proportion_correct = c(proportion_correct,proportion_correct_session1)
  proportion_incorrect_blank = c(proportion_incorrect_blank,proportion_incorrect_blank_session1)
  proportion_incorrect_response = c(proportion_incorrect_response,proportion_incorrect_response_session1)

  # get names of columns for stimuli (names of conditions)
  test_condition_names = c("CRtest_list1",
                           "CRtest_list2")
  
  # add separate session 1 test 1 and 2 information
  for (i in test_condition_names) {
    
    # set to NA
    total_current_condition = NA
    proportion_correct_current = NA
    proportion_incorrect_blank_current = NA
    proportion_incorrect_response_current = NA
    
    # get proportion correct, incorrect blank, and incorrect response for the current condition
    total_current_condition = nrow(subset(subject_data, experimentCondition == i))
    proportion_correct_current = nrow(subset(subject_data, experimentCondition == i & correct == 1)) / total_current_condition
    proportion_incorrect_blank_current = nrow(subset(subject_data, experimentCondition == i & correct == -1)) / total_current_condition
    proportion_incorrect_response_current = nrow(subset(subject_data, experimentCondition == i & correct == 0)) / total_current_condition
    
    # add information
    proportion_correct = c(proportion_correct,proportion_correct_current)
    proportion_incorrect_blank = c(proportion_incorrect_blank,proportion_incorrect_blank_current)
    proportion_incorrect_response = c(proportion_incorrect_response,proportion_incorrect_response_current)
    
  }
  
  # create a data frame with the subject's information
  new_subject_row = NA
  new_subject_row = data.frame(current_sub, 
                               GROUP,
                               DELAY,
                               CONDITION, 
                               proportion_correct[1],
                               proportion_incorrect_blank[1],
                               proportion_incorrect_response[1],
                               proportion_correct[2],
                               proportion_incorrect_blank[2],
                               proportion_incorrect_response[2],
                               proportion_correct[3],
                               proportion_incorrect_blank[3],
                               proportion_incorrect_response[3])
  
  colnames(new_subject_row) = x # set column names
  
  # add subject to SUBJECT_LEVEL_S1 data frame
  SUBJECT_LEVEL_S1 = rbind(SUBJECT_LEVEL_S1, new_subject_row)
  
}

##### MEASURES: SESSION 2 ####
##### construct an empty matrix to fill in results ####

SUBJECT_LEVEL_S2 = data.frame(matrix(ncol = 33, nrow = 0))
x = c("subid",
       "TRIALS_UNDER_250MS",
       "S2_DPRIME",
       "S2_PROP_HIT",
       "S2_PROP_MISS",
       "S2_PROP_CR",
       "S2_PROP_FA",
       "S2_PROP_HIT_R",
       "S2_PROP_HIT_K",
       "S2_PROP_FA_R",
       "S2_PROP_FA_K",
       "S2_REC_WITH_FA",
       "S2_FAM_WITH_FA",
       "S2_REC_NO_FA",
       "S2_FAM_NO_FA",
       "S2_DPRIME_TEST",
       "S2_PROP_HIT_TEST",
       "S2_PROP_MISS_TEST",
       "S2_PROP_HIT_R_TEST",
       "S2_PROP_HIT_K_TEST",
       "S2_REC_WITH_FA_TEST",
       "S2_FAM_WITH_FA_TEST",
       "S2_REC_NO_FA_TEST",
       "S2_FAM_NO_FA_TEST",
       "S2_DPRIME_NOTEST",
       "S2_PROP_HIT_NOTEST",
       "S2_PROP_MISS_NOTEST",
       "S2_PROP_HIT_R_NOTEST",
       "S2_PROP_HIT_K_NOTEST",
       "S2_REC_WITH_FA_NOTEST",
       "S2_FAM_WITH_FA_NOTEST",
       "S2_REC_NO_FA_NOTEST",
       "S2_FAM_NO_FA_NOTEST") # column names

colnames(SUBJECT_LEVEL_S2) = x # set column names

##### get all unique subject ids ####
SESSION2_SUBJECTS = as.character(INCLUDE)

##### subject level measures summary ####

for (current_sub in SESSION2_SUBJECTS) {
  
  # start variable at NA
  subject_data = NA
  
  TRIALS_UNDER_250MS = NA
  
  total_session2_old = NA
  total_session2_new = NA
  
  S2_DPRIME = NA
  
  S2_PROP_HIT = NA
  S2_PROP_MISS = NA
  S2_PROP_CR = NA
  S2_PROP_FA = NA
  
  S2_PROP_HIT_R = NA
  S2_PROP_HIT_K = NA
  S2_PROP_FA_R = NA
  S2_PROP_FA_K = NA
  S2_REC_WITH_FA = NA
  S2_FAM_WITH_FA = NA
  S2_REC_NO_FA = NA
  S2_FAM_NO_FA = NA

  # get individual subject's rows, removing trials with RT < 250ms
  subject_data = subset(SESSION2, subjectID == current_sub & experimentCondition == "finalTest_session2" & rt >= 250)
  
  # get number of trials removed for the subject, due to < 250 ms RTs
  TRIALS_UNDER_250MS = nrow(subset(SESSION2, subjectID == current_sub & experimentCondition == "finalTest_session2" & rt < 250))
  
  # get number of old and new items
  total_session2_old = nrow(subset(subject_data, prior_condition != "New")) # number of old items
  total_session2_new = nrow(subset(subject_data, prior_condition == "New")) # number of new items
  
  # get proportion hit, miss, CR, FA overall for session 2
  S2_PROP_HIT = nrow(subset(subject_data, correct_coded == "hit")) / total_session2_old
  S2_PROP_MISS = nrow(subset(subject_data, correct_coded == "miss")) / total_session2_old
  S2_PROP_CR = nrow(subset(subject_data, correct_coded == "CR")) / total_session2_new
  S2_PROP_FA = nrow(subset(subject_data, correct_coded == "FA")) / total_session2_new

  # get overall dprime for session 2
  # Standard Correct for hit rate of 1 or FA rate of 0
  S2_PROP_HIT_FOR_DPRIME = S2_PROP_HIT
  S2_PROP_FA_FOR_DPRIME = S2_PROP_FA
  
  if (S2_PROP_HIT_FOR_DPRIME == 1) {
    S2_PROP_HIT_FOR_DPRIME = 1 - (1/(2*total_session2_old)) # 1- (1/(2*number of targets))
  }
  if (S2_PROP_FA_FOR_DPRIME == 0) {
    S2_PROP_FA_FOR_DPRIME = 1/(2*total_session2_new) # 1/(2*number of lures)
  }
  
  # calculate dprime
  S2_DPRIME = qnorm(S2_PROP_HIT_FOR_DPRIME) - qnorm(S2_PROP_FA_FOR_DPRIME)
  
  # get proportion hit R and K and FA R and K
  S2_PROP_HIT_R = nrow(subset(subject_data, correct_coded == "hit" & formatted_response_confidence_RKN == "R")) / total_session2_old
  S2_PROP_HIT_K = nrow(subset(subject_data, correct_coded == "hit" & formatted_response_confidence_RKN == "K")) / total_session2_old

  S2_PROP_FA_R = nrow(subset(subject_data, correct_coded == "FA" & formatted_response_confidence_RKN == "R")) / total_session2_new
  S2_PROP_FA_K = nrow(subset(subject_data, correct_coded == "FA" & formatted_response_confidence_RKN == "K")) / total_session2_new
  
  # get rec/fam estimates with and without FAs
  S2_REC_WITH_FA = S2_PROP_HIT_R - S2_PROP_FA_R
  S2_FAM_WITH_FA = (S2_PROP_HIT_K / (1 - S2_PROP_HIT_R)) - (S2_PROP_FA_K / (1 - S2_PROP_FA_R))
  
  S2_REC_NO_FA = S2_PROP_HIT_R
  S2_FAM_NO_FA = S2_PROP_HIT_K / (1 - S2_PROP_HIT_R)
  
  ### Get values for the test condition ###
  
  # start variable at NA
  subject_data_test = NA
  total_session2_old_test = NA
  
  S2_DPRIME_TEST = NA
  
  S2_PROP_HIT_TEST = NA
  S2_PROP_MISS_TEST = NA
  
  S2_PROP_HIT_R_TEST = NA
  S2_PROP_HIT_K_TEST = NA
  S2_REC_WITH_FA_TEST = NA
  S2_FAM_WITH_FA_TEST = NA
  S2_REC_NO_FA_TEST = NA
  S2_FAM_NO_FA_TEST = NA
  
  # select prior test data
  subject_data_test = subset(subject_data, prior_condition == "Old_Test1" | prior_condition == "Old_Test2")
  
  # get number of old test items
  total_session2_old_test = nrow(subject_data_test)
  
  # get proportion hits and misses for tested items
  S2_PROP_HIT_TEST = nrow(subset(subject_data_test, correct_coded == "hit")) / total_session2_old_test
  S2_PROP_MISS_TEST = nrow(subset(subject_data_test, correct_coded == "miss")) / total_session2_old_test
  
  # get proportion of hit R and K for tested items
  S2_PROP_HIT_R_TEST = nrow(subset(subject_data_test, correct_coded == "hit" & formatted_response_confidence_RKN == "R")) / total_session2_old_test
  S2_PROP_HIT_K_TEST = nrow(subset(subject_data_test, correct_coded == "hit" & formatted_response_confidence_RKN == "K")) / total_session2_old_test
  
  # get rec and fam estimates with and without FAs for tested items
  S2_REC_WITH_FA_TEST = S2_PROP_HIT_R_TEST - S2_PROP_FA_R
  S2_FAM_WITH_FA_TEST = (S2_PROP_HIT_K_TEST / (1 - S2_PROP_HIT_R_TEST)) - (S2_PROP_FA_K / (1 - S2_PROP_FA_R))
 
  S2_REC_NO_FA_TEST = S2_PROP_HIT_R_TEST
  S2_FAM_NO_FA_TEST = S2_PROP_HIT_K_TEST / (1 - S2_PROP_HIT_R_TEST)
  
  # get dprime in test condition
  S2_PROP_HIT_TEST_FOR_DPRIME = S2_PROP_HIT_TEST
  S2_PROP_FA_FOR_DPRIME = S2_PROP_FA
  
  # Standard Correct for hit rate of 1 or FA rate of 0
  if (S2_PROP_HIT_TEST_FOR_DPRIME == 1) {
    S2_PROP_HIT_TEST_FOR_DPRIME = 1 - (1/(2*total_session2_old_test)) # 1- (1/(2*number of targets in test condition))
  }
  if (S2_PROP_FA_FOR_DPRIME == 0) {
    S2_PROP_FA_FOR_DPRIME = 1/(2*total_session2_new) # 1/(2*number of lures)
  }
  
  # calculate dprime in the test condition
  S2_DPRIME_TEST = qnorm(S2_PROP_HIT_TEST_FOR_DPRIME) - qnorm(S2_PROP_FA_FOR_DPRIME)
  
  ### Get values for the no test condition ###
  
  # start variable at NA
  subject_data_notest = NA
  total_session2_old_notest = NA
  
  S2_DPRIME_NOTEST = NA
  
  S2_PROP_HIT_NOTEST = NA
  S2_PROP_MISS_NOTEST = NA
  
  S2_PROP_HIT_R_NOTEST = NA
  S2_PROP_HIT_K_NOTEST = NA
  S2_REC_WITH_FA_NOTEST = NA
  S2_FAM_WITH_FA_NOTEST = NA
  S2_REC_NO_FA_NOTEST = NA
  S2_FAM_NO_FA_NOTEST = NA
  
  # select prior no test data
  subject_data_notest = subset(subject_data, prior_condition == "Old_noTest1" | prior_condition == "Old_noTest2")
  
  # get number of old no test items
  total_session2_old_notest = nrow(subject_data_notest)
  
  # get proportion hits and misses for non-tested items
  S2_PROP_HIT_NOTEST = nrow(subset(subject_data_notest, correct_coded == "hit")) / total_session2_old_notest
  S2_PROP_MISS_NOTEST = nrow(subset(subject_data_notest, correct_coded == "miss")) / total_session2_old_notest
  
  # get proportion of hit R and K for non-tested items
  S2_PROP_HIT_R_NOTEST = nrow(subset(subject_data_notest, correct_coded == "hit" & formatted_response_confidence_RKN == "R")) / total_session2_old_notest
  S2_PROP_HIT_K_NOTEST = nrow(subset(subject_data_notest, correct_coded == "hit" & formatted_response_confidence_RKN == "K")) / total_session2_old_notest
  
  # get rec and fam estimates with and without FAs for non-tested items
  S2_REC_WITH_FA_NOTEST = S2_PROP_HIT_R_NOTEST - S2_PROP_FA_R
  S2_FAM_WITH_FA_NOTEST = (S2_PROP_HIT_K_NOTEST / (1 - S2_PROP_HIT_R_NOTEST)) - (S2_PROP_FA_K / (1 - S2_PROP_FA_R))
  
  S2_REC_NO_FA_NOTEST = S2_PROP_HIT_R_NOTEST
  S2_FAM_NO_FA_NOTEST = S2_PROP_HIT_K_NOTEST / (1 - S2_PROP_HIT_R_NOTEST)
  
  # get dprime in no test condition
  S2_PROP_HIT_NOTEST_FOR_DPRIME = S2_PROP_HIT_NOTEST
  S2_PROP_FA_FOR_DPRIME = S2_PROP_FA
  
  # Standard Correct for hit rate of 1 or FA rate of 0
  if (S2_PROP_HIT_NOTEST_FOR_DPRIME == 1) {
    S2_PROP_HIT_NOTEST_FOR_DPRIME = 1 - (1/(2*total_session2_old_notest)) # 1- (1/(2*number of targets in no test condition))
  }
  if (S2_PROP_FA_FOR_DPRIME == 0) {
    S2_PROP_FA_FOR_DPRIME = 1/(2*total_session2_new) # 1/(2*number of lures)
  }
  
  # calculate dprime in the no test condition
  S2_DPRIME_NOTEST = qnorm(S2_PROP_HIT_NOTEST_FOR_DPRIME) - qnorm(S2_PROP_FA_FOR_DPRIME)
  
  ### create a data frame with the subject's information ###
  new_subject_row = NA
  new_subject_row = data.frame(current_sub,
                               TRIALS_UNDER_250MS,
                               S2_DPRIME,
                               S2_PROP_HIT,
                               S2_PROP_MISS,
                               S2_PROP_CR,
                               S2_PROP_FA,
                               S2_PROP_HIT_R,
                               S2_PROP_HIT_K,
                               S2_PROP_FA_R,
                               S2_PROP_FA_K,
                               S2_REC_WITH_FA,
                               S2_FAM_WITH_FA,
                               S2_REC_NO_FA,
                               S2_FAM_NO_FA,
                               S2_DPRIME_TEST,
                               S2_PROP_HIT_TEST,
                               S2_PROP_MISS_TEST,
                               S2_PROP_HIT_R_TEST,
                               S2_PROP_HIT_K_TEST,
                               S2_REC_WITH_FA_TEST,
                               S2_FAM_WITH_FA_TEST,
                               S2_REC_NO_FA_TEST,
                               S2_FAM_NO_FA_TEST,
                               S2_DPRIME_NOTEST,
                               S2_PROP_HIT_NOTEST,
                               S2_PROP_MISS_NOTEST,
                               S2_PROP_HIT_R_NOTEST,
                               S2_PROP_HIT_K_NOTEST,
                               S2_REC_WITH_FA_NOTEST,
                               S2_FAM_WITH_FA_NOTEST,
                               S2_REC_NO_FA_NOTEST,
                               S2_FAM_NO_FA_NOTEST)
  
  colnames(new_subject_row) = x # set column names
  
  # add subject to SUBJECT_LEVEL_S1 data frame
  SUBJECT_LEVEL_S2 = rbind(SUBJECT_LEVEL_S2, new_subject_row)
  
}


##### MERGE SUBJECT LEVEL ESTIMATES SESSION 1 AND 2 ####

SUBJECT_LEVEL_TOTAL = merge(SUBJECT_LEVEL_S1,SUBJECT_LEVEL_S2,by="subid")

##### SAVE TO FILE ####

write.csv(SUBJECT_LEVEL_TOTAL, file = "../../R_OUTPUT/SUBJECT_LEVEL_ESTIMATES.csv")
