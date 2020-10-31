##### SCRIPT INFO #####

# This script cleans the session 1 and session 2 data
# and then performs extensive data checks.

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

##### ORGANIZE / CLEAN DATA ####

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


##################### CHECK DATA ##################### 

##### IMPORT WORD LISTS ####

##### import word lists session 1 ####
STIMULI_SESSION1_CONDITION1=read.csv("../../DATA/STIMULI/STIMULI_SESSION1_CONDITION1.csv",header=TRUE,sep=",")
STIMULI_SESSION1_CONDITION2=read.csv("../../DATA/STIMULI/STIMULI_SESSION1_CONDITION2.csv",header=TRUE,sep=",")
STIMULI_SESSION1_CONDITION3=read.csv("../../DATA/STIMULI/STIMULI_SESSION1_CONDITION3.csv",header=TRUE,sep=",")
STIMULI_SESSION1_CONDITION4=read.csv("../../DATA/STIMULI/STIMULI_SESSION1_CONDITION4.csv",header=TRUE,sep=",")

# combine into a single indexable data.frame
STIMULI_SESSION1_ALL = list(STIMULI_SESSION1_CONDITION1,
                            STIMULI_SESSION1_CONDITION2,
                            STIMULI_SESSION1_CONDITION3,
                            STIMULI_SESSION1_CONDITION4)

##### import word lists session 2 ####
STIMULI_SESSION2_CONDITION1=read.csv("../../DATA/STIMULI/STIMULI_SESSION2_CONDITION1.csv",header=TRUE,sep=",")
STIMULI_SESSION2_CONDITION2=read.csv("../../DATA/STIMULI/STIMULI_SESSION2_CONDITION2.csv",header=TRUE,sep=",")
STIMULI_SESSION2_CONDITION3=read.csv("../../DATA/STIMULI/STIMULI_SESSION2_CONDITION3.csv",header=TRUE,sep=",")
STIMULI_SESSION2_CONDITION4=read.csv("../../DATA/STIMULI/STIMULI_SESSION2_CONDITION4.csv",header=TRUE,sep=",")

# combine into a single indexable data.frame
STIMULI_SESSION2_ALL = list(STIMULI_SESSION2_CONDITION1,
                            STIMULI_SESSION2_CONDITION2,
                            STIMULI_SESSION2_CONDITION3,
                            STIMULI_SESSION2_CONDITION4)


##### CHECK: SESSION 1 ####
##### construct an empty matrix to fill in results of checks ####

CHECK_RESULTS_S1 = data.frame(matrix(ncol = 13, nrow = 0))
x = c("subid",
       "group",
       "delay",
       "condition",
       "correct_number_of_stimuli_per_list",
       "count_trials_study_list1",
       "count_trials_CRtest_list1",
       "count_trials_study_list2",
       "count_trials_CRtest_list2",
       "correct_stimuli_study_list1",
       "correct_stimuli_CRtest_list1",
       "correct_stimuli_study_list2",
       "correct_stimuli_CRtest_list2") # column names

colnames(CHECK_RESULTS_S1) = x # set column names

##### get all unique subject ids ####
SESSION1_SUBJECTS = as.character(INCLUDE)

##### get subject info from subject info sheet for comparison ####

# included subject information from subject info sheet
OLDER_TEMP = SUB_INFO_OLDER[,c("SUBID","DELAY","CONDITION")]
OLDER_TEMP = cbind(OLDER_TEMP,c(rep("older",length(OLDER_TEMP$SUBID))))
colnames(OLDER_TEMP) = c("SUBID","DELAY","CONDITION","GROUP")

YOUNGER_TEMP = SUB_INFO_YOUNGER[,c("SUBID","DELAY","CONDITION")]
YOUNGER_TEMP = cbind(YOUNGER_TEMP,c(rep("younger",length(YOUNGER_TEMP$SUBID))))
colnames(YOUNGER_TEMP) = c("SUBID","DELAY","CONDITION","GROUP")          

SUB_INFO_CURRENT = rbind(OLDER_TEMP,YOUNGER_TEMP) # combine

# change labels to match data file (0 becomes delay0; 1 becomes delay 1)
SUB_INFO_CURRENT[SUB_INFO_CURRENT$DELAY==0,]$DELAY = "delay0"
SUB_INFO_CURRENT[SUB_INFO_CURRENT$DELAY==1,]$DELAY = "delay1"

##### check stimuli and subject info ####

for (current_sub in SESSION1_SUBJECTS) {
  
  # start variable at NA
  subject_data = NA
  info_sheet_current_sub = NA
  GROUP = NA
  DELAY = NA
  CONDITION = NA
  current_stimuli = NA
  
  # get individual subject's rows
  subject_data = subset(SESSION1, subjectID == current_sub)
  
  # get subject info from subject info sheet
  info_sheet_current_sub = SUB_INFO_CURRENT[SUB_INFO_CURRENT$SUBID==current_sub,]
  
  # get group
  if (all(subject_data$GROUP == subject_data$GROUP[1])) {
    # make sure this matches the subject info sheet
    if (subject_data$GROUP[1] == info_sheet_current_sub$GROUP) {
      GROUP = subject_data$GROUP[1]
    } else {
      GROUP = "ERROR"
    }
  } else {
    GROUP = "ERROR"
  }
  
  # get delay
  if (all(subject_data$DELAY == subject_data$DELAY[1])) {
    # make sure this matches the subject info sheet
    if (subject_data$DELAY[1] == info_sheet_current_sub$DELAY) {
      DELAY = subject_data$DELAY[1]
    } else {
      DELAY = "ERROR"
    }
  } else {
    DELAY = "ERROR"
  }
  
  # get condition number
  if (all(subject_data$conditionNumber == subject_data$conditionNumber[1])) {
    # make sure this matches the subject info sheet
    if (subject_data$conditionNumber[1] == info_sheet_current_sub$CONDITION) {
      CONDITION = subject_data$conditionNumber[1]
    } else {
      CONDITION = "ERROR"
    }
  } else {
    CONDITION = "ERROR"
  }
  
  # check that all of the correct stimuli are in each study and test list from session 1
  # this is based on the condition #
  # so get the correct data frame of stimuli from the list of data frames (organized by condition)
  current_stimuli = as.data.frame(STIMULI_SESSION1_ALL[[CONDITION]]) # index into the correct set of stimuli
  
  # get names of columns for stimuli (names of conditions)
  condition_names = c("study_list1",
                      "CRtest_list1",
                      "study_list2",
                      "CRtest_list2")
  
  correct_amount_overall = 1 # initialize to 1
  count_stimuli_condition = c() # initialize vector to hold information about # of stimuli in the study and test sections
  correct_stimuli_condition = c() # initialize vector to hold information about whether correct stimuli for study and test sections
  for (i in condition_names) {
    
    # start variables as NA
    amount_subject = NA
    cs_temp = NA
    current_stimuli_words = NA
    amount_stimuli = NA
    
    ## check that the overall number of items in this condition is correct ##
    # get the number of rows for this subject with this condition name
    amount_subject = nrow(subset(subject_data, experimentCondition == i))
    
    # get non empty elements in study or test list
    cs_temp = current_stimuli[[i]]
    current_stimuli_words = cs_temp[cs_temp != ""]
    
    # get the number of non-blank elements in study or test list
    amount_stimuli = length(current_stimuli_words)
    
    # if it is incorrect, set checker to 0
    if (amount_subject != amount_stimuli) {
      correct_amount_overall = 0
    }
    
    ## check that the stimuli in this condition are correct ##
    correct_stimuli_condition_current = 0 # initialize at 0
    for (j in current_stimuli_words) {
      
      # start variable as NA
      stimulus_count = NA
      
      # check if it's a study or test trial (will change which column you look for full word)
      if (i == "study_list1" | i == "study_list2") {
        # check that see correct word in correct condition (count should be 1 always)
        stimulus_count = nrow(subset(subject_data, experimentCondition == i & stimulus == paste("<p>",toupper(j),"</p>",sep="")))
      } else if (i == "CRtest_list1" | i == "CRtest_list2") {
        # check that see correct word in correct condition (count should be 1 always)
        stimulus_count = nrow(subset(subject_data, experimentCondition == i & full_word == toupper(j)))
      }
      # if there isn't 1 row, it's incorrect
      if (stimulus_count == 1){
        correct_stimuli_condition_current = correct_stimuli_condition_current+1
      }
      
    }
    
    # add information
    count_stimuli_condition = c(count_stimuli_condition,amount_subject)
    correct_stimuli_condition = c(correct_stimuli_condition,correct_stimuli_condition_current)
  }
  
  # create a data frame with the subject's information
  new_subject_row = NA
  new_subject_row = data.frame(current_sub, 
                               as.character(GROUP),
                               as.character(DELAY),
                               as.character(CONDITION),
                               correct_amount_overall,
                               count_stimuli_condition[1],
                               count_stimuli_condition[2],
                               count_stimuli_condition[3],
                               count_stimuli_condition[4],
                               correct_stimuli_condition[1],
                               correct_stimuli_condition[2],
                               correct_stimuli_condition[3],
                               correct_stimuli_condition[4])
  
  colnames(new_subject_row) = x # set column names
  
  # add subject to CHECK_RESULTS_S1 data frame
  CHECK_RESULTS_S1 = rbind(CHECK_RESULTS_S1, new_subject_row)
  
}

#View(CHECK_RESULTS_S1)


##### CHECK: SESSION 2 ####
##### get all unique subject ids ####
SESSION2_SUBJECTS = as.character(INCLUDE)

##### construct an empty matrix to fill in results of checks ####

CHECK_RESULTS_S2 = data.frame(matrix(ncol = 16, nrow = 0))
x = c("subid",
       "group",
       "delay",
       "condition",
       "correct_number_of_stimuli",
       "count_trials_New",
       "count_trials_OldTest1",
       "count_trials_OldTest2",
       "count_trials_Old_noTest1",
       "count_trials_Old_noTest2",
       "correct_stimuli_New",
       "correct_stimuli_OldTest1",
       "correct_stimuli_OldTest2",
       "correct_stimuli_Old_noTest1",
       "correct_stimuli_Old_noTest2",
       "scoring_correct") # column names

colnames(CHECK_RESULTS_S2) = x # set column names

##### get subject info from subject info sheet for comparison ####

# included subject information from subject info sheet
OLDER_TEMP = SUB_INFO_OLDER[,c("SUBID","DELAY","CONDITION")]
OLDER_TEMP = cbind(OLDER_TEMP,c(rep("older",length(OLDER_TEMP$SUBID))))
colnames(OLDER_TEMP) = c("SUBID","DELAY","CONDITION","GROUP")

YOUNGER_TEMP = SUB_INFO_YOUNGER[,c("SUBID","DELAY","CONDITION")]
YOUNGER_TEMP = cbind(YOUNGER_TEMP,c(rep("younger",length(YOUNGER_TEMP$SUBID))))
colnames(YOUNGER_TEMP) = c("SUBID","DELAY","CONDITION","GROUP")          

SUB_INFO_CURRENT = rbind(OLDER_TEMP,YOUNGER_TEMP) # combine

# change labels to match data file (0 becomes delay0; 1 becomes delay 1)
SUB_INFO_CURRENT[SUB_INFO_CURRENT$DELAY==0,]$DELAY = "delay0"
SUB_INFO_CURRENT[SUB_INFO_CURRENT$DELAY==1,]$DELAY = "delay1"

##### check stimuli, condition, and scoring, and get measures of accuracy ####
for (current_sub in SESSION2_SUBJECTS) {
  
  # start variable at NA
  subject_data = NA
  info_sheet_current_sub = NA
  GROUP = NA
  DELAY = NA
  CONDITION = NA
  current_stimuli = NA
  
  # get individual subject's rows
  subject_data = subset(SESSION2, subjectID == current_sub)
  
  # get subject info from subject info sheet
  info_sheet_current_sub = SUB_INFO_CURRENT[SUB_INFO_CURRENT$SUBID==current_sub,]
  
  # get group
  if (all(subject_data$GROUP == subject_data$GROUP[1])) {
    # make sure this matches the subject info sheet
    if (subject_data$GROUP[1] == info_sheet_current_sub$GROUP) {
      GROUP = subject_data$GROUP[1]
    } else {
      GROUP = "ERROR"
    }
  } else {
    GROUP = "ERROR"
  }
  
  # get delay
  if (all(subject_data$DELAY == subject_data$DELAY[1])) {
    # make sure this matches the subject info sheet
    if (subject_data$DELAY[1] == info_sheet_current_sub$DELAY) {
      DELAY = subject_data$DELAY[1]
    } else {
      DELAY = "ERROR"
    }
  } else {
    DELAY = "ERROR"
  }
  
  # get condition number
  if (all(subject_data$conditionNumber == subject_data$conditionNumber[1])) {
    # make sure this matches the subject info sheet
    if (subject_data$conditionNumber[1] == info_sheet_current_sub$CONDITION) {
      CONDITION = subject_data$conditionNumber[1]
    } else {
      CONDITION = "ERROR"
    }
  } else {
    CONDITION = "ERROR"
  }
  
  # check that all of the correct stimuli are in each study and test list from session 2
  # this is based on the condition #
  # so get the correct data frame of stimuli from the list of data frames (organized by condition)
  current_stimuli = as.data.frame(STIMULI_SESSION2_ALL[[CONDITION]]) # index into the correct set of stimuli
  
  # get names of columns for stimuli (names of conditions)
  condition_names = c("New",
                      "Old_Test1",
                      "Old_Test2",
                      "Old_noTest1",
                      "Old_noTest2")
  
  correct_amount_overall = 1 # initialize to 1
  count_stimuli_condition = c() # initialize vector to hold information about # of stimuli in the test 
  correct_stimuli_condition = c() # initialize vector to hold information about whether correct stimuli for test
  correct_scoring_overall = 1 # initialize to 1
  
  for (i in condition_names) {
    
    # initialize variables to NA
    amount_subject = NA
    cs_temp = NA
    current_stimuli_words = NA
    amount_stimuli = NA
    
    ## check that the overall number of items in this condition is correct ##
    # get the number of rows for this subject with this condition name
    amount_subject = nrow(subset(subject_data, prior_condition == i))
    
    # get non empty elements in list
    cs_temp = current_stimuli[[i]]
    current_stimuli_words = cs_temp[cs_temp != ""]
    
    # get the number of non-blank elements in study or test list
    amount_stimuli = length(current_stimuli_words)
    
    # if it is incorrect, set checker to 0
    if (amount_subject != amount_stimuli) {
      correct_amount_overall = 0
    }
    
    correct_stimuli_current = 0
    ## check that the stimuli in this condition are correct ##
    for (j in current_stimuli_words) {
      
      # initialize variable to NA
      stimulus_count = NA
      
      # check that see correct word in correct condition (count should be 1 always)
      stimulus_count = nrow(subset(subject_data, prior_condition == i & formatted_stimulus == toupper(j)))
      
      # if there isn't 1 row, it's incorrect
      if (stimulus_count == 1){
        correct_stimuli_current = correct_stimuli_current + 1
      } 
      
    }
    
    # add information
    count_stimuli_condition = c(count_stimuli_condition,amount_subject)
    correct_stimuli_condition = c(correct_stimuli_condition,correct_stimuli_current)
    
    # check scoring
    current_condition_data = NA
    current_condition_data = subset(subject_data, prior_condition == i)
    
    for (k in 1:length(current_condition_data$GROUP)) {
      
      if (i == "Old_Test1" || i == "Old_Test2" || i == "Old_noTest1" || i == "Old_noTest2") { # old items
        
        if (current_condition_data[k,]$formatted_response_confidence_RKN == "R" || current_condition_data[k,]$formatted_response_confidence_RKN == "K") { # response was "R" or "K"
          if (current_condition_data[k,]$correct_coded != "hit") {
            correct_scoring_overall = 0
          }
        } else { #response was "N"
          if (current_condition_data[k,]$correct_coded != "miss") {
            correct_scoring_overall = 0
          }
        }
        
      } else { # new items
        
        if (current_condition_data[k,]$formatted_response_confidence_RKN == "R" || current_condition_data[k,]$formatted_response_confidence_RKN == "K") { # response was "R" or "K"
          if (current_condition_data[k,]$correct_coded != "FA") {
            correct_scoring_overall = 0
          }
        } else { #response was "N"
          if (current_condition_data[k,]$correct_coded != "CR") {
            correct_scoring_overall = 0
          }
        }
      }
    }
  }
  
  # create a data frame with the subject's information
  new_subject_row = NA
  new_subject_row = data.frame(current_sub, 
                               as.character(GROUP),
                               as.character(DELAY),
                               as.character(CONDITION), 
                               correct_amount_overall,
                               count_stimuli_condition[1],
                               count_stimuli_condition[2],
                               count_stimuli_condition[3],
                               count_stimuli_condition[4],
                               count_stimuli_condition[5],
                               correct_stimuli_condition[1],
                               correct_stimuli_condition[2],
                               correct_stimuli_condition[3],
                               correct_stimuli_condition[4],
                               correct_stimuli_condition[5],
                               correct_scoring_overall)
  
  colnames(new_subject_row) = x # set column names
  
  # add subject to CHECK_RESULTS_S2 data frame
  CHECK_RESULTS_S2 = rbind(CHECK_RESULTS_S2, new_subject_row)
  
}


##### SAVE TO FILES ####

write.csv(CHECK_RESULTS_S1, file = "../../R_OUTPUT/CHECK_SESSION1.csv")
write.csv(CHECK_RESULTS_S2, file = "../../R_OUTPUT/CHECK_SESSION2.csv")
