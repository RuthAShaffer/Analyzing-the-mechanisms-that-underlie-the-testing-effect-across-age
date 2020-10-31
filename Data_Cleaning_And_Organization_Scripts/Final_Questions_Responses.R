##### SCRIPT INFO #####

# This script cleans the session 1 and session 2 data
# and then adds subject-level final questions information.

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

##################### SUBJECT-LEVEL FINAL QUESTIONS ##################### 

##### RESPONSES: SESSION 1 AND SESSION 2 ####
##### construct an empty matrix to fill in results ####

SUBJECT_LEVEL_FINAL_QUESTIONS = data.frame(matrix(ncol = 8, nrow = 0))
x = c("subid",
       "group",
       "delay",
       "condition",
       "SESSION1_DIFFICULTY",
       "SESSION1_EFFORT",
       "SESSION2_DIFFICULTY",
       "SESSION2_EFFORT") # column names

colnames(SUBJECT_LEVEL_FINAL_QUESTIONS) = x # set column names

##### get all unique subject ids ####
SESSION1_AND_2_SUBJECTS = as.character(INCLUDE)

##### subject level measures summary ####

for (current_sub in SESSION1_AND_2_SUBJECTS) {
  
  # start variable at NA
  subject_data_session1 = NA
  subject_data_session2 = NA
  GROUP = NA
  DELAY = NA
  CONDITION = NA
  SESSION1_DIFFICULTY = NA
  SESSION1_EFFORT = NA
  SESSION2_DIFFICULTY = NA
  SESSION2_EFFORT = NA

  # get individual subject's rows for session 1
  subject_data_session1 = subset(SESSION1, subjectID == current_sub)
  
  # get group
  GROUP = subject_data_session1$GROUP[1]
  
  # get delay
  DELAY = subject_data_session1$DELAY[1]
  
  # get condition number
  CONDITION = subject_data_session1$conditionNumber[1]
  
  # get session 1 difficulty and effort responses
  SESSION1_DIFFICULTY = as.character(subset(subject_data_session1, experimentCondition == "session1_finalQuestions_difficulty")$responses)
  SESSION1_DIFFICULTY = as.character(substr(SESSION1_DIFFICULTY, 8, nchar(SESSION1_DIFFICULTY)-2))
  
  SESSION1_EFFORT = as.character(subset(subject_data_session1, experimentCondition == "session1_finalQuestions_effort")$responses)
  SESSION1_EFFORT = as.character(substr(SESSION1_EFFORT, 8, nchar(SESSION1_EFFORT)-2))
  
  # get individual subject's rows for session 2
  subject_data_session2 = subset(SESSION2, subjectID == current_sub)
  
  # get session 2 difficulty and effort responses
  SESSION2_DIFFICULTY = as.character(subset(subject_data_session2, experimentCondition == "session2_finalQuestions_difficulty")$responses)
  SESSION2_DIFFICULTY = as.character(substr(SESSION2_DIFFICULTY, 8, nchar(SESSION2_DIFFICULTY)-2))
  
  SESSION2_EFFORT = as.character(subset(subject_data_session2, experimentCondition == "session2_finalQuestions_effort")$responses)
  SESSION2_EFFORT = as.character(substr(SESSION2_EFFORT, 8, nchar(SESSION2_EFFORT)-2))
  
  # create a data frame with the subject's information
  new_subject_row = NA
  new_subject_row = data.frame(current_sub, 
                               GROUP,
                               DELAY,
                               CONDITION, 
                               SESSION1_DIFFICULTY,
                               SESSION1_EFFORT,
                               SESSION2_DIFFICULTY,
                               SESSION2_EFFORT)
  
  colnames(new_subject_row) = x # set column names
  
  # add subject to SUBJECT_LEVEL_S1 data frame
  SUBJECT_LEVEL_FINAL_QUESTIONS = rbind(SUBJECT_LEVEL_FINAL_QUESTIONS, new_subject_row)
  
}

##### SAVE TO FILE ####
write.csv(SUBJECT_LEVEL_FINAL_QUESTIONS, file = "../../R_OUTPUT/SUBJECT_LEVEL_FINAL_QUESTIONS.csv")
