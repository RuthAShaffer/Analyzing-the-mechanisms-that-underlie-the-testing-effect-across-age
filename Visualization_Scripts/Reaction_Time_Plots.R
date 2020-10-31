##### SCRIPT INFO #####

# This script cleans, organizes, and then creates 
# visualizations of response time (RT) data.

# Author: Ruth A. Shaffer
# For outlier analysis, modification of code from Violet Brown

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

# below is modified from outlier RT code from Violet Brown:

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

############## GRAPHS ############## 

SESSION2_RT_FINAL$correct_coded = factor(SESSION2_RT_FINAL$correct_coded,levels=c("hit","CR","FA","miss"), labels = c("Hit","CR","FA","Miss"))

##### Older vs. younger X hit,CR,FA,miss #####
# make violin plot: item-level data
RT_GROUP_hitCRFAmiss_ITEM_LEVEL =
ggplot(SESSION2_RT_FINAL, aes(x=GROUP,y=rt)) + 
  scale_x_discrete(labels=c("Younger", "Older")) +
  facet_grid(~ correct_coded) +
  geom_violin(width = .6, position = position_dodge(width=0.5),fill='gray74') +
  stat_summary(fun.y="median",geom='point',size = 1, position = position_dodge(width=0.5) ) + 
  scale_y_continuous(limits = c(0,22000),
                     breaks=seq(0,22000,2000),
                     expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black", fill=NA),
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank()) +
  labs (x = "Age Group", y = "Response time (ms)") 

ggsave("../../R_OUTPUT/PLOTS/RT_GROUP_hitCRFAmiss_ITEM_LEVEL.pdf", RT_GROUP_hitCRFAmiss_ITEM_LEVEL, width=9, height=5.5)

# obtain subject means per group
means_analysis=NA
means_analysis = 
  SESSION2_RT_FINAL %>%
  group_by(subjectID,correct_coded,GROUP) %>%
  summarise(mean_rt = mean(rt,na.rm=TRUE))

# make violin plot: subject-level data
RT_GROUP_hitCRFAmiss_SUBJECT_LEVEL = 
ggplot(means_analysis, aes(x=GROUP,y=mean_rt)) + 
  scale_x_discrete(labels=c("Younger", "Older")) +
  facet_grid(~ correct_coded) +
  geom_violin(width = .6, position = position_dodge(width=0.5),fill='gray74') +
  stat_summary(fun.y="median",geom='point',size = 1, position = position_dodge(width=0.5) ) + 
   scale_y_continuous(limits = c(500,10000),
                      breaks = seq(500,10000,1000),
                     expand = c(0,0)
   ) +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black", fill=NA),
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank()) +
  labs (x = "Age Group", y = "Response Time (ms)\n(from subject-level means per condition)") 

ggsave("../../R_OUTPUT/PLOTS/RT_GROUP_hitCRFAmiss_SUBJECT_LEVEL.pdf", RT_GROUP_hitCRFAmiss_SUBJECT_LEVEL, width=9, height=5.5)

##### Remember hit vs. Know hit #####

# select Remember and Know hits only
data = subset(SESSION2_RT_FINAL,correct_coded == "Hit" & (formatted_response_confidence_RKN == "R" | formatted_response_confidence_RKN == "K"))
data$formatted_response_confidence_RKN = factor(data$formatted_response_confidence_RKN, levels = c("R","K"), labels=c("Remember Hit","Know Hit"))

# make violin plot: item-level data
RT_GROUP_RhitKhit_ITEM_LEVEL = 
ggplot(data, aes(x=GROUP,y=rt)) + 
  scale_x_discrete(labels=c("Younger", "Older")) +
  facet_grid(~ formatted_response_confidence_RKN) +
  geom_violin(width = .6, position = position_dodge(width=0.5),fill='gray74') +
  stat_summary(fun.y="median",geom='point',size = 1, position = position_dodge(width=0.5) ) + 
  scale_y_continuous(limits = c(0,15000),
                     breaks=seq(0,15000,2000),
                     expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black", fill=NA),
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank()) +
  labs (x = "Age Group", y = "Response time (ms)") 

ggsave("../../R_OUTPUT/PLOTS/RT_GROUP_RhitKhit_ITEM_LEVEL.pdf", RT_GROUP_RhitKhit_ITEM_LEVEL, width=9, height=5.5)


# obtain subject means per group
means_analysis = NA
means_analysis = 
  data %>%
  group_by(subjectID,formatted_response_confidence_RKN,GROUP) %>%
  summarise(mean_rt = mean(rt,na.rm=TRUE))

# make violin plot: subject-level data
RT_GROUP_RhitKhit_SUBJECT_LEVEL = 
ggplot(means_analysis, aes(x=GROUP,y=mean_rt)) + 
  scale_x_discrete(labels=c("Younger", "Older")) +
  facet_grid(~ formatted_response_confidence_RKN) +
  geom_violin(width = .6, position = position_dodge(width=0.5),fill='gray74') +
  stat_summary(fun.y="median",geom='point',size = 1, position = position_dodge(width=0.5) ) + 
  scale_y_continuous(limits = c(500,7000),
                     breaks = seq(500,7000,1000),
                     expand = c(0,0)
  ) +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black", fill=NA),
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank()) +
  labs (x = "Age Group", y = "Response Time (ms)\n(from subject-level means per condition)") 

ggsave("../../R_OUTPUT/PLOTS/RT_GROUP_RhitKhit_SUBJECT_LEVEL.pdf", RT_GROUP_RhitKhit_SUBJECT_LEVEL, width=9, height=5.5)


##### Test hit vs. no test hit #####

# select hits only
data = NA
data = subset(SESSION2_RT_FINAL,correct_coded == "Hit")
data$prior_condition_combo = factor(data$prior_condition_combo)
data$prior_condition_combo = factor(data$prior_condition_combo, levels = c("test","notest"), labels = c("Test Hit","No Test Hit"))

# make violin plot: item-level data
RT_GROUP_TestHitNoTestHit_ITEM_LEVEL = 
ggplot(data, aes(x=GROUP,y=rt)) + 
  scale_x_discrete(labels=c("Younger", "Older")) +
  facet_grid(~ prior_condition_combo) +
  geom_violin(width = .6, position = position_dodge(width=0.5),fill='gray74') +
  stat_summary(fun.y="median",geom='point',size = 1, position = position_dodge(width=0.5) ) + 
  scale_y_continuous(limits = c(0,15000),
                     breaks=seq(0,15000,2000),
                     expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black", fill=NA),
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank()) +
  labs (x = "Age Group", y = "Response time (ms)") 

ggsave("../../R_OUTPUT/PLOTS/RT_GROUP_TestHitNoTestHit_ITEM_LEVEL.pdf", RT_GROUP_TestHitNoTestHit_ITEM_LEVEL, width=9, height=5.5)

# obtain subject means per group
means_analysis = NA
means_analysis = 
  data %>%
  group_by(subjectID,prior_condition_combo,GROUP) %>%
  summarise(mean_rt = mean(rt,na.rm=TRUE))

# make violin plot: subject-level data
RT_GROUP_TestHitNoTestHit_SUBJECT_LEVEL = 
ggplot(means_analysis, aes(x=GROUP,y=mean_rt)) + 
  scale_x_discrete(labels=c("Younger", "Older")) +
  facet_grid(~ prior_condition_combo) +
  geom_violin(width = .6, position = position_dodge(width=0.5),fill='gray74') +
  stat_summary(fun.y="median",geom='point',size = 1, position = position_dodge(width=0.5) ) + 
  scale_y_continuous(limits = c(500,7000),
                     breaks = seq(500,7000,1000),
                     expand = c(0,0)
  ) +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black", fill=NA),
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank()) +
  labs (x = "Age Group", y = "Response Time (ms)\n(from subject-level means per condition)") 

ggsave("../../R_OUTPUT/PLOTS/RT_GROUP_TestHitNoTestHit_SUBJECT_LEVEL.pdf", RT_GROUP_TestHitNoTestHit_SUBJECT_LEVEL, width=9, height=5.5)
