##### SCRIPT INFO #####

# This script loads summary data and then analyzes 
# Dual-Process Signal-Detection parameter estimates 
# for older and yunger adult subjects in 2 retention
# interval conditions (immediate / 1-day delayed).

# Author: Ruth A. Shaffer

##### SET WD TO FILE PATH ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### LOAD PACKAGES ####
library("readxl")
library("tidyverse")
library(ez)

##################### IMPORT DATA ##################### 

##### IMPORT SUMMARY DATA ####
SUBJECT_LEVEL_ESTIMATES = as.data.frame(read.csv("../../DATA/R_SUMMARY_DATA/SUBJECT_LEVEL_ESTIMATES.csv",header=TRUE,sep=","))

##### IMPORT SUBJECT INFO ####
 
# import data
SUBJECT_INFO_YOUNGER = as.data.frame(read_excel("../../DATA/SUBJECT_INFO/YOUNGER_ADULT_SUB_INFO.xlsx"))
SUBJECT_INFO_OLDER = as.data.frame(read_excel("../../DATA/SUBJECT_INFO/OLDER_ADULT_SUB_INFO.xlsx"))

##################### SUBJECT INFO DATA ORGANIZE ##################### 

##### ORGANIZE AND COMBINE #####
# select columns
SUBJECT_INFO_YOUNGER = SUBJECT_INFO_YOUNGER[,c("SUBID","AGE","SEX","YRS_EDU")]
SUBJECT_INFO_OLDER = SUBJECT_INFO_OLDER[,c("SUBID","AGE","SEX","YRS_EDU")]

# merge
SUBJECT_INFO = rbind(SUBJECT_INFO_YOUNGER,SUBJECT_INFO_OLDER)

# rename SUBID to subid to match SUBJECT_LEVEL_ESTIMATES subid
names(SUBJECT_INFO)[names(SUBJECT_INFO) == 'SUBID'] = 'subid'

# select only subjects that are included in subject level estimates file
SUBJECT_LEVEL_INFO_AND_ESTIMATES = merge(SUBJECT_INFO, SUBJECT_LEVEL_ESTIMATES, by = "subid", all.y=TRUE)

##### VARIABLES: SUMMARY DATA ####
SUBJECT_LEVEL_ESTIMATES
SUBJECT_INFO # includes all subjects (even excluded)
SUBJECT_LEVEL_INFO_AND_ESTIMATES # includes only included subjects based on SUBJECT_LEVEL_ESTIMATES

##################### SESSION 2 DATA ORGANIZE ##################

##### PREP ####
# data for working with
SESSION2_DATA = SUBJECT_LEVEL_INFO_AND_ESTIMATES

# variables as factors / order the factors
SESSION2_DATA = SESSION2_DATA %>% 
  mutate(subid = factor(subid),
         group = factor(group, levels = c("younger", "older"), labels = c("Younger", "Older")),
         delay = factor(delay, levels = c("delay0", "delay1"), labels = c("delay0", "delay1")))

##### CHECK FOR NAs #####

# overall
sum(is.na(SESSION2_DATA$S2_REC_WITH_FA_TEST)) # should be 0
sum(is.na(SESSION2_DATA$S2_REC_WITH_FA_NOTEST)) # should be 0
sum(is.na(SESSION2_DATA$S2_FAM_WITH_FA_TEST)) # should be 0
sum(is.na(SESSION2_DATA$S2_FAM_WITH_FA_NOTEST)) # should be 0

##### ORGANIZE DATA: LONG FORMAT #####

SESSION2_DATA_LONG = 
  gather(SESSION2_DATA, 
         temp_name, 
         estimate, 
         c("S2_REC_WITH_FA_TEST",
           "S2_REC_WITH_FA_NOTEST",
           "S2_FAM_WITH_FA_TEST",
           "S2_FAM_WITH_FA_NOTEST"), 
         factor_key=TRUE) %>% 
  separate(temp_name, 
           sep = "_WITH_FA_",
           c("parameter", "prior_condition")) %>% 
  mutate(prior_condition = factor(prior_condition, levels = c("TEST", "NOTEST"), labels = c("test", "notest")),
         parameter = factor(parameter, levels = c("S2_REC","S2_FAM"), labels = c("recollection","familiarity")))

##### VARIABLES: OVERALL AND LONG FORMAT #####
SESSION2_DATA # OVERALL
SESSION2_DATA_LONG # LONG FORMAT

##################### COMBO ADULT DATA ANALYZE ##################### 


##### SUMMARY STATS OVERALL #####
# get summary stats (mean / SE)
SESSION2_DATA_LONG_SUMMARY = SESSION2_DATA_LONG %>% 
  group_by(group,delay,parameter,prior_condition) %>% 
  summarise(MEAN_ESTIMATE = mean(estimate,na.rm=TRUE),
            SE_ESTIMATE = sd(estimate,na.rm=TRUE)/sqrt(sum(!is.na(estimate))))

##### DIAGNOSTICS #####

# histograms
ggplot(SESSION2_DATA_LONG, aes(x = estimate)) +
  geom_histogram(aes(color = prior_condition, fill = prior_condition), 
                 position = "identity", bins = 30, alpha = 0.3) +
  facet_wrap(~ group*parameter*delay)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

# box plots for main effects of delay and prior condition
plot(estimate~group*delay*prior_condition*parameter, data=SESSION2_DATA_LONG)

# see order of variables
levels(SESSION2_DATA_LONG$group)
levels(SESSION2_DATA_LONG$prior_condition)
levels(SESSION2_DATA_LONG$delay)
levels(SESSION2_DATA_LONG$parameter)


##### DEFINE FUNCTION FOR MODEL OUTPUT ####
# define function
organize_ez = function(ezANOVA.model.output){
  temp.model = ezANOVA.model.output$ANOVA
  temp.model$partial_eta = temp.model$SSn / (temp.model$SSn + temp.model$SSd)
  temp.model$effect_name = temp.model$Effect
  temp.model$rounded_F = round(temp.model$F,2)
  temp.model$rounded_p = round(temp.model$p,3)
  temp.model$s = temp.model$`p<.05`
  temp.model$rounded_partial_eta = round(temp.model$partial_eta,2)
  return(temp.model)
}

##### REC / FAM, NO TEST, OLDER VS YOUNGER ##### 
##### MAIN: EZ ANOVA (TYPE III) #####

# RECOLLECTION
recollection_data = subset(SESSION2_DATA_LONG,parameter=="recollection")
ezanova.model.rec = 
  ezANOVA(
    data = recollection_data
    , dv = estimate
    , wid = subid
    , within = prior_condition
    , within_full = NULL
    , within_covariates = NULL
    , between = .(group,delay)
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )
# calculations and summary information
organize_ez(ezanova.model.rec)

# follow-up marginal means for interaction
SESSION2_DATA %>% 
  mutate(TE_REC = S2_REC_WITH_FA_TEST-S2_REC_WITH_FA_NOTEST) %>%
  group_by(group,delay) %>% 
  summarise(MEAN_ESTIMATE = mean(TE_REC,na.rm=TRUE)) %>%
  group_by(group) %>%
  summarise(MEAN = round(mean(MEAN_ESTIMATE,na.rm=TRUE),2))

# follow-up marginal means for trend
SESSION2_DATA %>% 
  mutate(TE_REC = S2_REC_WITH_FA_TEST-S2_REC_WITH_FA_NOTEST) %>%
  group_by(group,delay) %>% 
  summarise(MEAN_ESTIMATE = mean(TE_REC,na.rm=TRUE)) %>%
  group_by(delay) %>%
  summarise(MEAN = round(mean(MEAN_ESTIMATE,na.rm=TRUE),2))


# testing effect effect for older and younger, collapsed across delay
ezanova.model.rec.younger = 
  ezANOVA(
    data = recollection_data[recollection_data$group=="Younger",]
    , dv = estimate
    , wid = subid
    , within = prior_condition
    , within_full = NULL
    , within_covariates = NULL
    , between = NULL
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )
# calculations and summary information
organize_ez(ezanova.model.rec.younger)

recollection_data[recollection_data$group=="Younger",] %>% 
  group_by(prior_condition) %>%
  summarise(MEAN = round(mean(estimate,na.rm=TRUE),2))

ezanova.model.rec.older = 
  ezANOVA(
    data = recollection_data[recollection_data$group=="Older",]
    , dv = estimate
    , wid = subid
    , within = prior_condition
    , within_full = NULL
    , within_covariates = NULL
    , between = NULL
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )
# calculations and summary information
organize_ez(ezanova.model.rec.older)

recollection_data[recollection_data$group=="Older",] %>% 
  group_by(prior_condition) %>%
  summarise(MEAN = round(mean(estimate,na.rm=TRUE),2))


# age effect for test vs. no test, collapsed across delay
ezanova.model.rec.notest = 
  ezANOVA(
    data = recollection_data[recollection_data$prior_condition=="notest",]
    , dv = estimate
    , wid = subid
    , within = NULL
    , within_full = NULL
    , within_covariates = NULL
    , between = .(group)
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )
# calculations and summary information
organize_ez(ezanova.model.rec.notest)

recollection_data[recollection_data$prior_condition=="notest",] %>% 
  group_by(group) %>%
  summarise(MEAN = round(mean(estimate,na.rm=TRUE),2))

ezanova.model.rec.test = 
  ezANOVA(
    data = recollection_data[recollection_data$prior_condition=="test",]
    , dv = estimate
    , wid = subid
    , within = NULL
    , within_full = NULL
    , within_covariates = NULL
    , between = .(group)
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )
# calculations and summary information
organize_ez(ezanova.model.rec.test)

recollection_data[recollection_data$prior_condition=="test",] %>% 
  group_by(group) %>%
  summarise(MEAN = round(mean(estimate,na.rm=TRUE),2))


# FAMILIARITY 
familiarity_data = subset(SESSION2_DATA_LONG,parameter=="familiarity")
ezanova.model.fam = 
  ezANOVA(
    data = familiarity_data
    , dv = estimate
    , wid = subid
    , within = prior_condition
    , within_full = NULL
    , within_covariates = NULL
    , between = .(group,delay)
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )
# calculations and summary information
organize_ez(ezanova.model.fam)

# follow-up marginal means for interaction
SESSION2_DATA %>% 
  mutate(TE_FAM = S2_FAM_WITH_FA_TEST-S2_FAM_WITH_FA_NOTEST) %>%
  group_by(group,delay) %>% 
  summarise(MEAN_ESTIMATE = mean(TE_FAM,na.rm=TRUE)) %>%
  group_by(group) %>%
  summarise(MEAN = round(mean(MEAN_ESTIMATE,na.rm=TRUE),2))

## COMBINED PARAMETER ESTIMATES ##
ezanova.model.combo = 
  ezANOVA(
    data = SESSION2_DATA_LONG
    , dv = estimate
    , wid = subid
    , within = .(prior_condition,parameter)
    , within_full = NULL
    , within_covariates = NULL
    , between = .(group,delay)
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )
# calculations and summary information
organize_ez(ezanova.model.combo)

# followup on 3-way interaction
familiarity_data = subset(SESSION2_DATA_LONG,parameter == "familiarity")

ezanova.model.combo = 
  ezANOVA(
    data = familiarity_data
    , dv = estimate
    , wid = subid
    , within = .(prior_condition)
    , within_full = NULL
    , within_covariates = NULL
    , between = delay
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )
# calculations and summary information
organize_ez(ezanova.model.combo)

# followup on 3-way interaction
recollection_data = subset(SESSION2_DATA_LONG,parameter=="recollection")

ezanova.model.combo = 
  ezANOVA(
    data = recollection_data
    , dv = estimate
    , wid = subid
    , within = .(prior_condition)
    , within_full = NULL
    , within_covariates = NULL
    , between = delay
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )
# calculations and summary information
organize_ez(ezanova.model.combo)

# marginal means collapse across group, because above analysis is collapsed across group
SESSION2_DATA %>% 
  mutate(TE_REC = S2_REC_WITH_FA_TEST-S2_REC_WITH_FA_NOTEST,
         TE_FAM = S2_FAM_WITH_FA_TEST-S2_FAM_WITH_FA_NOTEST) %>%
  group_by(delay) %>%
  summarise(MEAN_RECOLLECTION_TE = round(mean(TE_REC,na.rm=TRUE),2),
            MEAN_FAMILIARITY_TE = round(mean(TE_FAM,na.rm=TRUE),2))

# follow-up on 2-way interaction between group / parameter
recollection_data = subset(SESSION2_DATA_LONG,parameter=="recollection")

ezanova.model.combo = 
  ezANOVA(
    data = recollection_data
    , dv = estimate
    , wid = subid
    , within = NULL
    , within_full = prior_condition
    , within_covariates = NULL
    , between = group
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )

organize_ez(ezanova.model.combo)

# follow-up on 2-way interaction between group / parameter
familiarity_data = subset(SESSION2_DATA_LONG,parameter=="familiarity")

ezanova.model.combo = 
  ezANOVA(
    data = familiarity_data
    , dv = estimate
    , wid = subid
    , within = NULL
    , within_full = prior_condition
    , within_covariates = NULL
    , between = group
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )

organize_ez(ezanova.model.combo)


# follow-up on 2-way marginally significant interaction between group / prior condition
younger_data = subset(SESSION2_DATA_LONG,group=="Younger")

ezanova.model.combo = 
  ezANOVA(
    data = younger_data
    , dv = estimate
    , wid = subid
    , within = prior_condition
    , within_full = parameter
    , within_covariates = NULL
    , between = NULL
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )

organize_ez(ezanova.model.combo)

# follow-up on 2-way marginally significant interaction between group / prior condition
older_data = subset(SESSION2_DATA_LONG,group=="Older")

ezanova.model.combo = 
  ezANOVA(
    data = older_data
    , dv = estimate
    , wid = subid
    , within = prior_condition
    , within_full = parameter
    , within_covariates = NULL
    , between = NULL
    , between_covariates = NULL
    , observed = NULL
    , diff = NULL
    , reverse_diff = FALSE
    , type = 3
    , white.adjust = FALSE
    , detailed = TRUE
    , return_aov = FALSE
  )

organize_ez(ezanova.model.combo)
