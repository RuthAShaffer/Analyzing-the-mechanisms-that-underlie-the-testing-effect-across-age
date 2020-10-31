##### SCRIPT INFO #####

# This script loads summary data
# and then creates a demographic information table

# Author: Ruth A. Shaffer

##### SET WD TO FILE PATH ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### LOAD PACKAGES ####
library("readxl")
library("ggplot2")
library("tidyverse")

##################### IMPORT DATA ##################### 

##### IMPORT SUMMARY DATA ####
SUBJECT_LEVEL_ESTIMATES = as.data.frame(read.csv("../../DATA/R_SUMMARY_DATA/SUBJECT_LEVEL_ESTIMATES.csv",header=TRUE,sep=","))

##### IMPORT SUBJECT INFO ####
 
# import data
SUBJECT_INFO_YOUNGER = as.data.frame(read_excel("../../DATA/SUBJECT_INFO/YOUNGER_ADULT_SUB_INFO.xlsx"))
SUBJECT_INFO_OLDER = as.data.frame(read_excel("../../DATA/SUBJECT_INFO/OLDER_ADULT_SUB_INFO.xlsx"))

##################### SUBJECT INFO DATA ORGANIZE ##################### 

# select included subjects only
SUBJECT_INFO_YOUNGER = subset(SUBJECT_INFO_YOUNGER,SUBID %in% SUBJECT_LEVEL_ESTIMATES$subid)
SUBJECT_INFO_OLDER = subset(SUBJECT_INFO_OLDER,SUBID %in% SUBJECT_LEVEL_ESTIMATES$subid)

# Add null MMSE column for younger
SUBJECT_INFO_YOUNGER$MMSE_SCORE = NA

# select relevant columns
SUBJECT_INFO_YOUNGER = SUBJECT_INFO_YOUNGER[,c("SUBID","DELAY","AGE","SEX","ETHNICITY","RACE","YRS_EDU","MMSE_SCORE")]
SUBJECT_INFO_OLDER = SUBJECT_INFO_OLDER[,c("SUBID","DELAY","AGE","SEX","ETHNICITY","RACE","YRS_EDU","MMSE_SCORE")]

# add age group column
SUBJECT_INFO_YOUNGER$GROUP = "Younger"
SUBJECT_INFO_OLDER$GROUP = "Older"

# merge
SUBJECT_INFO = rbind(SUBJECT_INFO_YOUNGER,SUBJECT_INFO_OLDER)

# set classes for columns
SUBJECT_INFO = 
  SUBJECT_INFO %>%
  mutate(
    SUBID = factor(SUBID),
    DELAY = factor(DELAY,levels=c(0,1), labels=c("No Delay", "1 Day Delay")),
    AGE = as.numeric(AGE),
    SEX = factor(SEX),
    ETHNICITY = factor(ETHNICITY),
    RACE = factor(RACE),
    YRS_EDU = as.numeric(YRS_EDU),
    MMSE_SCORE = as.numeric(MMSE_SCORE),
    GROUP = factor(GROUP,levels=c("Younger","Older"))
  )

# organize "Race" information column based on participant responses
SUBJECT_INFO[SUBJECT_INFO$RACE=="Asian; Caucasian; More than one race",]$RACE = "More than one race"
SUBJECT_INFO[SUBJECT_INFO$RACE=="Asian; Caucasian; Other; More than one race",]$RACE = "More than one race"
SUBJECT_INFO[SUBJECT_INFO$RACE=="Black / African American; More than one race",]$RACE = "More than one race"
SUBJECT_INFO$RACE = factor(SUBJECT_INFO$RACE)

##################### INFORMATION FOR DEMOGRAPHIC INFO TABLE ##################### 

DEMOGRAPHIC_INFO_SUMMARIZED = 
SUBJECT_INFO %>%
  group_by(GROUP,DELAY) %>%
  summarise(SUBS_N = n(),
            AGE_MEAN = round(mean(AGE),1),
            AGE_SD = round(sd(AGE),1),
            AGE_MIN = min(AGE),
            AGE_MAX = max(AGE),
            FEMALE = sum(SEX=="F"),
            MALE = sum(SEX=="M"),
            YRS_EDU_MEAN = round(mean(YRS_EDU),1),
            YRS_EDU_SD = round(sd(YRS_EDU),1),
            MMSE_MEAN = round(mean(MMSE_SCORE),1),
            MMSE_SD = round(sd(MMSE_SCORE),1),
            ETHNICITY_HISPANIC = sum(ETHNICITY=="Hispanic / Latino"),
            ETHNICITY_NOT_HISPANIC = sum(ETHNICITY=="Not hispanic/latino"),
            ETHNICITY_NA = sum(ETHNICITY=="BLANK"),
            Asian = sum(RACE=="Asian"),
            AfricanAmerican = sum(RACE=="Black / African American"),
            Caucasian = sum(RACE=="Caucasian"),
            PacificIslander = sum(RACE=="Native Hawaiian / Pacific Islander"),
            MoreThanOne = sum(RACE=="More than one race"),
            Other = sum(RACE=="Other"))
DEMOGRAPHIC_INFO_SUMMARIZED   

##### SAVE TO FILE ####

write.csv(DEMOGRAPHIC_INFO_SUMMARIZED, file = "../../R_OUTPUT/DEMOGRAPHIC_INFO_SUMMARIZED.csv")

