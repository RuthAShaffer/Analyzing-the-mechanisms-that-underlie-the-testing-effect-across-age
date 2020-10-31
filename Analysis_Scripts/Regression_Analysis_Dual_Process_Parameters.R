##### SCRIPT INFO #####

# This script loads summary data and then analyzes the relation
# between recollection and the magnitude of the testing effect in familiarity
# Author: Ruth A. Shaffer

##### SET WD TO FILE PATH ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### LOAD PACKAGES ####
library(sjPlot)
library(sjmisc)
library(sjlabelled)
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

##################### SESSION 2 DATA ORGANIZE ##################

##### CALCULATE TESTING EFFECT ESTIMATES ####
CURRENT_DATA = SUBJECT_LEVEL_INFO_AND_ESTIMATES
CURRENT_DATA$TE_FAM = CURRENT_DATA$S2_FAM_WITH_FA_TEST - CURRENT_DATA$S2_FAM_WITH_FA_NOTEST
CURRENT_DATA$TE_REC = CURRENT_DATA$S2_REC_WITH_FA_TEST - CURRENT_DATA$S2_REC_WITH_FA_NOTEST

CURRENT_DATA$TE_KNOW = CURRENT_DATA$S2_PROP_HIT_K_TEST - CURRENT_DATA$S2_PROP_HIT_K_NOTEST
CURRENT_DATA$TE_REMEMBER = CURRENT_DATA$S2_PROP_HIT_R_TEST - CURRENT_DATA$S2_PROP_HIT_R_NOTEST
CURRENT_DATA$TE_HIT = CURRENT_DATA$S2_PROP_HIT_TEST - CURRENT_DATA$S2_PROP_HIT_NOTEST

##### VARIABLES #####
CURRENT_DATA # OVERALL

##################### QUICK VISUALIZATIONS ##################

ggplot(CURRENT_DATA, aes(x=S2_REC_WITH_FA, y=TE_FAM)) +
  geom_point(shape=1) +
  stat_smooth(method = "lm")

ggplot(CURRENT_DATA, aes(x=S2_PROP_HIT_R, y=TE_KNOW)) +
  geom_point(shape=1) + 
  stat_smooth(method = "lm")

ggplot(CURRENT_DATA, aes(x=S2_PROP_HIT_R, y=TE_KNOW)) +
  geom_point(shape=1) + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

##################### ANALYSIS ##################

##### Testing effect in familiarity from overall recollection ####

# center continuous variables for analysis
centered = 
  CURRENT_DATA %>%
  mutate(
    S2_REC_WITH_FA.c = scale(S2_REC_WITH_FA, center = TRUE, scale=FALSE),
    AGE.c = scale(AGE,center=TRUE,scale=FALSE)
    )

# testing effect in familiarity from overall recollection
mod1A = lm(TE_FAM ~ S2_REC_WITH_FA.c, data = centered)
summary(mod1A)

# interaction with delay
mod1B = lm(TE_FAM ~ S2_REC_WITH_FA.c*delay, data = centered)
summary(mod1B)
anova(mod1B)
anova(mod1A,mod1B)

# interaction with age
mod1C = lm(TE_FAM ~ S2_REC_WITH_FA.c*AGE.c, data = centered)
summary(mod1C)
anova(mod1C)
anova(mod1A,mod1C)

# tables
tab_model(mod1A, show.std = TRUE,show.ci = .95 ,show.se = TRUE,show.stat = TRUE,
          col.order = c("est", "se", "ci","std.est","stat","p"))

tab_model(mod1B, show.std = TRUE,show.ci = .95 ,show.se = TRUE,show.stat = TRUE,
          col.order = c("est", "se", "ci","std.est","stat","p"))

tab_model(mod1C, show.std = TRUE,show.ci = .95 ,show.se = TRUE,show.stat = TRUE,
          col.order = c("est", "se", "ci","std.est","stat","p"))

# outliers 
rmv_outliers = 
  CURRENT_DATA %>%
  filter( abs(scale(TE_FAM, center = TRUE, scale = TRUE)) < 3 ) %>% 
  filter( abs(scale(S2_REC_WITH_FA, center = TRUE, scale = TRUE)) < 3 ) %>%
  mutate(
    S2_REC_WITH_FA.c = scale(S2_REC_WITH_FA, center = TRUE, scale=FALSE),
    AGE.c = scale(AGE,center=TRUE,scale=FALSE)
  )

length(rmv_outliers$subid)

# models excluding outliers
mod1A = lm(TE_FAM ~ S2_REC_WITH_FA.c, data = rmv_outliers)
mod1B = lm(TE_FAM ~ S2_REC_WITH_FA.c*delay, data = rmv_outliers)
mod1C = lm(TE_FAM ~ S2_REC_WITH_FA.c*AGE.c, data = rmv_outliers)
anova(mod1A)
anova(mod1B)
anova(mod1C)

##### Testing effect in familiarity from testing effect in recollection ####
# center continuous variables for analysis
centered = NA
centered = 
  CURRENT_DATA %>%
  mutate(
    TE_REC.c = scale(TE_REC, center = TRUE, scale=FALSE),
    AGE.c = scale(AGE,center=TRUE,scale=FALSE)
  )

# testing effect in familiarity from testing effect in recollection
mod1A = lm(TE_FAM ~ TE_REC.c, data = centered)
summary(mod1A)

# interaction with delay
mod1B = lm(TE_FAM ~ TE_REC.c*delay, data = centered)
summary(mod1B)
anova(mod1B)
anova(mod1A,mod1B)

# interaction with age
mod1C = lm(TE_FAM ~ TE_REC.c*AGE.c, data = centered)
summary(mod1C)
anova(mod1C)
anova(mod1A,mod1C)

# outliers 
rmv_outliers = 
  CURRENT_DATA %>%
  filter( abs(scale(TE_FAM, center = TRUE, scale = TRUE)) < 3 ) %>% 
  filter( abs(scale(TE_REC, center = TRUE, scale = TRUE)) < 3 )

length(rmv_outliers$subid) # none detected


##### Testing effect in Know from overall Remember ####
# center continuous variables for analysis
centered = NA
centered = 
  CURRENT_DATA %>%
  mutate(
    S2_PROP_HIT_R.c = scale(S2_PROP_HIT_R, center = TRUE, scale = FALSE),
    AGE.c = scale(AGE, center = TRUE,scale = FALSE)
  )

# Testing effect in raw Know responses from overall Remember responses
mod1A = lm(TE_KNOW ~ S2_PROP_HIT_R.c, data = centered)
summary(mod1A)

# tables for beta
tab_model(mod1A, show.std = TRUE,show.ci = .95 ,show.se = TRUE,show.stat = TRUE,
          col.order = c("est", "se", "ci","std.est","stat","p"))

# interaction with age
mod1B = lm(TE_KNOW ~ S2_PROP_HIT_R.c*AGE.c, data = centered)
summary(mod1B)
anova(mod1B)
anova(mod1A,mod1B)

# interaction with delay
centered$delay = relevel(centered$delay, ref="delay0")
mod1C = lm(TE_KNOW ~ S2_PROP_HIT_R.c*delay, data = centered)
anova(mod1A, mod1C)
anova(mod1C)
summary(mod1C)

# tables for beta
tab_model(mod1C, show.std = TRUE,show.ci = .95 ,show.se = TRUE,show.stat = TRUE,
          col.order = c("est", "se", "ci","std.est","stat","p"))

# relevel delay
centered$delay = relevel(centered$delay, ref="delay1")
# regression coefficients for releveled delay
mod1C2 = lm(TE_KNOW ~ S2_PROP_HIT_R.c*delay, data = centered)
summary(mod1C2)

# tables for beta
tab_model(mod1C2, show.std = TRUE,show.ci = .95 ,show.se = TRUE,show.stat = TRUE,
          col.order = c("est", "se", "ci","std.est","stat","p"))


# standardized
standardized_data = 
  CURRENT_DATA %>%
  mutate(
    S2_PROP_HIT_R.sd = scale(S2_PROP_HIT_R, center = TRUE, scale = TRUE),
    TE_KNOW.sd = scale(TE_KNOW, center = TRUE,scale = TRUE)
  )

# interaction with delay (standardized data)
mod1C.sd = lm(TE_KNOW.sd ~ S2_PROP_HIT_R.sd*delay, data = standardized_data)
summary(mod1C.sd)

# releveled delay variable
standardized_data$delay = relevel(standardized_data$delay, ref="delay1")
# regression coefficients for releveled delay
mod1C2.sd = lm(TE_KNOW.sd ~ S2_PROP_HIT_R.sd*delay, data = standardized_data)
summary(mod1C2.sd)


# outliers 
rmv_outliers = 
  CURRENT_DATA %>%
  filter( abs(scale(TE_KNOW, center = TRUE, scale = TRUE)) < 3 ) %>% 
  filter( abs(scale(S2_PROP_HIT_R, center = TRUE, scale = TRUE)) < 3 )

length(rmv_outliers$subid)

##### Testing effect in Know from testing effect in Remember ####
# center continuous variables for analysis
centered = NA
centered = 
  CURRENT_DATA %>%
  mutate(
    TE_REMEMBER.c = scale(TE_REMEMBER, center = TRUE, scale=FALSE),
    AGE.c = scale(AGE,center=TRUE,scale=FALSE)
  )

# testing effect in raw Know from testing effect in raw remember
mod1A = lm(TE_KNOW ~ TE_REMEMBER.c, data = centered)
summary(mod1A)

# table for betas
tab_model(mod1A, show.std = TRUE,show.ci = .95 ,show.se = TRUE,show.stat = TRUE,
          col.order = c("est", "se", "ci","std.est","stat","p"))

# interaction with age
mod1B = lm(TE_KNOW ~ TE_REMEMBER.c*AGE.c, data = centered)
summary(mod1B)
anova(mod1B)
anova(mod1A,mod1B)

# interaction with delay
centered$delay = relevel(centered$delay, ref="delay0")
mod1C = lm(TE_KNOW ~ TE_REMEMBER.c*delay, data = centered)
summary(mod1C)
anova(mod1C)
anova(mod1A, mod1C)

# outliers 
rmv_outliers = 
  CURRENT_DATA %>%
  filter( abs(scale(TE_KNOW, center = TRUE, scale = TRUE)) < 3 ) %>% 
  filter( abs(scale(TE_REMEMBER, center = TRUE, scale = TRUE)) < 3 )

length(rmv_outliers$subid) 

######## QUADRATIC TREND #####
##### Testing effect in familiarity from overall recollection ####
# center continuous variables for analysis
centered = NA
centered = 
  CURRENT_DATA %>%
  mutate(
    S2_REC_WITH_FA.c = scale(S2_REC_WITH_FA, center = TRUE, scale=FALSE)
  )

# Quadratic relation between overall recollection and the testing effect in familiarity
mod1A = lm(TE_FAM ~ S2_REC_WITH_FA.c + I(S2_REC_WITH_FA.c^2), data = centered)
summary(mod1A)
anova(mod1A)

# table for betas
tab_model(mod1A, show.std = TRUE,show.ci = .95 ,show.se = TRUE,show.stat = TRUE,
          col.order = c("est", "se", "ci","std.est","stat","p"))

# standardized beta
standardized_data = NA
standardized_data = 
  CURRENT_DATA %>%
  mutate(
    S2_REC_WITH_FA.sd = scale(S2_REC_WITH_FA, center = TRUE, scale=TRUE),
    TE_FAM.sd = scale(TE_FAM, center = TRUE, scale=TRUE)
  )

# analysis of standardized variables
mod1A.sd = lm(TE_FAM.sd ~ S2_REC_WITH_FA.sd + I(S2_REC_WITH_FA.sd^2), data = standardized_data)
summary(mod1A.sd)

# outliers 
rmv_outliers = 
  CURRENT_DATA %>%
  filter( abs(scale(TE_FAM, center = TRUE, scale = TRUE)) < 3 ) %>% 
  filter( abs(scale(S2_REC_WITH_FA, center = TRUE, scale = TRUE)) < 3 ) %>%
  mutate(
    S2_REC_WITH_FA.c = scale(S2_REC_WITH_FA, center = TRUE, scale=FALSE)
  )

length(rmv_outliers$subid) 

# model with outliers removed
mod1A = lm(TE_FAM ~ S2_REC_WITH_FA.c + I(S2_REC_WITH_FA.c^2), data = rmv_outliers)
summary(mod1A)
anova(mod1A)

# table for betas
tab_model(mod1A, show.std = TRUE,show.ci = .95 ,show.se = TRUE,show.stat = TRUE,
          col.order = c("est", "se", "ci","std.est","stat","p"))

# standardized beta
standardized_data = NA
standardized_data = 
  rmv_outliers %>%
  #group_by(group) %>%
  mutate(
    S2_REC_WITH_FA.sd = scale(S2_REC_WITH_FA, center = TRUE, scale=TRUE),
    TE_FAM.sd = scale(TE_FAM, center = TRUE, scale=TRUE)
  )

# standardized variables with outliers removed
mod1A.sd = lm(TE_FAM.sd ~ S2_REC_WITH_FA.sd + I(S2_REC_WITH_FA.sd^2), data = standardized_data)
summary(mod1A.sd)

##### Testing effect in know from overall remember ####
# center continuous variables for analysis
centered = NA
centered = 
  CURRENT_DATA %>%
  mutate(
    S2_PROP_HIT_R.c = scale(S2_PROP_HIT_R, center = TRUE, scale=FALSE)
  )

# quadratic relation between raw Remember hits and the testing effect in raw Know hits
mod1A = lm(TE_KNOW ~ S2_PROP_HIT_R.c + I(S2_PROP_HIT_R.c^2), data = centered)
summary(mod1A)
anova(mod1A)

# table for betas
tab_model(mod1A, show.std = TRUE,show.ci = .95 ,show.se = TRUE,show.stat = TRUE,
          col.order = c("est", "se", "ci","std.est","stat","p"))

# standardized beta
standardized_data = NA
standardized_data = 
  CURRENT_DATA %>%
  mutate(
    S2_PROP_HIT_R.sd = scale(S2_PROP_HIT_R, center = TRUE, scale=TRUE),
    TE_KNOW.sd = scale(TE_KNOW, center = TRUE, scale=TRUE)
  )

# analysis with standardized variables
mod1A.sd = lm(TE_KNOW.sd ~ S2_PROP_HIT_R.sd + I(S2_PROP_HIT_R.sd^2), data = standardized_data)
summary(mod1A.sd)


# outliers 
rmv_outliers = 
  CURRENT_DATA %>%
  filter( abs(scale(TE_KNOW, center = TRUE, scale = TRUE)) < 3 ) %>% 
  filter( abs(scale(S2_PROP_HIT_R, center = TRUE, scale = TRUE)) < 3 )

length(rmv_outliers$subid) 
