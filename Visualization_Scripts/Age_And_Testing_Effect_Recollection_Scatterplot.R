##### SCRIPT INFO #####

# This script loads summary data and creates a visualization of 
# the relation between subject age and the magnitude of the 
# observed testing effect in recollection.

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

##### CALCULATE ESTIMATES OF THE TESTING EFFECT ####
CURRENT_DATA = SUBJECT_LEVEL_INFO_AND_ESTIMATES
CURRENT_DATA$TE_FAM = CURRENT_DATA$S2_FAM_WITH_FA_TEST - CURRENT_DATA$S2_FAM_WITH_FA_NOTEST
CURRENT_DATA$TE_REC = CURRENT_DATA$S2_REC_WITH_FA_TEST - CURRENT_DATA$S2_REC_WITH_FA_NOTEST

CURRENT_DATA$TE_KNOW = CURRENT_DATA$S2_PROP_HIT_K_TEST - CURRENT_DATA$S2_PROP_HIT_K_NOTEST
CURRENT_DATA$TE_REMEMBER = CURRENT_DATA$S2_PROP_HIT_R_TEST - CURRENT_DATA$S2_PROP_HIT_R_NOTEST
CURRENT_DATA$TE_HIT = CURRENT_DATA$S2_PROP_HIT_TEST - CURRENT_DATA$S2_PROP_HIT_NOTEST

##### VARIABLES #####
CURRENT_DATA # OVERALL

##################### VISUALIZATIONS ##################

# refactor / rename
CURRENT_DATA = 
  CURRENT_DATA %>%
  mutate(
    group = factor(group, levels=c("younger","older"), labels=c("Younger","Older")),
    delay = factor(delay, levels = c("delay0","delay1"), labels = c("Immediate","1 Day"))
  )

####### AGE --> TE REC ######
plot1 = ggplot(CURRENT_DATA[CURRENT_DATA$group=="Older",], aes(x = AGE, y = TE_REC)) + 
  geom_point(color=c("#000000"), size=3) + 
  xlab("Age (Older Adults)") + 
  ylab("Testing Effect in Recollection") + 
  geom_smooth(method=lm, se=FALSE, color = "black", size=2) +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size = 24, margin = margin(t = 15)), 
        axis.text.x = element_text(face="bold",size=24,color="black"), 
        axis.title.y = element_text(face="bold", size = 24, margin = margin(r = 15)), 
        axis.text.y = element_text(face="bold",size=24,color="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold", size = 20),
        legend.position = "none",
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(.25, "cm"),
        panel.border = element_blank(), 
        axis.line.x = element_line(size = 2, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 2, linetype = "solid", colour = "black"))
plot1
# saves plot
ggsave("../../R_OUTPUT/PLOTS/AGE_TESTING_EFFECT_REC.pdf", plot1, width=6, height=6)
