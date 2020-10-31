##### SCRIPT INFO #####

# This script loads summary data and creates scatterplots depicting
# the relation between Dual-Process parameter estimates of 
# recollection and familiarity (overall and the testing effect) and 
# between raw Remember and Know responses (overall and the testing effect).

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

# refactor / rename group variables (age group / delay condition)
CURRENT_DATA = 
  CURRENT_DATA %>%
  mutate(
    group = factor(group, levels=c("younger","older"), labels=c("Younger","Older")),
    delay = factor(delay, levels = c("delay0","delay1"), labels = c("No Delay","1 Day Delay"))
  )

####### REC --> TE FAM ######
# Recollection and the testing effect in familiarity
plot1 = ggplot(CURRENT_DATA, aes(x = S2_REC_WITH_FA, y = TE_FAM)) + 
  geom_point(color=c("#000000"), size=2) + 
  xlab("Recollection") + 
  ylab("Testing Effect in Familiarity") + 
  scale_x_continuous(breaks=seq(-1, 1, .2)) + 
  scale_y_continuous(breaks=seq(-1, 1, .2)) +
  geom_smooth(method=lm, se=FALSE, color = "black", size=1) +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, size = 2, linetype = "solid", colour = "black"),
        legend.title = element_text(size=12, colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.position = c(0.115, 0.91),
        panel.spacing = unit(-.13, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size=12, colour = "black"))
plot1
# saves plot
ggsave("../../R_OUTPUT/PLOTS/Rec_TEfam.pdf", plot1, width=5, height=5)

# Recollection and the testing effect in familiarity, by delay and age group
plot2 = ggplot(CURRENT_DATA, aes(x = S2_REC_WITH_FA, y = TE_FAM)) + 
  geom_point(color=c("#000000"), size=2) + 
  xlab("Recollection") + 
  ylab("Testing Effect in Familiarity") + 
  facet_grid(delay~group) +
  scale_x_continuous(breaks=seq(-1, 1, .2)) + 
  scale_y_continuous(breaks=seq(-1, 1, .2)) +
  geom_smooth(method=lm, se=FALSE, color = "black", size=1) +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title.x = element_text(size = 18, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=18, colour = "black"), 
        axis.title.y = element_text(size = 18, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=18, colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, size = 2, linetype = "solid", colour = "black"),
        legend.title = element_text(size=18, colour = "black"),
        legend.text = element_text(size=18, colour = "black"),
        legend.position = c(0.115, 0.91),
        panel.spacing = unit(-.13, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size=18, colour = "black"))
plot2
# saves plot
ggsave("../../R_OUTPUT/PLOTS/Rec_TEfam_by_AgeGroup_and_Delay.pdf", plot2, width=9, height=9)

####### TE REC --> TE FAM ######
# The testing effect in recollection and the testing effect in familiarity
plot1b = ggplot(CURRENT_DATA, aes(x = TE_REC, y = TE_FAM)) + 
  geom_point(color=c("#000000"), size=2) + 
  xlab("Testing Effect in Recollection") + 
  ylab("Testing Effect in Familiarity") + 
  scale_x_continuous(breaks=seq(-1, 1, .2)) + 
  scale_y_continuous(breaks=seq(-1, 1, .2)) +
  geom_smooth(method=lm, se=FALSE, color = "black", size=1) +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title.x = element_text(size = 18, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=18, colour = "black"), 
        axis.title.y = element_text(size = 18, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=18, colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, size = 2, linetype = "solid", colour = "black"),
        legend.title = element_text(size=18, colour = "black"),
        legend.text = element_text(size=18, colour = "black"),
        legend.position = c(0.115, 0.91),
        panel.spacing = unit(-.13, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size=18, colour = "black"))
plot1b
# saves plot
ggsave("../../R_OUTPUT/PLOTS/TErec_TEfam.pdf", plot1b, width=5, height=5)

# The testing effect in recollection and the testing effect in familiarity,
# by delay and age group
plot2b = ggplot(CURRENT_DATA, aes(x = TE_REC, y = TE_FAM)) + 
  geom_point(color=c("#000000"), size=2) + 
  xlab("Testing Effect in Recollection") + 
  ylab("Testing Effect in Familiarity") + 
  facet_grid(delay~group) +
  scale_x_continuous(breaks=seq(-1, 1, .2)) + 
  scale_y_continuous(breaks=seq(-1, 1, .2)) +
  geom_smooth(method=lm, se=FALSE, color = "black", size=1) +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title.x = element_text(size = 18, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=18, colour = "black"), 
        axis.title.y = element_text(size = 18, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=18, colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, size = 2, linetype = "solid", colour = "black"),
        legend.title = element_text(size=18, colour = "black"),
        legend.text = element_text(size=18, colour = "black"),
        legend.position = c(0.115, 0.91),
        panel.spacing = unit(-.13, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size=18, colour = "black"))
plot2b
# saves plot
ggsave("../../R_OUTPUT/PLOTS/TErec_TEfam_by_AgeGroup_and_Delay.pdf", plot2b, width=9, height=9)

####### Remember --> TE Know ######
# Remember responses and the testing effect in Know responses
plot3 = ggplot(CURRENT_DATA, aes(x = S2_PROP_HIT_R, y = TE_KNOW)) + 
  geom_point(color=c("#000000"), size=2) + 
  xlab("Raw Remember Hit Rate") + 
  ylab("Testing Effect in Raw Know Hit Rate") + 
  scale_x_continuous(breaks=seq(-1, 1, .2)) + 
  scale_y_continuous(breaks=seq(-1, 1, .2)) +
  geom_smooth(method=lm, se=FALSE, color = "black", size=1) +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, size = 2, linetype = "solid", colour = "black"),
        legend.title = element_text(size=12, colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.position = c(0.115, 0.91),
        panel.spacing = unit(-.13, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size=12, colour = "black"))
plot3
# saves plot
ggsave("../../R_OUTPUT/PLOTS/RawRemember_TERawKnow.pdf", plot3, width=5, height=5)

# Remember responses and the testing effect in Know responses, by delay and age group
plot4 = ggplot(CURRENT_DATA, aes(x = S2_PROP_HIT_R, y = TE_KNOW)) + 
  geom_point(color=c("#000000"), size=2) + 
  xlab("Raw Remember Hit Rate") + 
  ylab("Testing Effect in Raw Know Hit Rate") + 
  facet_grid(delay~group) +
  scale_x_continuous(breaks=seq(-1, 1, .2)) + 
  scale_y_continuous(breaks=seq(-1, 1, .2)) +
  geom_smooth(method=lm, se=FALSE, color = "black", size=1) +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title.x = element_text(size = 18, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=18, colour = "black"), 
        axis.title.y = element_text(size = 18, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=18, colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, size = 2, linetype = "solid", colour = "black"),
        legend.title = element_text(size=18, colour = "black"),
        legend.text = element_text(size=18, colour = "black"),
        legend.position = c(0.115, 0.91),
        panel.spacing = unit(-.13, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size=18, colour = "black"))
plot4
# saves plot
ggsave("../../R_OUTPUT/PLOTS/RawRemember_TERawKnow_by_AgeGroup_and_Delay.pdf", plot4, width=9, height=9)

####### TE Remember --> TE Know ######
# The testing effect in Remember responses and the testing effect in Know responses
plot5 = ggplot(CURRENT_DATA, aes(x = TE_REMEMBER, y = TE_KNOW)) + 
  geom_point(color=c("#000000"), size=2) + 
  xlab("Testing Effect\nin Raw Remember Hit Rate") + 
  ylab("Testing Effect\nin Raw Know Hit Rate") + 
  scale_x_continuous(breaks=seq(-1, 1, .2)) + 
  scale_y_continuous(breaks=seq(-1, 1, .2)) +
  geom_smooth(method=lm, se=FALSE, color = "black", size=1) +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title.x = element_text(size = 18, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=18, colour = "black"), 
        axis.title.y = element_text(size = 18, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=18, colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, size = 2, linetype = "solid", colour = "black"),
        legend.title = element_text(size=18, colour = "black"),
        legend.text = element_text(size=18, colour = "black"),
        legend.position = c(0.115, 0.91),
        panel.spacing = unit(-.13, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size=18, colour = "black"))
plot5
# saves plot
ggsave("../../R_OUTPUT/PLOTS/TERawRemember_TERawKnow.pdf", plot5, width=5, height=5)

# The testing effect in Remember responses and the testing effect in Know responses
# by delay and age group
plot6 = ggplot(CURRENT_DATA, aes(x = TE_REMEMBER, y = TE_KNOW)) + 
  geom_point(color=c("#000000"), size=2) + 
  xlab("Testing Effect in Raw Remember Hit Rate") + 
  ylab("Testing Effect in Raw Know Hit Rate") + 
  facet_grid(delay~group) +
  scale_x_continuous(breaks=seq(-1, 1, .2)) + 
  scale_y_continuous(breaks=seq(-1, 1, .2)) +
  geom_smooth(method=lm, se=FALSE, color = "black", size=1) +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title.x = element_text(size = 18, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=18, colour = "black"), 
        axis.title.y = element_text(size = 18, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=18, colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, size = 2, linetype = "solid", colour = "black"),
        legend.title = element_text(size=18, colour = "black"),
        legend.text = element_text(size=18, colour = "black"),
        legend.position = c(0.115, 0.91),
        panel.spacing = unit(-.13, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size=18, colour = "black"))
plot6
# saves plot
ggsave("../../R_OUTPUT/PLOTS/TERawRemember_TERawKnow_by_AgeGroup_and_Delay.pdf", plot6, width=9, height=9)

####### Remember --> TE Know; Quadratic ######
# Remember responses and the testing effect in Know responses
# Quadratic relation
plot7 = ggplot(CURRENT_DATA, aes(x = S2_PROP_HIT_R, y = TE_KNOW)) + 
  geom_point(color=c("#000000"), size=2) + 
  xlab("Raw Remember Hit Rate") + 
  ylab("Testing Effect in Raw Know Hit Rate") + 
  scale_x_continuous(breaks=seq(-1, 1, .2)) + 
  scale_y_continuous(breaks=seq(-1, 1, .2)) +
  geom_smooth(method=lm, formula = y ~ x + I(x^2), se=FALSE, color = "black", size=1) +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, "cm"),
        axis.title.x = element_text(size = 18, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=18, colour = "black"), 
        axis.title.y = element_text(size = 18, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=18, colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA, size = 2, linetype = "solid", colour = "black"),
        legend.title = element_text(size=18, colour = "black"),
        legend.text = element_text(size=18, colour = "black"),
        legend.position = c(0.115, 0.91),
        panel.spacing = unit(-.13, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size=18, colour = "black"))
plot7
# saves plot
ggsave("../../R_OUTPUT/PLOTS/RawRemember_TERawKnow_Quadratic.pdf", plot7, width=5, height=5)

