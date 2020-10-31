##### SCRIPT INFO #####

# This script loads summary data and creates visualizations of 
# the testing effect in Dual-Process parameter estimates.
# Bar graphs: With and without individual data points overlain

# Author: Ruth A. Shaffer

##### SET WD TO FILE PATH ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### LOAD PACKAGES ####
library("ggplot2")
library("dplyr")
library("tidyr")

##################### IMPORT DATA ##################### 

##### IMPORT SUMMARY DATA ####
SUBJECT_LEVEL_ESTIMATES = as.data.frame(read.csv("../../DATA/R_SUMMARY_DATA/SUBJECT_LEVEL_ESTIMATES.csv",header=TRUE,sep=","))

##### VARIABLES: SUMMARY DATA ####
SUBJECT_LEVEL_ESTIMATES

##################### CREATE SESSION 2 GRAPH ##################### 

##### ORGANIZE DATA FOR PLOTTING ####
# data for working with
SESSION2_DATA = SUBJECT_LEVEL_ESTIMATES

# variables as factors
SESSION2_DATA = SESSION2_DATA %>% 
  mutate(subid = factor(subid),
         group = factor(group, levels = c("younger", "older"), labels = c("Younger", "Older")),
         delay = factor(delay, levels = c("delay0", "delay1"), labels = c("No Delay", "1 Day Delay")))

# data into long format
SESSION2_DATA_LONG = 
  gather(SESSION2_DATA, 
         temp_name, 
         estimate, 
         c("S2_REC_WITH_FA_TEST","S2_REC_WITH_FA_NOTEST","S2_FAM_WITH_FA_TEST","S2_FAM_WITH_FA_NOTEST"), 
         factor_key=TRUE) %>% 
  separate(temp_name, 
           sep = "_WITH_FA_",
           c("parameter", "prior_condition"))

# refactor variables
SESSION2_DATA_LONG = SESSION2_DATA_LONG %>% 
  mutate(parameter = factor(parameter, levels = c("S2_REC", "S2_FAM"), labels = c("Recollection", "Familiarity")),
         prior_condition = factor(prior_condition, levels = c("TEST", "NOTEST"), labels = c("Test", "No Test")))

# get summary stats (mean / SE) for graphing
SESSION2_DATA_LONG_SUMMARY = SESSION2_DATA_LONG %>% 
  group_by(group,delay,parameter,prior_condition) %>% 
  summarise(MEAN = mean(estimate,na.rm=TRUE),
            SE = sd(estimate,na.rm=TRUE)/sqrt(sum(!is.na(estimate))))

# add labels for plotting
SESSION2_DATA_LONG_SUMMARY$label = ifelse(
  (SESSION2_DATA_LONG_SUMMARY$group == "Younger" & SESSION2_DATA_LONG_SUMMARY$parameter == "Recollection"), "A", 
  ifelse(
    (SESSION2_DATA_LONG_SUMMARY$group == "Younger" & SESSION2_DATA_LONG_SUMMARY$parameter == "Familiarity"), "C",
    ifelse(
      (SESSION2_DATA_LONG_SUMMARY$group == "Older" & SESSION2_DATA_LONG_SUMMARY$parameter == "Recollection"), "B",
      "D"
    )
  )
)

##### GRAPH: RECOLLECTION AND FAMILIARITY WITH INDIVIDUAL DATA POINTS #####

SESSION2_REC_FAM_WITH_FA_WITH_DATA_POINTS =
  ggplot(SESSION2_DATA_LONG_SUMMARY, 
         aes(x = delay,
             y = MEAN, 
             group = prior_condition, 
             fill = prior_condition)) +
  facet_grid(parameter ~ group)+
  geom_bar(position = position_dodge(), 
           stat = "identity", 
           alpha = .5,
           colour="black",
           size=1)+
    geom_text(
      size    = 5,
      data    = SESSION2_DATA_LONG_SUMMARY,
      mapping = aes(x = Inf, y = Inf, label = label),
      hjust   = 1.5,
      vjust   = 1.5
    ) +
  geom_errorbar(aes(ymin=MEAN-SE, 
                    ymax=MEAN+SE),
                width=.2,# Width of the error bars
                size=1,
                position=position_dodge(.9)) +
  geom_point(data=SESSION2_DATA_LONG,
             aes(x = delay, 
                 y = estimate, 
                 group = prior_condition), 
             alpha = .4,
             position=position_jitterdodge(dodge.width=0.9,
                                           jitter.width = 0.4))+ 
   scale_y_continuous(breaks=seq(0,1,.2))+
  xlab("Delay") +
  ylab("Final Test Parameter Estimates") +
  scale_fill_manual(name="Learning Condition",
                    values=c("gray54","gray100")) + 
  guides(fill = guide_legend(override.aes = list(shape = NA))) + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_text(size=12, colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.position = c(1.17, .5),
        plot.margin=unit(c(.5,4,.5,.5),"cm"),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank())

##### SAVE GRAPH #####
ggsave("../../R_OUTPUT/PLOTS/SESSION2_REC_FAM_WITH_FA_WITH_DATA_POINTS.pdf", SESSION2_REC_FAM_WITH_FA_WITH_DATA_POINTS, width=8, height=9)

##### GRAPH: RECOLLECTION AND FAMILIARITY NO INDIVIDUAL DATA POINTS #####

SESSION2_REC_FAM_WITH_FA =
  ggplot(SESSION2_DATA_LONG_SUMMARY, 
         aes(x = delay,
             y = MEAN, 
             group = prior_condition, 
             fill = prior_condition)) +
  facet_grid(parameter ~ group)+
  geom_bar(position = position_dodge(), 
           stat = "identity", 
           alpha = .5,
           colour="black",
           size=1)+
  geom_text(
    size    = 5,
    data    = SESSION2_DATA_LONG_SUMMARY,
    mapping = aes(x = Inf, y = Inf, label = label),
    hjust   = 1.5,
    vjust   = 1.5
  ) +
  geom_errorbar(aes(ymin=MEAN-SE, 
                    ymax=MEAN+SE),
                width=.2,# Width of the error bars
                size=1,
                position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,.55),
                     expand = c(0,0)) +
  xlab("Delay") +
  ylab("Final Test Parameter Estimates") +
  scale_fill_manual(name="Learning Condition",
                    values=c("gray54","gray100")) + 
  guides(fill = guide_legend(override.aes = list(shape = NA))) + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_text(size=12, colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.position = c(1.17, .5),
        plot.margin=unit(c(.5,4,.5,.5),"cm"),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank())

##### SAVE GRAPH #####
ggsave("../../R_OUTPUT/PLOTS/SESSION2_REC_FAM_WITH_FA.pdf", SESSION2_REC_FAM_WITH_FA, width=8, height=9)


##### GRAPH: YOUNGER + NO INDIVIDUAL DATA POINTS #####

# select younger adult data
YOUNGER_FOR_PLOT = SESSION2_DATA_LONG_SUMMARY
YOUNGER_FOR_PLOT = subset(YOUNGER_FOR_PLOT,group=="Younger")

SESSION2_REC_FAM_WITH_FA_YOUNGER =
  ggplot(YOUNGER_FOR_PLOT, 
         aes(x = parameter,
             y = MEAN, 
             group = prior_condition, 
             fill = prior_condition)) +
  facet_grid(~delay)+
  geom_bar(position = position_dodge(), 
           stat = "identity", 
           alpha = .5,
           colour="black",
           size=1)+
  geom_errorbar(aes(ymin=MEAN-SE, 
                    ymax=MEAN+SE),
                width=.2,# Width of the error bars
                size=1,
                position=position_dodge(.9)) +
    scale_y_continuous(limits = c(0,.55),
                       expand = c(0,0)) +
  xlab("Parameter") +
  ylab("Final Test Parameter Estimates") +
  scale_fill_manual(name="Learning Condition",
                    values=c("gray54","gray100")) + 
  guides(fill = guide_legend(override.aes = list(shape = NA))) + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_text(size=12, colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.position = c(1.12, .5),
        plot.margin=unit(c(.5,4,.5,.5),"cm"),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank())

##### SAVE GRAPH #####
ggsave("../../R_OUTPUT/PLOTS/SESSION2_REC_FAM_WITH_FA_YOUNGER.pdf", SESSION2_REC_FAM_WITH_FA_YOUNGER, width=9, height=5.5)

##### GRAPH: OLDER + NO INDIVIDUAL DATA POINTS #####

# select older adult data
OLDER_FOR_PLOT = SESSION2_DATA_LONG_SUMMARY
OLDER_FOR_PLOT = subset(OLDER_FOR_PLOT,group=="Older")

SESSION2_REC_FAM_WITH_FA_OLDER =
  ggplot(OLDER_FOR_PLOT, 
         aes(x = parameter,
             y = MEAN, 
             group = prior_condition, 
             fill = prior_condition)) +
  facet_grid(~delay)+
  geom_bar(position = position_dodge(), 
           stat = "identity", 
           alpha = .5,
           colour="black",
           size=1)+
  geom_errorbar(aes(ymin=MEAN-SE, 
                    ymax=MEAN+SE),
                width=.2,# Width of the error bars
                size=1,
                position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,.55),
                     expand = c(0,0)) +
  xlab("Parameter") +
  ylab("Final Test Parameter Estimates") +
  scale_fill_manual(name="Learning Condition",
                    values=c("gray54","gray100")) + 
  guides(fill = guide_legend(override.aes = list(shape = NA))) + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_text(size=12, colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.position = c(1.12, .5),
        plot.margin=unit(c(.5,4,.5,.5),"cm"),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank())

##### SAVE GRAPH #####
ggsave("../../R_OUTPUT/PLOTS/SESSION2_REC_FAM_WITH_FA_OLDER.pdf", SESSION2_REC_FAM_WITH_FA_OLDER, width=9, height=5.5)

##### GRAPH: RECOLLECTION + NO INDIVIDUAL DATA POINTS #####

# select recollection data
RECOLLECTION_FOR_PLOT = SESSION2_DATA_LONG_SUMMARY
RECOLLECTION_FOR_PLOT = subset(RECOLLECTION_FOR_PLOT,parameter=="Recollection")

SESSION2_REC_FAM_WITH_FA_RECOLLECTION_ONLY =
  ggplot(RECOLLECTION_FOR_PLOT, 
         aes(x = group,
             y = MEAN, 
             group = prior_condition, 
             fill = prior_condition)) +
  facet_grid(~delay)+
  geom_bar(position = position_dodge(), 
           stat = "identity", 
           alpha = .5,
           colour="black",
           size=1)+
  geom_errorbar(aes(ymin=MEAN-SE, 
                    ymax=MEAN+SE),
                width=.2,# Width of the error bars
                size=1,
                position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,.55),
                     expand = c(0,0)) +
  xlab("Age Group") +
  ylab("Final Test Parameter Estimates") +
  scale_fill_manual(name="Learning Condition",
                    values=c("gray54","gray100")) + 
  guides(fill = guide_legend(override.aes = list(shape = NA))) + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_text(size=12, colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.position = c(1.12, .5),
        plot.margin=unit(c(.5,4,.5,.5),"cm"),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank())

##### SAVE GRAPH #####
ggsave("../../R_OUTPUT/PLOTS/SESSION2_REC_FAM_WITH_FA_RECOLLECTION_ONLY.pdf", SESSION2_REC_FAM_WITH_FA_RECOLLECTION_ONLY, width=9, height=5.5)

##### GRAPH: FAMILIARITY + NO INDIVIDUAL DATA POINTS #####

# select familiarity data
FAMILIARITY_FOR_PLOT = SESSION2_DATA_LONG_SUMMARY
FAMILIARITY_FOR_PLOT = subset(FAMILIARITY_FOR_PLOT,parameter=="Familiarity")

SESSION2_REC_FAM_WITH_FA_FAMILIARITY_ONLY =
  ggplot(FAMILIARITY_FOR_PLOT, 
         aes(x = group,
             y = MEAN, 
             group = prior_condition, 
             fill = prior_condition)) +
  facet_grid(~delay)+
  geom_bar(position = position_dodge(), 
           stat = "identity", 
           alpha = .5,
           colour="black",
           size=1)+
  geom_errorbar(aes(ymin=MEAN-SE, 
                    ymax=MEAN+SE),
                width=.2,# Width of the error bars
                size=1,
                position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,.55),
                     expand = c(0,0)) +
  xlab("Age Group") +
  ylab("Final Test Parameter Estimates") +
  scale_fill_manual(name="Learning Condition",
                    values=c("gray54","gray100")) + 
  guides(fill = guide_legend(override.aes = list(shape = NA))) + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15), colour = "black"), 
        axis.text.x = element_text(size=12, colour = "black"), 
        axis.title.y = element_text(size = 12, margin = margin(r = 15), colour = "black"), 
        axis.text.y = element_text(size=12, colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        panel.border = element_rect(size = 2, linetype = "solid", colour = "black"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_text(size=12, colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.position = c(1.12, .5),
        plot.margin=unit(c(.5,4,.5,.5),"cm"),
        strip.text = element_text(size = 15,colour = "black"),
        strip.background = element_blank())

##### SAVE GRAPH #####
ggsave("../../R_OUTPUT/PLOTS/SESSION2_REC_FAM_WITH_FA_FAMILIARITY_ONLY.pdf", SESSION2_REC_FAM_WITH_FA_FAMILIARITY_ONLY, width=9, height=5.5)
