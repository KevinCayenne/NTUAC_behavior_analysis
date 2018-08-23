library(Matrix)
library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)
library(lme4)
library(tidyr)
library(dplyr)
library(data.table)
library(reshape2)
library(plyr)

setwd("d:/Data/Research_projeccts/NTUACdata/NTUAC_Analysis/2018_final_new/")
NTUAC.final <- read.csv("NTUAC_total_34567_final.csv")

NTUAC.final$trainning_times_level <- sequence(rle(as.character(NTUAC.final$Level))$lengths)

NTUAC.final <- NTUAC.final[NTUAC.final$Level != 6,]
NTUAC.final$Level
str(NTUAC.final)

NTUAC.model <- lmer(ACC ~ Task*trainning_times_level*poly(Level, 2)*Group + (1+Level+trainning_times_level|Subj.num), data = NTUAC.final)
NTUAC.model.summary <- summary(NTUAC.model)
NTUAC.model.anova <- anova(NTUAC.model)
NTUAC.model.summary
NTUAC.model.anova

boxplot(NTUAC.final$ACC ~ NTUAC.final$Task*NTUAC.final$Level*NTUAC.final$Group)

NTUAC.model.nopoly <- lmer(ACC ~ Task*trainning_times_level*Level*Group + (1+Level+trainning_times_level|Subj.num), data = NTUAC.final)
NTUAC.model.nopoly.summary <- summary(NTUAC.model.nopoly)
NTUAC.model.nopoly.summary

NTUAC.notask.model <- lmer(ACC ~ poly(trainning_times_level, 2)*Level*Group + (1+Level+trainning_times_level|Subj.num), data = NTUAC.final)
summary(NTUAC.notask.model)
anova(NTUAC.notask.model)

NTUAC.final.Inhibition <- NTUAC.final[NTUAC.final$Task == "Inhibition",]
NTUAC.final.NBack <- NTUAC.final[NTUAC.final$Task == "NBack",]
NTUAC.final.Switching <- NTUAC.final[NTUAC.final$Task == "Switching",]

NTUAC.model.Inhibition <- lmer(ACC ~ Level*Group*trainning_times_level + (1+Level+trainning_times_level|Subj.num), data = NTUAC.final.Inhibition)
NTUAC.model.Inhibition.sum <- summary(NTUAC.model.Inhibition)
NTUAC.model.Inhibition.sum
plot(ACC ~ Level*Group*trainning_times_level, NTUAC.final.Inhibition)

NTUAC.model.NBack <- lmer(ACC ~ Level*Group*trainning_times_level + (1+Level+trainning_times_level|Subj.num), data = NTUAC.final.NBack)
NTUAC.model.NBack.sum <- summary(NTUAC.model.NBack)
NTUAC.model.NBack.sum
plot(ACC ~ Level*Group*trainning_times_level, NTUAC.final.NBack)

NTUAC.model.Switching <- lmer(ACC ~ Level*Group*trainning_times_level + (1+Level+trainning_times_level|Subj.num), data = NTUAC.final.Switching)
NTUAC.model.Switching.sum <- summary(NTUAC.model.Switching)
NTUAC.model.Switching.sum
plot(ACC ~ Level*Group*trainning_times_level, NTUAC.final.Switching)

ggline(NTUAC.final.Inhibition, x = "Level", y = "ACC", add = "mean_se", color = "Group") +
       labs(x = "Level", y = "ACC", colour = "Group") + 
       stat_compare_means(aes(group = Group), label = "p.signif", 
                         label.y = 110, size = 5)

ggline(NTUAC.final.NBack, x = "Level", y = "ACC", add = "mean_se", color = "Group") +
       labs(x = "Level", y = "ACC", colour = "Group") + 
        stat_compare_means(aes(group = Group), label = "p.signif", 
                     label.y = 110, size = 5)

ggline(NTUAC.final.Switching, x = "Level", y = "ACC", add = "mean_se", color = "Group") +
       labs(x = "Level", y = "ACC", colour = "Group") + 
        stat_compare_means(aes(group = Group), label = "p.signif", 
                     label.y = 110, size = 5)


ggline(NTUAC.final.Inhibition, x = "trainning_times_level", y = "ACC", add = "mean_se", color = "Group", facet.by = "Level") +
  labs(x = "training times", y = "ACC", colour = "Group", title = "Inhibition") + facet_wrap(~ Level, ncol=5) +
  stat_compare_means(aes(group = Group), label = "p.signif", 
                     label.y = 110, size = 5) +
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = Group), size=1)

ggline(NTUAC.final.NBack, x = "trainning_times_level", y = "ACC", add = "mean_se", color = "Group", facet.by = "Level") +
  labs(x = "training times", y = "ACC", colour = "Group", title = "NBack") + facet_wrap(~ Level, ncol=5) +
  stat_compare_means(aes(group = Group), label = "p.signif", 
                     label.y = 110, size = 5) +
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = Group), size=1)

ggline(NTUAC.final.Switching, x = "trainning_times_level", y = "ACC", add = "mean_se", color = "Group", facet.by = "Level") +
  labs(x = "training times", y = "ACC", colour = "Group", title = "Switching") + facet_wrap(~ Level, ncol=5) +
  stat_compare_means(aes(group = Group), label = "p.signif", 
                     label.y = 110, size = 5) +
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = Group), size=1)
