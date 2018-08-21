setwd("d:/Data/Research_projeccts/NTUACdata/NTUAC_Analysis/2018_final_new/")
NTUAC.final <- read.csv("NTUAC_total_34567_final.csv")

NTUAC.final$trainning_times_level <- sequence(rle(as.character(NTUAC.final$Level))$lengths)
NTUAC.final$Level <- as.factor(NTUAC.final$Level)

NTUAC.final <- NTUAC.final[NTUAC.final$Level != 6,]

NTUAC.final$Level
str(NTUAC.final)
NTUAC.model <- lmer(ACC ~ Task*trainning_times_level*Level*Group + (1+trainning_times_level|Subj.num),data = NTUAC.final)
summary(NTUAC.model)

table(NTUAC.final$)