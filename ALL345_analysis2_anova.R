library(lme4)

NTUAC2.all <- read.csv('ALL_3_4_5±èV2.csv')
NTUAC2.data <- data.frame(NTUAC2.all)

NTUAC.all <- read.csv('ALL_3_4_5±è.csv')
NTUAC.data <- data.frame(NTUAC.all)

NTUAC2.data$Session <- as.factor(NTUAC2.data$Session)
NTUAC2.data$Task <- as.factor(NTUAC2.data$Task)
NTUAC2.data$Subject <- as.factor(NTUAC2.data$Subject)
NTUAC2.data$Tag <- as.factor(NTUAC2.data$Tag)

NTUAC.data$Session <- as.factor(NTUAC.data$Session)
NTUAC.data$Task <- as.factor(NTUAC.data$Task)

Df.NB <- subset(NTUAC2.data, Task=="NB") 
Df.I <- subset(NTUAC2.data, Task=="I") 
Df.S <- subset(NTUAC2.data, Task=="S")

attach(NTUAC2.data)

Nor.DfNB <- Df.NB$ACC - mean(na.rm=TRUE, Df.NB$ACC)
Nor.DfS <- Df.S$ACC - mean(na.rm=TRUE, Df.S$ACC)
Nor.DfI <- Df.I$ACC - mean(na.rm=TRUE, Df.I$ACC)
Nor.ACC <- ACC - mean(na.rm=TRUE, ACC)

R.1 <- lmer(ACC ~ Tag + (1|Subject) + 0, data=NTUAC2.data) 
summary(R.1)

R.2 <- lmer(ACC ~ Tag + (1|Subject) + 0, data=Df.NB) 
summary(R.2)

R.2 <- lmer(ACC ~ Tag + (1|Subject) + 0, data=Df.I) 
summary(R.2)

R.2 <- lmer(ACC ~ Tag + (1|Subject) + 0, data=Df.S) 
summary(R.2)

M.NB <- lm(Nor.DfNB ~ Df.NB$Subject)
summary(lm(Nor.DfNB ~ Df.NB$Subject+0))

M.S <- lm(Nor.DfS ~ Df.S$Subject)
summary(lm(Nor.DfS ~ Df.S$Subject+0))

M.I <- lm(Nor.DfI ~ Df.I$Subject)
summary(lm(Nor.DfI ~ Df.I$Subject+0))

M.ACC <- lm(Nor.ACC ~ Task)
summary(lm(Nor.ACC ~ Task+0))

# NTUAC.lm<- lm(ACC ~ Task*Subject)
# 
# summary(lm(ACC ~ Task*Subject))
# 
# model.AS <- aov(ACC ~ Subject)
# summary(model.AS)
# 
# model.AT <- aov(ACC ~ Task)
# summary(model.AT)
# 
# model.NTUAC <- aov(ACC ~ Task * Subject)
# summary(model.NTUAC)