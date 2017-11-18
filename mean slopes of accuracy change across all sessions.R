setwd("D:/Data/Research_projeccts/NTUACdata/NTUAC_Analysis")
NTUAC2.all <- read.csv('ALL_3_4_5±èV2.csv')

library(ggplot2)
library(ggsignif)
library(wesanderson)
library(magrittr)
library(ggpubr)
library(dplyr)

NTUAC2.data <- data.frame(NTUAC2.all)

#NTUAC2.data$Session <- as.factor(NTUAC2.data$Session)
NTUAC2.data$Task <- as.factor(NTUAC2.data$Task)
NTUAC2.data$DIA_N <- as.factor(NTUAC2.data$DIA_N)

NTUAC2.data.FThreeA <- NTUAC2.data[complete.cases(NTUAC2.data),]
NTUAC2.data.FThree <- NTUAC2.data.FThreeA[NTUAC2.data.FThreeA$Fir3==1,]
Df.NB <- subset(NTUAC2.data, Task=="NB") 
Df.I <- subset(NTUAC2.data, Task=="I") 
Df.S <- subset(NTUAC2.data, Task=="S")

NTUAC2.data.FThree[,9] <- sequence(rle(NTUAC2.data.FThree$Tag)$length)
NTUAC2.data.FThree[,9]
NTUAC2.data$Subject <- as.factor(NTUAC2.data$Subject)
colnames(NTUAC2.data.FThree)[9] <- "times"
NTUAC2.data.FThree$Tag <- as.factor(NTUAC2.data.FThree$Tag)
#NTUAC2.data.FThree$times <- as.factor(NTUAC2.data.FThree$times)
#NTUAC2.data.FThree$Session <- as.factor(NTUAC2.data.FThree$Session)

####

tapply(NTUAC2.data.FThree$Subject)

B.data <- c()
BI.data <- c()
BNB.data <- c()
BS.data <- c()

groupNum <- 5
taskName <- c("I", "NB","S")
groupName <- c("CN", "SCD", "MCI")

R.data.total <- c()
Sub.tag <- c()
Dif.tag <- c()
Dia.tag <- c()
Task.tag <- c()
u = 0

GroupACC <- split(NTUAC2.data.FThree, NTUAC2.data.FThree$Subject)

for (i in 1:length(GroupACC)){
  SS <- GroupACC[[i]]
  ST <- split(SS, SS$Task)
  for (j in 1:length(ST)){
      STT <- ST[[j]]
      STTD <- split(STT, STT$Tag)
      for (k in 1:(length(STTD)-1)){
          STTDD <- STTD[[k]]
          if (is.na(STTDD$Session[1])==FALSE){
            RE <- summary(lm(STTDD$ACC ~ STTDD$times))
            dataR <- as.data.frame(RE$coefficients)
            R.data.total <- c(R.data.total, dataR$Estimate[2])
            Sub.tag <- c(Sub.tag, as.character(STTDD$Subject[1]))
            Dif.tag <- c(Dif.tag, as.character(STTDD$Tag[1]))
            Dia.tag <- c(Dia.tag, as.character(STTDD$DIA_N[1]))
            Task.tag <- c(Task.tag, as.character(STTDD$Task[1]))
            u = u+1
          }
      }
  }
}

col.name <- c("Task.tag", "Sub.tag", "Dif.tag", "Dia.tag")
Total.df <- data.frame(R.data.total, Task.tag, Sub.tag, Dif.tag, Dia.tag)
Total.df <- na.omit(Total.df)
Total.df[col.name] <- lapply(Total.df[col.name], factor)

ggline(Total.df, x = "Dif.tag", y = "R.data.total", add = "mean_se",
       color = "Dia.tag", palette = "jco", facet.by = "Task.tag") +
  labs(title = "Group difference in slope of accuarcy changing in each levels", x = "Difficulty levels", y = "Slope of accuracy", fill = "DIAGNOSE") +
  theme(plot.title = element_text(hjust = 0.5, size= 15)) +
  stat_compare_means(aes(group = Dia.tag), label = "p.signif", 
                     label.y = 0.13)

kk <- lm(Total.df$R.data.total ~ Total.df$Dia.tag * Total.df$Task.tag * Total.df$Dif.tag)
summary(kk)

#######

for (Gname in groupName){
  for (i in (1:groupNum)){
    GroupACC <- NTUAC2.data.FThree[NTUAC2.data.FThree$Tag==i&NTUAC2.data.FThree$DIA_N==Gname,]
    GroupACC.S <- split(GroupACC, GroupACC$Task)
    
    do.this <- c("I","NB","S")
    
    for (do in do.this){
    
    switch(do,
      I = {
        IR <- summary(lm(GroupACC.S$I$ACC ~ GroupACC.S$I$times))
        dataI <- as.data.frame(IR$coefficients)
        BI.data <- c(BI.data, dataI$Estimate[2])
      },
      NB = {
        NBR <- summary(lm(GroupACC.S$NB$ACC ~ GroupACC.S$NB$times))
        dataNB <- as.data.frame(NBR$coefficients)
        BNB.data <- c(BNB.data, dataNB$Estimate[2])
      },
      S = {
        SR <- summary(lm(GroupACC.S$S$ACC ~ GroupACC.S$S$times))
        dataS <- as.data.frame(SR$coefficients)
        BS.data <- c(BS.data, dataS$Estimate[2])
      }
    )
    }
    }
}

barplot(BI.data)
barplot(BNB.data)
barplot(BS.data)

group.Tag <- rep(rep(c("CN","SCD","MCI"),c(5,5,5)), 3)
dif.Tag <- rep(rep(c(1:5),3), 3)
mean.slope <-  c(BI.data, BNB.data, BS.data)
Task.Tag <- rep(c("I", "NB", "S"), c(15,15,15))

M.slope.R <- data.frame(mean.slope, Task.Tag, dif.Tag, group.Tag)

M.slope.R$dif.Tag <- as.factor(M.slope.R$dif.Tag)
M.slope.R$Task.Tag <- as.factor(M.slope.R$Task.Tag)
M.slope.R$group.Tag <- as.factor(M.slope.R$group.Tag)

head(M.slope.R)

ggplot(data=M.slope.R, aes(y=mean.slope))+
  geom_bar(aes(x=group.Tag, group=dif.Tag,  fill=group.Tag), stat="identity", position=position_dodge(1)) +
  facet_grid(~Task.Tag, scale='free_x')

ggplot(data=M.slope.R, aes(y=mean.slope))+
      geom_bar(aes(x=dif.Tag, group=group.Tag,  fill=group.Tag), stat="identity", position=position_dodge(1)) +
      facet_grid(~Task.Tag, scale='free_x') +
      geom_smooth(aes(x = dif.Tag, group=group.Tag, colour=group.Tag, fill=group.Tag), method="glm", position=position_dodge(1))
a69111833

ggline(M.slope.R, x = "dif.Tag", y = "mean.slope", add = "mean_se",
       color = "group.Tag", palette = "jco", facet.by = "Task.Tag") +
  labs(title = "Group difference in each levels", x = "Difficulty levels", y = "Accuracy", fill = "DIAGNOSE") +
  theme(plot.title = element_text(hjust = 0.5, size= 15))

ggplot(data=M.slope.R, aes(x=dif.Tag, y=mean.slope))+
  geom_bar(aes(group=dif.Tag,  fill=group.Tag), stat="identity", position=position_dodge(1)) +
  facet_grid(group.Tag~Task.Tag, scale='free_x') +
  geom_smooth(aes(group=group.Tag), method="lm")

m <- lm(M.slope.R$mean.slope ~ M.slope.R$Task.Tag * M.slope.R$group.Tag * M.slope.R$dif.Tag)
summary(m)
