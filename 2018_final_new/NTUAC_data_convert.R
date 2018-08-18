library(magrittr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)
library(plyr)
library(MASS)
library(tidyr)
library(dplyr)

# data preprocessing ####
setwd("d:/Data/Research_projeccts/NTUACdata/NTUAC_Analysis/2018_final_new/sub_list/")

File.list.NTUAC = mixedsort(list.files())
domain.total.sub  <- c()
# loop for subjects ####
for (subjn in 1:length(File.list.NTUAC)){

  NTUAC.1 <- read.csv(File.list.NTUAC[subjn]) 
  NTUAC.1 <- NTUAC.1[-c(1),-c(25)]
  NTUAC.1 <- sapply(NTUAC.1, as.character)
  NTUAC.1 <- as.data.frame(sub("%","", NTUAC.1))
  
  for (i in 2:ncol(NTUAC.1)){
    NTUAC.1[,i] <- as.numeric(levels(NTUAC.1[,i]))[NTUAC.1[,i]]
  }
  NTUAC.1 <- NTUAC.1[NTUAC.1$X != "",]
  
  # data separating into 4 tasks ####
  nrow(NTUAC.1)
  SWI <- NTUAC.1[1:30,]
  INH <- NTUAC.1[31:66,]
  NB <- NTUAC.1[67:90,]
  MEM <- NTUAC.1[91:93,]
  
  task.list <- list(SWI, INH, NB)
  length(task.list)
  sesssionsnum <- c(6,6,4)
  taskname <- c("Switching", "Inhibition", "Nback")
  domain.total <- c()
  
  # loop for tasks ####
  for (domain in 1:length(task.list)){
    temp.domain <- task.list[[domain]]
    
    temp.domain.total <- temp.domain[seq(0,nrow(temp.domain), by = sesssionsnum[domain]),]
    temp.domain.total <- temp.domain.total[temp.domain.total!= "NA",]
    temp.domain.total <- na.omit(gather(temp.domain.total, key = X, value = value))
    temp.domain.total$X <- as.numeric(temp.domain.total$X)
    Task <- rep(taskname[domain], nrow(temp.domain.total))
    trainning_times <- 1:nrow(temp.domain.total)
    colnames(temp.domain.total) <- c("Level", "sessions", "ACC")
    temp.domain.total$sessions <- as.numeric(sub("session","", temp.domain.total$sessions))
    temp.domain.total <- cbind(Task, temp.domain.total, trainning_times) 
    temp.domain.total$Level <- dense_rank(temp.domain.total$Level) 
    
    domain.total <- rbind(domain.total, temp.domain.total)
  }
  Subj.num <- rep(sub(".csv","",File.list.NTUAC[subjn]), nrow(domain.total))
  domain.total <- cbind(domain.total, Subj.num)
  domain.total.sub <- rbind(domain.total.sub, domain.total)
}
View(domain.total.sub)

NTUAC.pre <- read.csv("../ALL_3_4_5±èV3.csv")
NTUAC.pre <- NTUAC.pre[,-c(6:27)]

NTUAC.pre <- na.omit(NTUAC.pre)

seq.table <- table(NTUAC.pre$Task, NTUAC.pre$Subj.num)

trainning_times <- c()
for (i in 1:ncol(seq.table)){
  for ( j in c(2,3,1)){
    trainning_times <- c(trainning_times,seq(1,seq.table[j,i]))
  }
}
NTUAC.pre$trainning_times <- trainning_times
NTUAC.pre <- NTUAC.pre[, c("Task", "Level", "sessions", "ACC", "trainning_times", "Subj.num")]
NTUAC.pre$ACC <- NTUAC.pre$ACC*100

domain.total.sub <- rbind(domain.total.sub, NTUAC.pre)

write.csv(domain.total.sub, file = "../NTUAC_total_34567.csv")
