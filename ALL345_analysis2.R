setwd("D:/Data/Research_projeccts/NTUACdata/NTUAC_Analysis")
NTUAC2.all <- read.csv('ALL_3_4_5±èV2.csv')

NTUAC2.data <- data.frame(NTUAC2.all)

NTUAC2.data$Session <- as.factor(NTUAC2.data$Session)
NTUAC2.data$Task <- as.factor(NTUAC2.data$Task)
NTUAC2.data$Subject <- as.factor(NTUAC2.data$Subject)
# NTUAC2.data$Tag <- as.factor(NTUAC2.data$Tag)

NTUAC2.data.FThree <- NTUAC2.data[complete.cases(NTUAC2.data),]
NTUAC2.data.FThree <- NTUAC2.data.FThree[NTUAC2.data.FThree$Fir3==1,]
Df.NB <- subset(NTUAC2.data, Task=="NB") 
Df.I <- subset(NTUAC2.data, Task=="I") 
Df.S <- subset(NTUAC2.data, Task=="S")

library(ggplot2)
library(ggsignif)
library(plotly)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

NTUAC2.data.FThree[,8] <- sequence(rle(NTUAC2.data.FThree$Tag)$length)

# Boxplot
boxplot(NTUAC2.data$ACC ~ NTUAC2.data$Subject)
boxplot(NTUAC2.data$ACC ~ NTUAC2.data$Task )
boxplot(NTUAC2.data$ACC ~ NTUAC2.data$Task * NTUAC2.data$Subject)

boxplot(NTUAC2.data.FThree$ACC ~ NTUAC2.data.FThree$DIA + NTUAC2.data.FThree$Tag)
plot(NTUAC2.data.FThree$ACC ~ NTUAC2.data.FThree$DIA + NTUAC2.data.FThree$Tag)

tapply(NTUAC2.data.FThree$ACC, list(NTUAC2.data.FThree$DIA, NTUAC2.data.FThree$Task), mean, na.rm="TRUE")
tapply(NTUAC2.data$ACC, list(NTUAC2.data$DIA, NTUAC2.data$Tag, NTUAC2.data$Task), mean, na.rm="TRUE")

tapply(NTUAC2.data.FThree$ACC, list(NTUAC2.data.FThree$Task, NTUAC2.data.FThree$DIA, NTUAC2.data.FThree$Tag), mean, na.rm="TRUE")
tapply(NTUAC2.data.FThree$ACC, list(NTUAC2.data.FThree$Subject, NTUAC2.data.FThree$DIA), mean, na.rm="TRUE")
tapply(NTUAC2.data.FThree$ACC, list(NTUAC2.data.FThree$Subject, NTUAC2.data.FThree$DIA), sd, na.rm="TRUE")

## linear model
K <- lm(NTUAC2.data.FThree$ACC ~ NTUAC2.data.FThree$Subject * NTUAC2.data.FThree$Tag * NTUAC2.data.FThree$Task)
summary(K)

M <- lm(NTUAC2.data.FThree$ACC ~ NTUAC2.data.FThree$DIA * NTUAC2.data.FThree$Tag * NTUAC2.data.FThree$Task)
summary(M)

R <- lm(NTUAC2.data.FThree$ACC ~ NTUAC2.data.FThree$DIA * NTUAC2.data.FThree$Task * NTUAC2.data.FThree$Tag * NTUAC2.data.FThree$times)
summary(R)

# ggplot line

png(sprintf("The accuracy of each subjects_Nback task.png"), width = 1800, height = 700)
NB.sub <- ggplot(na.omit(Df.NB), aes(x=Session, y=ACC, group=interaction(Subject, Task), colour = Subject)) +
          # geom_line() +
          # geom_point(data = transform(na.omit(Df.NB), class = NULL), colour = "grey85", size = 4) +
          geom_point() +
          labs(title = "The accuracy of each subjects in n-back task") +
          theme(plot.title = element_text(hjust = 0.5, size= 30),
                title = element_text(size=10),
                legend.text = element_text(size=20),
                legend.title = element_text(size=20),
                axis.text = element_text(size=20),
                axis.title = element_text(size=20,face="bold")) + 
          facet_wrap(~Tag, ncol = 2, nrow = 4)
NB.sub
dev.off()

png(sprintf("The accuracy of each subjects_Inhibition task.png"), width = 1800, height = 700)
I.sub <- ggplot(na.omit(Df.I), aes(x=Session, y=ACC, group=interaction(Subject, Task), color = Subject)) +
         geom_line() +
         geom_point(size = 4) +
         labs(title = "The accuracy of each subjects in Inhibition task") +
         theme(plot.title = element_text(hjust = 0.5, size= 30),
              title = element_text(size=10),
              legend.text = element_text(size=20),
              legend.title = element_text(size=20),
              axis.text = element_text(size=20),
              axis.title = element_text(size=20,face="bold")) +
              facet_wrap(~Tag, ncol = 2, nrow = 4)
I.sub
dev.off()

png(sprintf("The accuracy of each subjects_Switching task.png"), width = 1800, height = 700)
S.sub <- ggplot(na.omit(Df.S), aes(x=Session, y=ACC, group=interaction(Subject, Task), color = Subject)) +
         geom_line() +
         geom_point(size = 4) +
         labs(title = "The accuracy of each subjects in Switching task") +
         theme(plot.title = element_text(hjust = 0.5, size= 30),
              title = element_text(size=10),
              legend.text = element_text(size=20),
              legend.title = element_text(size=20),
              axis.text = element_text(size=20),
              axis.title = element_text(size=20,face="bold")) +
              facet_wrap(~Tag, ncol = 2, nrow = 4)
S.sub
dev.off()

# ggplot Boxplot
png(sprintf("The accuracy of each subjects.png"), width = 1800, height = 700)
Sub.ACC <- ggplot(NTUAC2.data, aes(x=Subject, y=ACC)) + 
           geom_boxplot(aes(colour = Subject)) +
           labs(title = "The accuracy of each subjects") +
           theme(plot.title = element_text(hjust = 0.5, size= 30),
                  title = element_text(size=10),
                  legend.text = element_text(size=20),
                  legend.title = element_text(size=20),
                  axis.text = element_text(size=20),
                  axis.title = element_text(size=20,face="bold"))
Sub.ACC
dev.off()

png(sprintf("The accuracy of each task.png"), width = 1800, height = 700)
Task.ACC <- ggplot(NTUAC2.data, aes(x=Task, y=ACC)) + 
            geom_boxplot(aes(colour = Task)) +
            labs(title = "The accuracy of each task") +
            theme(plot.title = element_text(hjust = 0.5, size= 30),
                  title = element_text(size=10),
                  legend.text = element_text(size=20),
                  legend.title = element_text(size=20),
                  axis.text = element_text(size=20),
                  axis.title = element_text(size=20,face="bold"))
Task.ACC
dev.off()

png(sprintf("The accuracy of each level in each task.png"), width = 1800, height = 700)
Tag.ACC <- ggplot(NTUAC2.data, aes(x=Tag, y=ACC)) + 
           geom_boxplot(aes(colour = Task)) +
           labs(title = "The accuracy of each level in each task") +
           theme(plot.title = element_text(hjust = 0.5, size= 30),
                  title = element_text(size=10),
                  legend.text = element_text(size=20),
                  legend.title = element_text(size=20),
                  axis.text = element_text(size=20),
                  axis.title = element_text(size=20,face="bold"))
Tag.ACC
dev.off()

png(sprintf("The accuracy of each subject in each task.png"), width = 1800, height = 700)
SubACC.Task <- ggplot(NTUAC2.data, aes(Subject, ACC)) + 
               geom_boxplot(aes(colour = Task)) +
               labs(title = "The accuracy of each subject in each task") +
               theme(plot.title = element_text(hjust = 0.5, size= 30),
                      title = element_text(size=10),
                      legend.text = element_text(size=20),
                      legend.title = element_text(size=20),
                      axis.text = element_text(size=20),
                      axis.title = element_text(size=20,face="bold"))
print(SubACC.Task)
dev.off()

png(sprintf("The accuracy of each task in each subject.png"), width = 1800, height = 700)
TaskACC.Subject <- ggplot(NTUAC2.data, aes(Task, ACC)) + 
                   geom_boxplot(aes(colour = Subject)) +
                   labs(title = "The accuracy of each task in each subject") +
                   theme(plot.title = element_text(hjust = 0.5, size= 30),
                            title = element_text(size=10),
                            legend.text = element_text(size=20),
                            legend.title = element_text(size=20),
                            axis.text = element_text(size=20),
                            axis.title = element_text(size=20,face="bold"))
print(TaskACC.Subject)
dev.off()


ggplot(NTUAC2.data.FThree, aes(x=Tag, y=ACC, group=DIA, color=DIA)) + 
        geom_line() + 
        geom_errorbar(aes(ymin=ACC-sd, ymax=ACC+sd), width=.1) +
        theme_classic() +
        scale_color_manual(values=c('#999999','#E69F00'))
