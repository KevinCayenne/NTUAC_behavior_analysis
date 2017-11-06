NTUAC2.all <- read.csv('ALL_3_4_5±èV2.csv')

NTUAC2.data <- data.frame(NTUAC2.all)

NTUAC2.data$Session <- as.factor(NTUAC2.data$Session)
NTUAC2.data$Task <- as.factor(NTUAC2.data$Task)
NTUAC2.data$Subject <- as.factor(NTUAC2.data$Subject)
NTUAC2.data$Tag <- as.factor(NTUAC2.data$Tag)

Df.NB <- subset(NTUAC2.data, Task=="NB") 
Df.I <- subset(NTUAC2.data, Task=="I") 
Df.S <- subset(NTUAC2.data, Task=="S")

library(ggplot2)
library(ggsignif)
library(plotly)

# violin plot

Sub.ACC.violin <- ggplot(NTUAC2.data, aes(x=Subject, y=ACC, fill=Subject)) +
                  geom_violin(width=1.5, colour = NA) + 
                  # geom_boxplot(width=0.1, fill="white") +
                  stat_summary(fun.y=mean, geom="point", shape=2, size=3) +
                  stat_summary(fun.y=median, geom="point", size=2, color="red")

Sub.ACC.violin


Tag.ACC.violin <- ggplot(NTUAC2.data, aes(x=Tag, y=ACC, fill=Task)) + 
                  geom_violin(scale = "width", colour = NA)
                  # geom_boxplot(width=0.1, fill="white") +
                  # stat_summary(fun.y=mean, geom="point", shape=2, size=3) +
                  # stat_summary(fun.y=median, geom="point", size=2, color="black")
Tag.ACC.violin
  
  
  
  
  
  