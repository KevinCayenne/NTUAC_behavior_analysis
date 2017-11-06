setwd("D:/Data/Research_projeccts/NTUACdata/NTUAC_Analysis")
NTUAC.all <- read.csv('ALL_3_4_5±è.csv')

NTUAC.data <- data.frame(NTUAC.all)

attach(NTUAC.data)

library(ggplot2)
library(ggsignif)
library(wesanderson)

NTUAC.data$Session <- as.factor(NTUAC.data$Session)
NTUAC.data$Task <- as.factor(NTUAC.data$Task)

for (k in c(3:21)){
    table_name <- colnames(NTUAC.data)[k]
    png(sprintf("%s.png",table_name), width = 1800, height = 700)
    par(mfrow=c(1,4))
    boxplot(NTUAC.data[,k] ~ NTUAC.data[,k+19],main=table_name)
    boxplot(NTUAC.data[,k] ~ NTUAC.data$Task)
    boxplot(NTUAC.data[,k] ~ NTUAC.data[,k+19] * NTUAC.data$Task)
    boxplot(NTUAC.data[,k] ~ NTUAC.data$Task * NTUAC.data[,k+19])
    dev.off()
}

for (i in c(22:40)){
  NTUAC.data[,i] <- as.factor(NTUAC.data[,i])
}

for (i in c(3:21)){
  table_name <- colnames(NTUAC.data)[i]
  
  png(sprintf("%s-1.png", table_name), width = 1800, height = 700)
  print(ggplot(NTUAC.data, aes(x = Session, y = NTUAC.data[,i], group = Task, colour = Task)) + 
        geom_line() + 
        geom_point(size=10) +
    
        labs(
             x = "Sessions",
             y = "Accuracy"
        ) + 
        
        ggtitle(sprintf("%s-1", table_name)) +
          
        theme(plot.title = element_text(hjust = 0.5, size= 30),
              title = element_text(size=10),
              legend.text = element_text(size=20),
              legend.title = element_text(size=20),
              axis.text = element_text(size=20),
              axis.title = element_text(size=15,face="bold")))
  
  dev.off()

  png(sprintf("%s-2.png", table_name), width = 1800, height = 700)
  print(ggplot(NTUAC.data, aes(x = Session, y = NTUAC.data[,i], group = Task, colour = NTUAC.data[,i+19])) + 
        geom_line() +
        geom_point(aes(shape = Task),size = 10) +
        geom_text(aes(label = NTUAC.data[,i]),hjust=0, vjust=2.5) +
        
        labs(
             x = "Sessions",
             y = "Accuracy",
             col= "Levels"
        ) + 
        
        ggtitle(sprintf("%s-2", table_name)) +
          
        theme(plot.title = element_text(hjust = 0.5, size= 30),
              title = element_text(size=10),
              legend.text = element_text(size=20),
              legend.title = element_text(size=20),
              axis.text = element_text(size=20),
              axis.title = element_text(size=15,face="bold")))
             
  dev.off()
}

# item <- 1
# ggplot(NTUAC.data, aes(x = Session, y = NTUAC.data[,3], group = Task, colour = Task)) +
#         geom_line() +
#         geom_point(size=2) +
# 
#         labs(title = sprintf("plot%d", item),
#              x = "Sessions",
#              y = "Accuracy"
#         ) +
# 
#         theme(plot.title = element_text(hjust = 0.5),
#         title = element_text(size=15),
#         legend.text = element_text(size=15),
#         legend.title = element_text(size=15),
#         axis.text = element_text(size=13),
#         axis.title = element_text(size=13,face="bold"))
# 
  # ggplot(NTUAC.data, aes(x = Session, y = NTUAC.data[,3], group = Task, colour = NTUAC.data[,22])) +
  #         geom_line() +
  #         geom_point(aes(shape = Task),size = 10) +
  #         geom_text(aes(label = NTUAC.data[,3]),hjust=0, vjust=2.5) +
  # 
  #         labs(title = sprintf("plot%d-2", item),
  #              x = "Sessions",
  #              y = "Accuracy",
  #              col= "Levels"
  #         ) +
  # 
  #         theme(plot.title = element_text(hjust = 0.5),
  #               title = element_text(size=15),
  #               legend.text = element_text(size=15),
  #               legend.title = element_text(size=15),
  #               axis.text = element_text(size=13), 
  #               axis.title = element_text(size=13,face="bold"))
