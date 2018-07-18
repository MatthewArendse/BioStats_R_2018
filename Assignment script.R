#BIOSTATS ASSIGNEMENT
#Matthew Arendse 3440197
#Shaun Pieterse 3135920

#Loading Libraries----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(reshape2)
library(scales)
library(ggpubr)

#ANOVA---------------------------------------------------------------------

#Supporting plots for ANOVA
plot1<-ggplot(X1999_Election_Results, aes(x = Party, y = value, fill = variable))+
  geom_bar(stat = "identity", width = 0.5)+
  theme_bw() +
  ggtitle("1999")+
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = c(1000000, 3000000, 5000000, 7000000, 9000000, 11000000))+
  ylab("Number of votes")

plot1

plot2 <- ggplot(X2004_Election_Results, aes(x = Party, y = value, fill = variable))+
  geom_bar(stat = "identity", width = 0.5)+
  theme_bw() +
  ggtitle("2004")+
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = c(1000000, 3000000, 5000000, 7000000, 9000000, 11000000))+
  ylab("Number of votes")

plot2

plot3 <- ggplot(X2009_Election_Results, aes(x = Party, y = value, fill = variable))+
  geom_bar(stat = "identity", width = 0.5)+
  theme_bw() +
  ggtitle("2009")+
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks = c(1000000, 3000000, 5000000, 7000000, 9000000, 11000000))+
  ylab("Number of votes")

plot3

plot4 <- ggplot(X2014_Election_Results, aes(x = Party, y = value, fill = variable))+
  geom_bar(stat = "identity", width = 0.5)+
  theme_bw() +
  ggtitle("2014")+
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0.5,
                                    color = "black"))+
  scale_y_continuous(breaks = c(1000000, 3000000, 5000000, 7000000, 9000000, 11000000))+
  ylab("Number of votes")


plot4

options(scipen = 999)

plot5 <- ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2, 
                   legend = "right", common.legend = TRUE)

Election_data_plot <- annotate_figure(plot5,
                top = text_grob("Figure 1: IEC Election Data (1999 - 2014)", 
                                color = "black", face = "bold", size = 20))

Election_data_plot

#Running and Visualizing the ANOVA
options(scipen = 999)
fit <- aov(Party ~ Vote*Year, data = ANOVA)

options(scipen = 999)

summary.aov(fit)

AOV_PLOT <- ggboxplot(ANOVA, x = "Party", y = "Vote", 
          color = "Party", palette = c("#00AFBB", "#E7B800", "#FC4E07", "green"),
          order = c("ANC", "Op1", "Op2", "Op3"),
          ylab = "Number of votes", xlab = "Party", title = "Figure 2: ANOVA Results 1999-2014")+
  theme(legend.position = "none")

AOV_PLOT

#T-TEST----------------------------------------------------------------------

t.test(Votes ~ Party, data = T_test, 
       var.equal = FALSE, 
       conf.level = 0.95)

T_testplot <- ggplot(data = T_test, aes( x = Party , y = Votes))+
  geom_boxplot(aes(fill = Party))+
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title = "Figure 3: Two Sample T-test Results")
  

T_testplot

#Correlation ANC Seats v DA Votes---------------------------------------------------------------

cor.test(Correlations$ANC_Seats, Correlations$DA_Votes)

Corr_plot1 <- ggplot(data = Correlations, aes(x = DA_Votes, y = ANC_Seats)) +
  geom_smooth(method = "lm", colour = "salmon", se = F) +
  geom_point(colour = "blue") +
  labs(x = "Number of Votes for DA", y = "Number of Seats for ANC") +
  theme(legend.title = element_blank(), , axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.caption = element_text(size = 10, hjust = 0.5,
                                    color = "black"))+
  labs(title = "Figure 4: DA Votes vs. ANC Seats")

Corr_plot1

#Correlation Registerd Voters v difference in voter turnout---------------------------------------------------------------


Correlation_2 <- read_csv("E:/R stuff/Election Data/Correlation 2.csv")
View(Correlation_2)

cor.test(Correlation_2$Registered, Correlation_2$Difference)

Corr_plot2 <- ggplot(data = Correlation_2, aes(x = Registered, y = Difference)) +
  geom_smooth(method = "lm", colour = "dodgerblue2", se = F) +
  geom_point(colour = "slateblue3") +
  labs(x = "Number of Registered Voters", y = "Difference in number of voters") +
  theme(legend.title = element_blank(), , axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        plot.caption = element_text(size = 10, hjust = 0.5,
                                    color = "black"))+
  labs(title = "Figure 5: Correlation of Number of Registered Voters with 
       Difference in number of actual voters ")

Corr_plot2


Corr_final <- ggarrange(Corr_plot1, Corr_plot2, ncol = 1, nrow = 2, 
                        legend = "right")

Corr_final

#Provincial Plots---------------------------------------------------------

options(scipen = 999)

Facet_provdat <- ggplot(ProvData, aes(x = Year, y = Votes, fill = Party))+
  geom_bar(stat = "identity", position = "dodge", width = 3)+
  scale_fill_manual(values = c("ANC" = "green4", "DA" = "dodgerblue3"))+
  scale_y_continuous(breaks = c(1000000, 3000000, 5000000, 7000000, 9000000, 11000000))+
  scale_x_continuous(breaks = c(1999, 2004, 2009, 2014))+
  facet_wrap(~Province, nrow = 3)+
  theme(legend.title = element_blank(), , axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  labs(title = "Figure 6: Provincial Election Results 1999-2014", 
       subtitle = "Actual ANC Votes vs Actual Da votes + Non-votes")
  
Facet_provdat
