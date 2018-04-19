#Day_4_BioStats
#19/04/2018

#Matthew_Arendse
#3440197

#Day 4 of the BSc BCB(Hons) BioStats Module 2018: ANOVA Tests


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)


# Run a T-Test ------------------------------------------------------------

  # First load the data
  chicks <- as_tibble(ChickWeight)

  # Then subset out only the sample sets to be compared
  chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21) # used to filter out and retain only the data specified

  # Run the T-test
  
  t.test(weight ~ Diet, data = chicks_sub, var.equal = FALSE)


# Run a one-way ANOVA -----------------------------------------------------

  # Research Question: Is there a diiference in chicken mass after 21 days based on diet type?
  
  # Hypothesis (0): There is no difference in mean chicken mass after 21 days based on diet type.
  # Hypothesis (1): A difference exisits in mean chicken mass after 21 days based on diet type.
  
  
  # subset the data for Day 21 only
  chicks21 <- chicks %>% 
    filter(Time == 21) 
  
  # Run the ANOVA
  chicks.aov1 <- aov(weight ~ Diet, data = chicks21)
  summary(chicks.aov1)
  
  # F statistic < 0.05 therefore we reject the Null Hypothesis
    # A significant difference does exist in mean chicken mass after 21 days based on diet type.
 
  chick21_av <- chicks21 %>% 
    group_by(Diet) %>% 
    summarise(mn.wt = mean(weight))
  
  ggplot(data = chicks21, aes( x = Diet , y = weight))+
    geom_boxplot(aes(fill = Diet), notch = TRUE)+
    geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))+#This plot allows us to see which diet types are significantly different from one another
    geom_point(data = chick21_av, size = 2, shape = 16,
               aes(y = mn.wt), colour = "yellow") +
    labs(title = "Variation in mass of chickens",  
         subtitle = "Based on diet type", y = "Weight (g)",x = "Diet Type")
  
# To test where the significant diference lies after an ANOVA has been done
  TukeyHSD(chicks.aov1)
  
    # Try to visualise
    ?TukeyHSD

   chicks.aov1 <- aov(weight ~ Diet, data = chicks21)
    TukeyHSD(chicks.aov1, "Diet", ordered = TRUE)
  
    # In Base R
    plot(TukeyHSD(chicks.aov1, "Diet"))
    
    # Using ggplot
    chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks21))$Diet)
    chicks_Tukey$pairs <- as.factor(row.names(chicks_Tukey))
    
    ggplot(chicks_Tukey, aes(x = pairs, y = diff))+ #group = 1 states that all points plotted belong to one group and that said group needs to be connected by a line.
      geom_errorbar(aes(ymin = lwr, 
                        ymax = upr))+
      geom_point(aes(colour = pairs))+
      geom_hline(yintercept = 0, linetype = "dashed")+
      labs(title = "TukeyHSD for one-tail ANOVA",  
           subtitle = "For variation in mean chick mass by diet type", 
           y = "Difference",x = "Diet Pairs")+
      theme_pubr()+
      theme(legend.position = "right")
 # RWS: Very nice!   

# Play Own Data
    
    Vipers <- read_csv("C:/R Workshop 2018/Vipers(Max SVL).csv")
    
    Av_all_vipers <- Vipers %>% 
      na.omit() %>% 
      group_by(SF) %>% 
      summarise(mn.TL = mean(TL))
    
    vipers.aov1 <- aov(TL ~ SF, data = Vipers)
    summary(vipers.aov1)
    
    TukeyHSD(vipers.aov1, "SF", ordered = TRUE)
    
    #Base R plot
    plot(TukeyHSD(vipers.aov1, "SF"))
    
    #Tidy R plot
    vipers_Tukey <- as.data.frame(TukeyHSD(aov(TL ~ SF, data = Vipers))$SF)
    vipers_Tukey$SF <- as.factor(row.names(vipers_Tukey))
    
    ggplot(vipers_Tukey, aes(x = SF, y = diff))+ 
      geom_errorbar(aes(ymin = lwr, 
                        ymax = upr))+
      geom_point(aes(colour = SF))+
      geom_hline(yintercept = 0, linetype = "dashed")+
      labs(title = "TukeyHSD for one-tail ANOVA",  
           subtitle = "For variation in mean body size by Clade", 
           y = "Difference",x = "Diet Pairs")+
      theme_pubr()+
      theme(legend.position = "right")
      
# Here we see that viperines are significantly smaller (on average) than crotalines  


# Run a Multiple Factor ANOVA ---------------------------------------------------
  
    # does time have an effect on chicken mass
    chicks.aov2 <- aov(weight ~ as.factor(Time), data = filter(chicks, Time %in% c(0, 2, 10, 21)))
    summary(chicks.aov2)
    
    #Tukey post-hoc plot
    
    plot(TukeyHSD(chicks.aov2))
    
    # How to run a Multiple Factor ANOVA
    summary(aov(weight ~ Diet * as.factor(Time), #this is where you add more factors
                data = filter(chicks, Time %in% c(4, 21)))) #this is used to specify the time range
    
    TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))  

    
    #Tukey post-hoc plot
    
    plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(0, 21)))))    
    
    #Better Visualise the Data
    chicks_mn <- ChickWeight %>% 
      group_by(Diet, Time) %>% 
      summarise(mn.wt = mean(weight,na.rm = TRUE))
    
    ggplot(data = chicks_mn, aes(x = Time, y = mn.wt))+
      geom_point(aes(colour = Diet))+
      geom_line(aes(colour = Diet))+
      theme_pubr()+
      theme(legend.position = "right")+
      labs(title = "Variation in Mean Chicken weight by Diet",  
           subtitle = "Observing changes over time", 
           y = "Mean weight (g)",x = "No. of Days")
    


    
