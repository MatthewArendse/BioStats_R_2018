#Day_2_BioStats
#13/04/2018

#Matthew_Arendse
#3440197

#Day 2 of the BSc BCB(Hons) BioStats Module 2018: Data visualisation and distribution


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridisLite)

# Load Data  --------------------------------------------------------------

  #To generate random normal data:

  r_data <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50),
                     sample = "A") #use TAB to check for additional arguments


# Quick visualisation of the data ------------------------------------------------------

ggplot(data = r_data, aes(x = dat)) +
  geom_density()



# Some basic stats: -------------------------------------------------------

  #Calculating mean:

  r_data %>% 
  summarise(sum.dat = sum(dat), #sum of all observations
            num.dat = n(), #number of observations
            mn.dat = sum.dat/num.dat,  #calculate the mean
            act.mn = mean(dat)) #Function for calculating mean

  #Calculate the median:

    #Brute Force with Base R:
    r_data$dat[(length(r_data$dat)+1/2)]

    #TidyR Method:
    r_data %>% 
    arrange(dat) %>%
    slice(n()/2)

    #or

    r_data %>% 
    summarise(med.dat = median(dat))


#Calculating Variance and Standard Deviation

  #Equal to:
    #Sum of (x - (xmean))squared
    #Divided by(n-1)

    r_data %>%
      mutate(error.dat = dat-mean(dat)) %>% #this fucntion allows you to manipulate the data in one column by creating a new one
      summarise(errorsquare.dat = sum(error.dat * error.dat),
                r_var = errorsquare.dat/n()-1)

    r_data %>% 
      summarise(var.dat = var(dat))
      
    
# Ch 3 Exercises: ---------------------------------------------------------

  #Exercise 1:
    
    #Using the summary approach
    summary(ChickWeight)
    
    #Using the summarise approach
    CHick <- data.frame(ChickWeight %>% 
      summarise( wt.min = min(weight),
                 L.quart = quantile(weight, 0.25),
                 med.wt = median(weight),
                 mn.wt = mean(weight),
                 U.quart = quantile(weight, 0.75),
                 wt.max = max(weight)))


    
# Ch 4: Graphical Visulisation of Data ------------------------------------
 
  #Load the data   
  SA_Time <- read_csv("SA_Time_Data (N,JN,NN).csv")
  View(SA_Time)

  #Edit our data
  SA_Time <- SA_Time %>% 
    mutate(human = seq(1, n(), 1))

  sa_long <- SA_Time %>%
    gather(key = "time_type", value = "minutes", -human)
  
#Creating Graphs:
  

# Qualitative data --------------------------------------------------------


  SA.cnt <- sa_long %>%
    count(time_type) %>% # creates a column, n, with the counts
    mutate(prop = n/sum(n)) # creates the relative proportion of each species
  
  #1 - Stacked Bar Graph displaying count

plt1 <- ggplot(data = SA.cnt, aes(x = "", y = n, fill = time_type)) +
    geom_bar(width = 1, stat = "identity") +
    labs(title = "Stacked bar graph", subtitle = "cumulative sum",
         x = NULL, y = "Count") +
    theme_minimal()
  
plt1
  

  
  #2 - Stacked Bar Graph displaying the relative proportions of observations
  
plt2 <- ggplot(data = SA.cnt, aes(x = "", y = prop, fill = time_type)) +
    geom_bar(width = 1, stat = "identity") +
    scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
    labs(title = "Stacked bar graph", subtitle = "relative proportions",
         x = NULL, y = "Proportion") +
    theme_minimal()

plt2
  
  #3 -  a basic pie chart
  plt3 <- plt1 + coord_polar("y", start = 0) +
    labs(title = "Friends don't let...", subtitle = "...friends make pie charts",
         x = NULL, y = NULL) +
    scale_fill_brewer(palette = "Blues") +
    theme_minimal()
  
  plt3
  
  #4 - Bar Plot
  plt4 <- ggplot(data = SA.cnt, aes(x = time_type, fill = time_type)) +
    geom_bar(show.legend = FALSE) +
    labs(title = "Side-by-side bars", subtitle = "n per species", y = "Count") +
    theme_minimal()
  plt4 
  
  ggarrange(plt1, plt2, plt3, plt4, nrow = 2, ncol = 2, labels = "AUTO")
               
             

  

# Quantitative Data -------------------------------------------------------

  #Plot a Histogram
  
ggplot(data = sa_long, aes(x = minutes)) +
    geom_histogram(show.legend = FALSE)

sa_clean <-sa_long %>% 
  filter(minutes < 100)


ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge")+
  facet_wrap(~time_type,ncol = 1, scales = "free_x")

#Boxplots

ggplot(data = sa_clean, aes( x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type))

#Notched Boxplots

ggplot(data = sa_clean, aes( x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type), notch = TRUE)
  

#Boxplot with Mean

  #calculate mean first
sa_sum_stats <- sa_clean %>% 
  group_by(time_type) %>%
  summarise(tt_mean = mean(minutes))
  
  #visualise
ggplot(data = sa_clean, aes( x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type), notch = TRUE)+
  geom_point(data = sa_sum_stats, size = 3, shape = 18, aes(y = tt_mean), colour = "goldenrod")




# Relationships between two catergories of data -----------------------------------------------------------

ggplot(data = SA_Time, aes(y = now_now, x = just_now))+
  geom_point()+
  coord_equal(xlim = c(0, 60), ylim = c(0,60))+
  geom_smooth(method = "lm")




# Own Data Playing --------------------------------------------------------

  #Load the Data
  Vipers <- read_csv("C:/R Workshop 2018/Vipers(Max SVL).csv")


  #Sort the Data
    
  Crots <- Vipers %>% 
    group_by(SF) %>% 
    filter(SF == "C")
  
  Av_Crots <- Crots %>% 
    na.omit() %>% 
    summarise(mn.svl = mean(TL))
  
  Vips <- Vipers %>% 
    group_by(SF) %>% 
    filter(SF == "V")
  
  Av_Vips <- Vips %>% 
    na.omit() %>% 
    summarise(mn.svl = mean(TL))
  
Av_all_vipers <- Vipers %>% 
  na.omit() %>% 
  group_by(SF) %>% 
  summarise(mn.TL = mean(TL))
  
  

  
  #TL vs Latitude across all Vipers
  
  ggplot(data = Vipers, aes(y = TL, x = Latitude))+
    geom_point(aes(colour = SF))+
    geom_smooth(method = "lm")+    
    labs(title = "Latitudinal Variation in Body Size", subtitle = "Across the Viperidae", y = "Av. Body Size (mm)",x = "Latitude (km)") +
    theme_minimal()
  
  
  #TL vs Latitude for Crotalines
  
  ggplot(data = Crots, aes(y = TL, x = Latitude))+
    geom_point()+
    geom_smooth(method = "lm")+
    labs(title = "Latitudinal Variation in Body Size", subtitle = "Across the Crotalines", y = "Av. Body Size (mm)",x = "Latitude (km)") +
    theme_minimal()
  
  
  #TL vs Latitude for Viperines
  
  ggplot(data = Vips, aes(y = TL, x = Latitude))+
    geom_point()+
    geom_smooth(method = "lm")+
    labs(title = "Latitudinal Variation in Body Size", subtitle = "Across the Viperines", y = "Av. Body Size (mm)",x = "Latitude (km)") +
    theme_minimal()
  
  
  #Av size Crotalines vs Viperines:
  ggplot(data = Vipers, aes( x = SF , y = TL))+
    geom_boxplot(aes(fill = SF), notch = TRUE)+
    geom_point(data = Av_all_vipers, size = 2, shape = 18, aes(y = mn.TL), colour = "goldenrod")+
    labs(title = "Average Body Size", subtitle = "Across Viper Clades", y = "Av. Body Size (mm)",x = "Clade") +
    theme_minimal()+
    facet_wrap(~SF, scales = "free_y")
  
  #Av size Viperines:
  ggplot(data = Vips, aes( x = SF , y = TL))+
    geom_boxplot(aes(fill = SF))+
    geom_point(data = Av_Vips, size = 2, shape = 18, aes(y = mn.svl), colour = "goldenrod")+
    labs(title = "Average Body Size", subtitle = "for the Viperines", y = "Av. Body Size (mm)",x = "Clade") +
    theme_minimal()


  #Av size Crotalines:
  ggplot(data = Crots, aes( x = SF , y = TL))+
    geom_boxplot(aes(fill = SF))+
    geom_point(data = Av_Crots, size = 2, shape = 18, aes(y = mn.svl), colour = "goldenrod")+
    labs(title = "Average Body Size", subtitle = "for the Crotalines", y = "Av. Body Size (mm)",x = "Clade") +
    theme_minimal()
  
