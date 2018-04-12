#Day_1_Intro to GitHub_&_BioStats
#12/04/2018

#Matthew_Arendse
#3440197

#This serves as the starting point for the BSc BCB(Hons) BioStats Module 2018

#Load Libraries:
library(tidyverse)
library(e1071) # for calculating skewness


#Exploring Data sets: Basic Stats (Ch2 2.1.1)

# Numerical Data:--------------------------------------------------------------------------------------------
#Intergers (Nominal)-------------------------------------------------------------

#Generate interger data
interger_r <- as.integer(seq(5, 14, by = 1)) # Creates a sequence of intergers from 5 to 22 spaced by 1 unit

#Get a summary of the data
summary(interger_r)

#Continuous data------------------------------
  numeric_r <- seq(23, 33, length.out = 10)
#Dates--------------------

dates_r <- seq.Date(as.Date("2009-12-16"), as.Date("2009-12-25"), by = "day")

#Creating a Data frame:------------------------------------

#create the data frame
df_r <- data.frame(intergers = interger_r,
                   nemric = numeric_r,
                   dates = dates_r)

#create a tibble
df_r <- as_tibble(df_r)
summary(df_r)
View(df_r)




#Qualitative Data:----------------------------------------------------------------------
#Categorical data------------------

#List factors

#Electronics

elec_r <- as.factor(c("laptop",
                    "desktop",
                    "cell phone"))
#People

people_r <- as.factor(c("happy",
                     "sad",
                     "non emotive"))
#colours
col_r <- as.factor(c("red",
                     "blue",
                     "green"))

#Create dataframe
df_r2 <- data.frame(Electronics = elec_r,
                   People = people_r,
                   Colour = col_r)

#create a tibble
df_r2 <- as_tibble(df_r2)
summary(df_r2)
View(df_r2)

#Ordinal Data---------------------------------------------------------

#Colour orders

colour_qual <- ordered(c("blue", "green","yellow","orange","red"),
                       levels = c("blue", "green","yellow","orange","red"))
colour_qual
View(colour_qual)



#Descriptive Satistics: Basic Stats (Ch3)

#bring data in
chicks <- as_tibble(ChickWeight)

#Count the number of observations (n)
chicks %>% 
  summarise(n())
#or
chicks %>% 
  count

# Measures of Central Tendancy---------------------------------------

#Mean of everything

mn.weight <- chicks %>% 
  summarise(mean(weight))

#Specific Mean
mn.weight.diet <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mn.wt = mean(weight))
           


#Meadian 

mn.wt.diet.med.wt <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mn.wt = mean(weight),
            med.wt = median(weight))


#Visualise the density of the data

ggplot(data = filter(chicks, Time == 21), aes( x = weight, fill = Diet))+
  geom_density(alpha = 0.4)


#Calculate Skewness

mn.wt.med.wt.skew <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mn.wt = mean(weight),
            med.wt = median(weight),
            skew.wt = skewness(weight))



#Create weight summary file for Ch3 Exercise:

wt.sum <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mn.wt = mean(weight),
            med.wt = median(weight),
            sd.wt = sd(weight),
            var.wt = var(weight),
            wt.min = min(weight),
            wt.max = max(weight),
            wt.quart.L = quantile(weight, 0.25),
            wt.quart.M = quantile(weight, 0.5),
            wt.quart.U = quantile(weight, 0.75))
  


