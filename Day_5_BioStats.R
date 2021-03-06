#Day_5_BioStats
#12/04/2018

#Matthew_Arendse
#3440197

#Day 5 of the BSc BCB(Hons) BioStats Module 2018: More ANOVA Tests and Examples



# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)
# library(Rmisc) # Unfortunately this overrides many dplyr functions


# Load & Sort Data ---------------------------------------------------------------

Snakes <- read_csv("Snakes.csv")
View(Snakes)


Snakes$day = as.factor(Snakes$day)


snakes.summary <- Snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

snakes.summary2 <- Rmisc::summarySE(data = Snakes, measurevar = "openings", groupvars = c("day"))


# Visualise the Data ------------------------------------------------------

ggplot(data = Snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)


# Set out a null hypothesis -----------------------------------------------

#H0: There is no difference between snakes with respect to the number of openings at which they habituate.
                                                       #or
#H0: There is no difference between days in terms of the number of openings at which the snakes habituate.


# Run an ANOVA to test for significant difference between trials ----------

snakes.aov <- aov(openings ~ day + snake, data = Snakes) 
summary(snakes.aov) #there is a diff between days but not between snakes(?)


# Test if our data conforms to the assumptions for ANOVA ------------------

# so we make a histogram of the residuals;
#  to see if the data is normal(normally distributed)
snakes.res <- residuals(snakes.aov)
hist(snakes.res)


# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic

plot(fitted(snakes.aov), residuals(snakes.aov))


#Check which days significantly differ from one another

snakes.tukey <- TukeyHSD(snakes.aov, which = "day")
plot(snakes.tukey)




# Moth Data Exercise ------------------------------------------------------

#Load & Summarise Data
Moths <- read_csv("Moths.csv")
View(Moths)

moths <- Moths %>% 
  gather(key = "trap", value = "count", -Location)

#Visulaise our Data generally

plot1 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = Location))+
  geom_jitter(width = 0.05, shape = 21)

plot1

plot2 <- ggplot(data = moths, aes(x = trap, y = count)) +
  geom_boxplot(aes(fill = trap))+
  geom_jitter(width = 0.05, shape = 21)

plot2

plot3 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap))

plot3

ggarrange(plot1, plot2, plot3, ncol = 2, nrow = 2)

#Null hypothesis

  #There is no difference in catch count based on trap location

  #There is no difference in catch count based on trap type


#Run ANOVA

moths.aov <- aov(count ~ trap * Location, data = moths) 
summary(moths.aov)

moths.tukey <- TukeyHSD(moths.aov, which = "Location")
plot(moths.tukey)

moths.tukey2 <- as.data.frame(moths.tukey$Location)
moths.tukey2$Locations <- row.names(moths.tukey2)
                                     
#Visualise our ANOVA results

ggplot(moths.tukey2, aes(x = Locations, y = diff))+ 
  geom_errorbar(aes(ymin = lwr, 
                    ymax = upr))+
  geom_point(aes(colour = Locations),size = 4)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(title = "TukeyHSD for two-tail ANOVA",  
       subtitle = "For variation in catch number size by trap location", 
       y = "Difference",x = "Locations")+
  theme(legend.position = "right")
# RWS: This looks great!



# ANOVA Exercises ---------------------------------------------------------


# Exercise 1 (Pigs) --------------------------------------------------------------

# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

# Research Question:

  #Does feed type have an effect on mass at the end of the experiment?

  #H0: Feed type has no affect on pig mass

#Summarise the data
bacon_sum <- bacon %>% 
  group_by(feed) %>% 
  summarise(mn.mass = mean(mass))

#visualise the data
ggplot(bacon_sum, aes(x = feed, y = mn.mass))+
  geom_col(aes(fill = feed))

#Run an ANOVA to test H0:

bacon.aov <- aov(mass ~ feed, data = bacon) 
 
summary(bacon.aov)

#Visualise the results
bacon.tukey <- TukeyHSD(bacon.aov, which = "feed")
plot(bacon.tukey)

bacon.tukey2 <- as.data.frame(bacon.tukey$feed)
bacon.tukey2$feed <- row.names(bacon.tukey2)


#Plot the results

ggplot(bacon.tukey2, aes(x = feed, y = diff))+ 
  geom_errorbar(aes(ymin = lwr, 
                    ymax = upr))+
  geom_point(aes(colour = feed),size = 4)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(title = "TukeyHSD for two-tail ANOVA",  
       subtitle = "For variation in pig mass by feed type", 
       y = "Difference",x = "Feed Type")+
  theme(legend.position = "right")


# We reject H0 in favour of an alternative hypothesis. 
# There is a significant difference in pig mass as a result of feed type.
# Feed type does have an effect on pig mass.


# Exercise 2 (Tooth Growth)--------------------------------------------------------------

# Load data
teeth <- datasets::ToothGrowth

# Learn about the experiment
?ToothGrowth

#Research Question
  #Is there a difference in results between OJ and VC tests? 
    #...and if so, which dosage level contributed most to tooth growth?
  #H0:No difference exists between OJ and VC treatments or dosage level
  #H1: A difference does exist

#Does treatment type (OJ/VC) affect tooth lenght?
teeth.aov <- aov(len ~ as.factor(supp) * as.factor(dose), data = teeth) 

summary(teeth.aov)

teeth.tukey <- TukeyHSD(teeth.aov)
plot(teeth.tukey)

#Visualise the results

teeth.tukey2 <- as.data.frame(teeth.tukey$dose)
teeth.tukey2$supp <- row.names(teeth.tukey2)

#Plot the results

ggplot(teeth.tukey2, aes(x = supp, y = diff))+ 
  geom_errorbar(aes(ymin = lwr, 
                    ymax = upr))+
  geom_point(aes(colour = supp),size = 4)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(title = "TukeyHSD for two-tail ANOVA",  
       subtitle = "For variation in teeth length by treatment type", 
       y = "Difference",x = "Treatment Type")+
  theme(legend.position = "right")

# Linear Regression -------------------------------------------------------

# Load the Data

View(faithful)

#Look at the data spread 

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() 

# Define a Null Hypothesis

  # Waiting time does not affect the duration of eruptions


# Run a linear regression model

  # is there a correlation between eruptions and waiting time?

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

#Visualise the data

# Get your p, r squared and slope values to display on the graph
  slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it is approx. 0, so...
  p.val = 0.001
  p.val.2 <- round(coefficients(summary(eruption.lm))[2, 4],3) # p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) 
  r2 <- round(summary(eruption.lm)$r.squared, 3)

#Plot the linear regression model (lm)
  
  ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), # These lines add the display values
           parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), 
           parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), 
           parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") + # This line fits the linear model (lm)
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")

  # This model shows us that there is a positive relationship...
  # ...between eruption duration and waiting time.




# Correlations ------------------------------------------------------------

# Load required libraries
library(corrplot)

# Load data

ecklonia <- read_csv("C:/R Workshop 2018/Intro_R_Workshop-master/data/ecklonia.csv")
  View(ecklonia)
  
# State hypothesis
  #There is no correlation between stipe length and frond length

# Run the analysis
  
cor.test(ecklonia$stipe_length, ecklonia$frond_length)
  
  # cor.test output
  
  # Pearson's product-moment correlation

  # data:  ecklonia$stipe_length and ecklonia$frond_length
  # t = 4.2182, df = 24, p-value = 0.0003032
  # alternative hypothesis: true correlation is not equal to 0
  # 95 percent confidence interval:
  # 0.3548169 0.8300525
  # sample estimates:
  # cor = 0.6524911 

# Dispaly the data

ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()


# How to run multiple correlation tests at once.

  # Tidy the dataset
ecklonia_sub <- ecklonia %>% 
  select(stipe_length:epiphyte_length)

ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor



# Spearman rank test ------------------------------------------------------

ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length),3))

  #Run the test
cor.test(ecklonia$length, ecklonia$primary_blade_length, method = "spearman")


# True Visualisation ------------------------------------------------------

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2)) #Used to display r value

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) + # code to display r value
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

# Alternate method

ecklonia_pearson <- round(cor(ecklonia_sub),2)
ecklonia_pearson

corrplot(ecklonia_pearson, method = "circle")



# Heat Map 1 for ecklonia_pearsons ------------------------------------------
library(reshape2)

melted_ecklonia <- melt(ecklonia_pearson)

ggplot(data = melted_ecklonia, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  labs(x = "Variable 1", y = "Variable 2",  title = "Pearson correlation coefficients of multiple Ecklonia variables") +
  theme_pubclean()+
  theme(legend.position = "right")+
  theme(axis.text.x = element_text(angle = 55, hjust = 1,vjust = 1))


# Heat Map 2 --------------------------------------------------------------


# Get upper triangle of the correlation matrix
get_upper_tri <- function(ecklonia_pearson){
  ecklonia_pearson[lower.tri(ecklonia_pearson)]<- NA
  return(ecklonia_pearson)
}

# Melt the correlation matrix
upper_tri <- get_upper_tri(ecklonia_pearson)
melted_eck2 <- melt(upper_tri, na.rm = TRUE)

# Plot new Heatmap

ggplot(data = melted_eck2 , aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "blue", mid = "black", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  labs(x = "Variable 1", y = "Variable 2",  title = "Pearson correlation coefficients of multiple Ecklonia variables ") +
  theme_pubclean()+
  theme(legend.position = "right")+
  theme(axis.text.x = element_text(angle = 55, hjust = 1,vjust = 1))+
  coord_fixed()


# Heat Map 3 FINAL --------------------------------------------------------------

# Re order the data

reorder_eck <- function(ecklonia_pearson){
  # Use correlation between variables as distance
  dd <- as.dist((1-ecklonia_pearson)/2)
  hc <- hclust(dd)
  ecklonia_pearson <-ecklonia_pearson[hc$order, hc$order]
}


# Reorder the correlation matrix
eck <- reorder_eck(ecklonia_pearson)
upper_tri <- get_upper_tri(eck)

# Melt the correlation matrix
melted_eck3 <- melt(upper_tri, na.rm = TRUE) 

# Create FINAL HeatMap
ggplot(melted_eck3, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)+
  scale_fill_gradient2(low = "red", high = "blue", mid = "black", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  labs(x = "Variable 1", y = "Variable 2",  title = "Pearson correlation coefficients of multiple Ecklonia variables ") +
  theme_pubclean()+
  theme(legend.position = "right")+
  theme(axis.text.x = element_text(angle = 55, hjust = 1,vjust = 1))+
  coord_fixed()
  





