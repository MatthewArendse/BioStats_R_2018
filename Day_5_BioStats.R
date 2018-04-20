#Day_5_BioStats
#12/04/2018

#Matthew_Arendse
#3440197

#Day 5 of the BSc BCB(Hons) BioStats Module 2018: More ANOVA Tests and Examples



# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(Rmisc)


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

snakes.summary2 <- summarySE(data = Snakes, measurevar = "openings", groupvars = c("day"))


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
  theme_pubr()+
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

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

corrplot(ecklonia_pearson, method = "circle")
