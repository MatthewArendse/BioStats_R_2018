#Day_3_BioStats
#17/04/2018

#Matthew_Arendse
#3440197

#Day 3 of the BSc BCB(Hons) BioStats Module 2018: Data visualisation and distribution


# Load Libraries ----------------------------------------------------------

library(logspline)
library(fitdistrplus)
library(tidyverse)



# Generate a bunch of  random normal data -----------------------------------------

r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

#Plot a histogram to visualise our generated data

hist(r_norm) #oooo much histogram. such normal. WOW!

#Plot a Cullen and Frey graph

descdist(r_norm, discrete = FALSE, boot = 100)

#Plot a Cullen and Frey graph with OWN DATA
Vipers <- read_csv("C:/R Workshop 2018/Vipers(Max SVL).csv")

vipers_no_na <- Vipers %>% 
  na.omit() 

vipers_TL_numeric <- as.numeric(vipers_no_na$TL)

descdist(vipers_TL_numeric, discrete = FALSE, boot = 1000)



# Generate some uniform data ----------------------------------------------

r_uni <- runif(100)

par(mfrow = c(1,1))
plot(x = c(1:100), y = r_uni)
hist(r_uni)
descdist(r_uni, discrete = FALSE)


# T-Test ------------------------------------------------------------------

#T-tests are used if youre comparing 2 variables, ANOVA's compare more than 2.

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Testing Assumptions:

  #Check for normality: Using the Shapiro-Wilk Test

  shapiro.test(r_dat$dat)

  #if 0.05 < p then the data is normally distributed
  #if 0.05 >= p then the data is non-normally dist.
  
  #however, we need to specify which data set (A/B) we wanna test for normality
  
  r_dat %>% 
    group_by(sample) %>% 
    summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))
  # The line of code above allows us to run a normality test 
  # for each sample group respectivley
  
  # so sample A and B had a normal dist. 
  
  
# GET a bunch of readings on your data set at once:
  
  r_dat %>% 
    group_by(sample) %>% 
    summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
              r_norm_var = var(dat))
  #by testing your data's normality, and normality of the variance.
  

  
  

# One-Sample T-Test -------------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5), sample = "A")

  #Visualise
  
  ggplot(data = r_one, aes(x = dat))+
    geom_histogram(binwidth = 5, fill = "salmon", colour = "grey")

#Run the T-test
  t.test(r_one$dat, mu = 20) #here mu refers to the Pop.Mean

  #Results
#  One Sample t-test #type of test we used
#  
#  data:  r_one$dat
#  t = 1.0718, df = 19, p-value = 0.2973 # T-value and P values (NOT SIG.)
#  alternative hypothesis: true mean is not equal to 20 # statement of results
#  95 percent confidence interval: # level of confidence
#    18.85107 23.56042
#  sample estimates:
#    mean of x 
#  21.20574 #actual mean calculated and tested against hypothesised mean.
  # RWS: Avoid having uncommented text in your scripts
  
  


# Two-Sample T-Test -------------------------------------------------------

  #Generate some random normal data
  r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                              rnorm(n = 20, mean = 5, sd = 1)),
                      sample = c(rep("A", 20), rep("B", 20)))
  
  #Perform t-test
  
  t.test(dat ~ sample, data = r_two, var.equal = TRUE) # note how we set the `var.equal` argument to TRUE because we know 
  # our data has the same SD 
  
  # ~ is used to test something by something...here we a comparing "sample" within "dat"
  
  #Visualise
  ggplot(data = r_two, aes( x = sample , y = dat))+
    geom_boxplot(aes(fill = sample))+
    theme_minimal()
  
  #Pick a side:
  #Is A less than B?
  t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")
  
  #is A greater than B?
  t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")
  
  

# Play Own Data -----------------------------------------------------------

# I need to be able to load the file to check your work
Vipers <- read_csv("C:/R Workshop 2018/Vipers(Max SVL).csv")

Vipers <- Vipers[-c(26),]

Av_all_vipers <- Vipers %>% 
  na.omit() %>% 
  group_by(SF) %>% 
  summarise(mn.TL = mean(TL))


t.test(TL ~ SF, data = Vipers, var.equal = FALSE)

ggplot(data = Vipers, aes( x = SF , y = TL))+
  geom_boxplot(aes(fill = SF))+
  geom_point(data = Av_all_vipers, size = 2, shape = 16, aes(y = mn.TL), colour = "yellow")+
  labs(title = "Average Body Size", subtitle = "Crotalines vs Viperines", y = "Av. Body Size (mm)",x = "Clade") +
  theme_minimal()

