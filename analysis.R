##################################
##################################

# Title: 'Analysis.R'
# Description: This script analyzes the previously merged data. It first creates dummy variables and summary statistics in part 1,
# and then uses regression analysis to estimate the effect of the introduction of nationwide minimum wage in Germany in 2015
# Author: Dominik Prugger
# Date: 2.2.2018

##################################
##################################


#### SKRIPT  ####  ####
#install necessary packages
#install.packages("dplyr")
#install necessary packages
#install.packages("dplyr")
"dplyr"### IMPORT DATA SKRIPT ### Load required libraries. They are preinstalled.
install.packages("stargazer")
library(foreign)
library(dplyr)
library(tidyr)
library(stargazer)
library(tidyverse)

#Define working directory 
#library(dplyr)
# Make Sure you check your Working Directory so that the code works flawless!
getwd()
# Otherwise Set the Working Directory 
setwd("~/Utrecht/GithubCode/Analysis-Minimum Wage")

# Read in the data from the previous do-file
df = read.csv("df2-indiv.csv")
states = read.csv("df3-states.csv")

#Part one: States 
#define a dataset just for 2014 and 2015
states2014 = subset(states, Year == 2014)

#define Treatment indicator for different levels of the kaitz-Index
states$d.kaitz = NA
states$d.kaitz[states2014$Bite <1.8]  = 1
states$d.kaitz[states2014$Bite >2.1] = 0

#define a Time indicator for the before the intervention (0) and after (1). It started 2015

states$d.Year = ifelse(states$Year == 2015, 1, 0)

#create an interaction term for the aggregated states

states$did.kaitz = states$d.kaitz * states$d.Year

#run a first hand DiD regression, based on if treated and on time variable 
didreg.kaitz = lm(unemp.perc ~ d.kaitz + d.Year + did.kaitz, data = states)

#define Treatment indicator for different levels of the Google-Index - The highest five percent and the lowest five percent 
states2015 = subset(states, Year == 2015)

states$d.google = NA
states <- states %>% 
  mutate(d.google = ifelse(Trend.perc > 85, 1, 0))



states$did.google = states$d.google * states$d.Year
#Run DiD - Regression
didreg.google = lm(unemp.perc ~ d.google + d.Year + did.google, data = states)






#print the result

#print using stargazer
library(stargazer)
stargazer(didreg.kaitz, title="Results - DiD - Kaitz-Index", align=TRUE, type = "text")

stargazer(didreg.google, title="Results - DiD - Google -Index", align=TRUE, type = "text")
