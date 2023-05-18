## Intro  ##

# we will first investigate for each covariate what levels yield the highest exposure to risk...
# exposure to risk =total number of claims*average claim amount/total number of years
# we will then make sure that the levels with the highest exposure to risk...
# will be the FIRST level when defining our factor variable
# as a result our expected number of claims for our reference group can be determined by only using the intercept...
# all other groups will be expressed as a correction to this reference group.
# for simplicity reasons of the first model, for the "location" covariate, we will group the observations based...
# on province level

library(ggplot2)
library(data.table)
library(dplyr)
library(tidyverse) # for data manipulation
library(sf) # for spatial data manipulation
library(tmap) # for creating maps
library(gridExtra)

## 0. Loading and manipulating data ##
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
rm(dir)

Data <- as.data.table(read.csv("DataCombined.csv", header=TRUE, sep=";", dec="."))

# make a quick selection for relevant covariates
Data <- select(Data, AGEPH,duree,nbrtotc,chargtot,agecar,sexp,fuelc,split,usec,fleetc,sportc,coverp,powerc,INS)

# make AGEPH variable into groups
a<-Data$AGEPH
a[a>=17&a<37]="17-36"
a[a>=37&a<57]="37-56"
a[a>=57&a<77]="57-76"
a[a>76]=">76"
Data$AGEPH<-a
rm(a)

# replace INS value by province name
a<-Data$INS
a<- substr(a,1,1)
a[a==1]="Antwerp"
a[a==2]="Brabant & BXL"
a[a==3]= "West Flanders"
a[a==4]= "East Flanders"
a[a==5]= "Hainaut"
a[a==6]= "Liege"
a[a==7]= "Limburg"
a[a==8]= "Luxembourg"
a[a==9]= "Namur"
Data$INS<-a
rm(a)

## 1. Poisson Regression to build frequency table for technical tariff plan##

# 1.1. Exposure to risk
# exposure to risk = average claim cost per policy / average policy duration

# Exposure to risk by age policy holder

ETR_AGEPH <- Data %>%
  group_by(AGEPH) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set age group 17-36 as reference level

# Exposure to risk by car age

ETR_agecar <- Data %>%
  group_by(agecar) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set age group "0-1" as reference level

# Exposure to risk by sex policyholder

ETR_sexp <- Data %>%
  group_by(sexp) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set female as reference level

# Exposure to risk by fuelc

ETR_fuelc <- Data %>%
  group_by(fuelc) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set gasoil as reference level

# Exposure to risk by splits

ETR_split <- Data %>%
  group_by(split) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set monthly as reference level

# Exposure to risk by fuelc

ETR_fuelc <- Data %>%
  group_by(fuelc) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set gasoil as reference level

# 1.2. construction of data frame

# straightforward conversions to factor variables
  
# we need a data frame where we have the response (number of claims), exposure (duration of contract),
# and all covariates in factor from.

  # straightforward conversions to factor variables
# 1.3. Resulting GLM & tariff table
# GLM1 <- glm(response ~ covariate1 + ... + covariateN + offset(log(expo)), data= DF, family=poisson(link="log"))

# to build the tariff table we will use: GLM1 %>% broom::augment(type.predict ="response")

