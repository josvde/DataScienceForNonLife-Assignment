## Intro  ####

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

## 0. Loading and manipulating data ####
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

## 1. Poisson Regression to build frequency table for technical tariff plan####

# 1.1. Exposure to risk ####
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

# Exposure to risk by usec

ETR_usec <- Data %>%
  group_by(usec) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set private as reference level

# Exposure to risk by fleetc

ETR_fleetc <- Data %>%
  group_by(fleetc) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set "No" as reference level

# Exposure to risk by sportc

ETR_sportc <- Data %>%
  group_by(sportc) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set "Yes" as reference level

# Exposure to risk by coverp

ETR_coverp <- Data %>%
  group_by(coverp) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set MTPL as reference level

# Exposure to risk by powerc

ETR_powerc <- Data %>%
  group_by(powerc) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set >110 as reference level

# Exposure to risk by INS (province)

ETR_INS <- Data %>%
  group_by(INS) %>%
  summarise(ETR = mean(chargtot)/mean(duree))

# conclusion: set West Flanders as reference level

# 1.2. construction of data frame ####

# conversions of all covariates to factor variables, with the reference level as first level.
# summary of reference level: age group 17-36, car age 0-1, female, gasoil, monthly payments, private car,...
# No to fleet car, Yes to sportcar, policy type MTPL, power of car >110 and finally West Flanders.

Data$AGEPH <- factor(Data$AGEPH, levels=c("17-36","37-56","57-76",">76"))
Data$agecar <- factor(Data$agecar, levels=c("0-1","2-5","6-10",">10"))
Data$sexp <- factor(Data$sexp, levels=c("Female","Male"))
Data$fuelc <- factor(Data$fuelc, levels=c("Gasoil","Petrol"))
Data$split <- factor(Data$split, levels=c("Monthly","Thrice","Twice","Once"))
Data$usec <- factor(Data$usec, levels=c("Private","Professional"))
Data$fleetc <- factor(Data$fleetc, levels=c("No","Yes"))
Data$sportc <- factor(Data$sportc, levels=c("Yes","No"))
Data$coverp <- factor(Data$coverp, levels=c("MTPL","MTPL+","MTPL+++"))
Data$powerc <- factor(Data$powerc, levels=c(">110","<66","66-110"))
Data$INS <- factor(Data$INS,levels=c("West Flanders","Antwerp","Brabant & BXL","East Flanders","Hainaut","Liege","Limburg","Luxembourg","Namur"))

# 1.3. Resulting GLM & tariff table

GLMPois1Full <- glm(nbrtotc~AGEPH+agecar+sexp+fuelc+split+usec+fleetc+sportc+coverp+powerc+INS,offset=log(duree),data= Data, family=poisson(link="log"))

# Output GLMPoisFull
# Coefficients:
#                     Estimate   Std.Er  z value Pr(>|z|)    
#   (Intercept)      -0.88697    0.09588  -9.251  < 2e-16 ***
#   AGEPH37-56       -0.28681    0.01612 -17.796  < 2e-16 ***
#   AGEPH57-76       -0.48468    0.02077 -23.336  < 2e-16 ***
#   AGEPH>76         -0.31348    0.05237  -5.986 2.15e-09 ***
#   agecar2-5        -0.25111    0.03295  -7.621 2.52e-14 ***
#   agecar6-10       -0.19484    0.03362  -5.795 6.83e-09 ***
#   agecar>10        -0.17780    0.03577  -4.970 6.69e-07 ***
#   sexpMale         -0.04406    0.01614  -2.730 0.006326 ** 
#   fuelcPetrol      -0.18045    0.01544 -11.685  < 2e-16 ***
#   splitThrice       0.09306    0.02895   3.215 0.001305 ** 
#   splitTwice       -0.13174    0.02320  -5.678 1.36e-08 ***
#   splitOnce        -0.33001    0.02187 -15.089  < 2e-16 ***
#   usecProfessional  0.02828    0.03305   0.856 0.392228    
#   fleetcYes        -0.10628    0.04338  -2.450 0.014286 *  
#   sportcNo         -0.09140    0.07060  -1.295 0.195461    
#   coverpMTPL+      -0.13851    0.01767  -7.840 4.52e-15 ***
#   coverpMTPL+++    -0.16170    0.02480  -6.521 6.97e-11 ***
#   powerc<66        -0.24721    0.07056  -3.503 0.000459 ***
#   powerc66-110     -0.16182    0.07048  -2.296 0.021686 *  
#   INSAntwerp        0.03717    0.03128   1.188 0.234765    
#   INSBrabant & BXL  0.25806    0.02832   9.112  < 2e-16 ***
#   INSEast Flanders  0.11154    0.03121   3.574 0.000352 ***
#   INSHainaut        0.01207    0.02840   0.425 0.670971    
#   INSLiege          0.08803    0.03109   2.832 0.004630 ** 
#   INSLimburg       -0.03967    0.04021  -0.986 0.323923    
#   INSLuxembourg    -0.21769    0.05664  -3.844 0.000121 ***
#   INSNamur         -0.09572    0.04053  -2.362 0.018191 * 

# Based on the output above, we will leave out sportc and usec as covariates...
# as they have no significant impact on the response variable

GLMPois2 <- glm(nbrtotc~AGEPH+agecar+sexp+fuelc+split+fleetc+coverp+powerc+INS,offset=log(duree),data= Data, family=poisson(link="log"))

# Output GLMPois2
# Coefficients:
#                     Estimate   Std.Er   z value  Pr(>|z|)    
#   (Intercept)      -0.94809    0.08192  -11.573  < 2e-16 ***
#   AGEPH37-56       -0.28717    0.01609  -17.850  < 2e-16 ***
#   AGEPH57-76       -0.48553    0.02074  -23.412  < 2e-16 ***
#   AGEPH>76         -0.31416    0.05235  -6.001  1.96e-09 ***
#   agecar2-5        -0.25018    0.03294  -7.594  3.09e-14 ***
#   agecar6-10       -0.19333    0.03361  -5.752  8.80e-09 ***
#   agecar>10        -0.17648    0.03576  -4.934  8.04e-07 ***
#   sexpMale         -0.04373    0.01613  -2.711  0.006715 ** 
#   fuelcPetrol      -0.18057    0.01539  -11.732  < 2e-16 ***
#   splitThrice       0.09444    0.02893   3.265  0.001096 ** 
#   splitTwice       -0.13087    0.02318  -5.645  1.65e-08 ***
#   splitOnce        -0.32907    0.02186  -15.052  < 2e-16 ***
#   fleetcYes        -0.10321    0.04324  -2.387  0.016988 *  
#   coverpMTPL+      -0.13833    0.01767  -7.830  4.88e-15 ***
#   coverpMTPL+++    -0.16052    0.02472  -6.492  8.45e-11 ***
#   powerc<66        -0.27761    0.06705  -4.140  3.47e-05 ***
#   powerc66-110     -0.18844    0.06768  -2.784  0.005365 ** 
#   INSAntwerp        0.03583    0.03127   1.146  0.251899    
#   INSBrabant & BXL  0.25690    0.02831   9.075   < 2e-16 ***
#   INSEast Flanders  0.11061    0.03120   3.545  0.000393 ***
#   INSHainaut        0.01095    0.02839   0.386  0.699690    
#   INSLiege          0.08691    0.03108   2.797  0.005163 ** 
#   INSLimburg       -0.04076    0.04020  -1.014  0.310617    
#   INSLuxembourg    -0.21852    0.05663  -3.858  0.000114 ***
#   INSNamur         -0.09657    0.04052  -2.383  0.017172 * 

