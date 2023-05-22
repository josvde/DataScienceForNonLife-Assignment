##########################################
## Assignment Data science for None-Life##
## Jean-Ferdinand van Cauwenbergh       ##
## Jos Van den Eynde                    ## 
## Robin Dessein                        ##
##########################################


library(mgcv)
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyverse) # for data manipulation
library(sf) # for spatial data manipulation
library(tmap) # for creating maps
library(gridExtra)
library(car)


## 0. Loading and manipulating data ####
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
rm(dir)

Data <- as.data.table(read.csv("DataCombined.csv", header=TRUE, sep=";", dec="."))

# make a quick selection for relevant covariates
Data <- select(Data, AGEPH,duree,nbrtotc,chargtot,agecar,sexp,fuelc,split,usec,fleetc,sportc,coverp,powerc,INS)

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
rm

#set reference levels
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

# load dataset with no outliers
DataCleaned <- Data[which(Data$chargtot  > 0),]
Tmax2=142112
Data_no_out <- DataCleaned[which(DataCleaned$chargtot < Tmax2),]

#add average claim amount as covariate
Data_no_out$avgCA <- Data_no_out$chargtot/Data_no_out$nbrtotc

## 1. Poisson GAM ####
# In order to fit a GAM, we need to define / determine a couple of values regarding the smooth function of covariates.
# The values that need to be determined/optimized are: the smoothing parameter sp, the number of basis functions...
# the type of these basis functions and the optimization method used to determine the shape of these function (E.G. REML)

GAMPois1 <- gam(nbrtotc~s(AGEPH)+sexp+agecar+fuelc+split+fleetc+coverp+INS,offset=log(duree),method="REML",family=poisson(link="log"),data=Data)
summary(GAMPois1)

# Parametric coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      -1.514343   0.046788 -32.366  < 2e-16 ***
#   sexpMale         -0.022109   0.015993  -1.382  0.16684    
# agecar2-5        -0.245879   0.032943  -7.464 8.40e-14 ***
#   agecar6-10       -0.189726   0.033560  -5.653 1.57e-08 ***
#   agecar>10        -0.181746   0.035623  -5.102 3.36e-07 ***
#   fuelcPetrol      -0.168951   0.015226 -11.096  < 2e-16 ***
#   splitThrice       0.045038   0.029085   1.548  0.12151    
# splitTwice       -0.161212   0.023234  -6.939 3.96e-12 ***
#   splitOnce        -0.347032   0.021943 -15.815  < 2e-16 ***
#   fleetcYes        -0.082978   0.043262  -1.918  0.05511 .  
# coverpMTPL+      -0.122700   0.017697  -6.933 4.11e-12 ***
#   coverpMTPL+++    -0.135802   0.024622  -5.515 3.48e-08 ***
#   INSAntwerp        0.091828   0.031481   2.917  0.00353 ** 
#   INSBrabant & BXL  0.306303   0.028502  10.747  < 2e-16 ***
#   INSEast Flanders  0.140420   0.031270   4.491 7.10e-06 ***
#   INSHainaut        0.053802   0.028555   1.884  0.05954 .  
# INSLiege          0.134981   0.031265   4.317 1.58e-05 ***
#   INSLimburg        0.006676   0.040327   0.166  0.86852    
# INSLuxembourg    -0.169174   0.056728  -2.982  0.00286 ** 
#   INSNamur         -0.044919   0.040685  -1.104  0.26957    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq p-value    
# s(AGEPH) 6.792  7.707   1003  <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# R-sq.(adj) =  0.014   Deviance explained =  2.5%
# -REML =  62951  Scale est. = 1         n = 163657

## 2. Gamma GAM ####

GAMGamma1 <- gam(avgCA~s(AGEPH)+sexp+agecar+fuelc+split+fleetc+coverp+INS,offset=log(duree),method="REML",family=Gamma(link="log"),data=Data_no_out)
summary(GAMGamma1)

# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       8.63011    0.42744  20.190   <2e-16 ***
#   sexpMale          0.06826    0.14574   0.468   0.6395    
# agecar2-5        -0.65290    0.30165  -2.164   0.0304 *  
#   agecar6-10       -0.67385    0.30861  -2.183   0.0290 *  
#   agecar>10        -0.48148    0.32822  -1.467   0.1424    
# fuelcPetrol       0.03692    0.13887   0.266   0.7903    
# splitThrice      -0.28868    0.26843  -1.075   0.2822    
# splitTwice       -0.52715    0.21243  -2.482   0.0131 *  
#   splitOnce        -0.32904    0.20107  -1.636   0.1018    
# fleetcYes        -0.15444    0.39205  -0.394   0.6936    
# coverpMTPL+      -0.40178    0.16280  -2.468   0.0136 *  
#   coverpMTPL+++     0.08469    0.22410   0.378   0.7055    
# INSAntwerp        0.22679    0.28634   0.792   0.4284    
# INSBrabant & BXL -0.09550    0.26208  -0.364   0.7156    
# INSEast Flanders -0.04209    0.28672  -0.147   0.8833    
# INSHainaut       -0.16124    0.26091  -0.618   0.5366    
# INSLiege         -0.16390    0.28638  -0.572   0.5671    
# INSLimburg        0.30324    0.36753   0.825   0.4093    
# INSLuxembourg    -0.15197    0.51915  -0.293   0.7697    
# INSNamur         -0.11823    0.37089  -0.319   0.7499    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value
# s(AGEPH) 7.588  8.355 0.967   0.461
# 
# R-sq.(adj) =  -0.0486   Deviance explained = 6.63%
# -REML = 1.51e+05  Scale est. = 75.586    n = 18333


























