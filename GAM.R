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

# Formula:
#   nbrtotc ~ s(AGEPH) + sexp + agecar + fuelc + split + fleetc + 
#   coverp + INS
# 
# Parametric coefficients:
#   Estimate
# (Intercept)   -1.428e+00
# sexpMale      -2.463e-02
# agecar0-1      1.805e-01
# agecar2-5     -6.920e-02
# agecar6-10    -8.986e-03
# fuelcPetrol   -1.638e-01
# splitOnce     -3.800e-01
# splitThrice    3.899e-02
# splitTwice    -1.784e-01
# fleetcYes     -7.137e-02
# coverpMTPL+   -9.482e-02
# coverpMTPL+++ -1.111e-01
# INS           -3.306e-06
# Std. Error
# (Intercept)    3.061e-02
# sexpMale       1.599e-02
# agecar0-1      3.562e-02
# agecar2-5      2.157e-02
# agecar6-10     1.866e-02
# fuelcPetrol    1.520e-02
# splitOnce      2.154e-02
# splitThrice    2.882e-02
# splitTwice     2.294e-02
# fleetcYes      4.323e-02
# coverpMTPL+    1.729e-02
# coverpMTPL+++  2.443e-02
# INS            3.363e-07
# z value Pr(>|z|)
# (Intercept)   -46.657  < 2e-16
# sexpMale       -1.541  0.12339
# agecar0-1       5.068 4.02e-07
# agecar2-5      -3.209  0.00133
# agecar6-10     -0.482  0.63007
# fuelcPetrol   -10.778  < 2e-16
# splitOnce     -17.639  < 2e-16
# splitThrice     1.353  0.17615
# splitTwice     -7.776 7.50e-15
# fleetcYes      -1.651  0.09873
# coverpMTPL+    -5.483 4.17e-08
# coverpMTPL+++  -4.546 5.48e-06
# INS            -9.829  < 2e-16
# 
# (Intercept)   ***
#   sexpMale         
# agecar0-1     ***
#   agecar2-5     ** 
#   agecar6-10       
# fuelcPetrol   ***
#   splitOnce     ***
#   splitThrice      
# splitTwice    ***
#   fleetcYes     .  
# coverpMTPL+   ***
#   coverpMTPL+++ ***
#   INS           ***
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01
# '*' 0.05 '.' 0.1 ' ' 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq
# s(AGEPH) 6.739  7.664  985.2
# p-value    
# s(AGEPH)  <2e-16 ***
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01
# '*' 0.05 '.' 0.1 ' ' 1
# 
# R-sq.(adj) =  0.013   Deviance explained = 2.33%
# -REML =  63023  Scale est. = 1         n = 163657

## 2. Gamma GAM ####

GAMGamma1 <- gam(avgCA~s(AGEPH)+sexp+agecar+fuelc+split+fleetc+coverp+INS,offset=log(duree),method="REML",family=Gamma(link="log"),data=Data_no_out)
summary(GAMGamma1)

# Family: Gamma 
# Link function: log 
# 
# Formula:
#   avgCA ~ s(AGEPH) + sexp + agecar + fuelc + split + fleetc + coverp + 
#   INS
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    8.212e+00  2.962e-01  27.729  < 2e-16 ***
#   sexpMale       8.069e-02  1.543e-01   0.523  0.60091    
# agecar0-1      4.481e-01  3.474e-01   1.290  0.19711    
# agecar2-5     -1.618e-01  2.089e-01  -0.775  0.43855    
# agecar6-10    -1.687e-01  1.815e-01  -0.929  0.35269    
# fuelcPetrol   -1.676e-02  1.467e-01  -0.114  0.90905    
# splitOnce     -2.696e-01  2.082e-01  -1.295  0.19542    
# splitThrice   -2.951e-01  2.815e-01  -1.048  0.29456    
# splitTwice    -5.306e-01  2.222e-01  -2.387  0.01698 *  
#   fleetcYes     -2.067e-01  4.149e-01  -0.498  0.61835    
# coverpMTPL+   -4.417e-01  1.682e-01  -2.627  0.00863 ** 
#   coverpMTPL+++  9.857e-02  2.354e-01   0.419  0.67535    
# INS           -2.425e-06  3.273e-06  -0.741  0.45877    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value
# s(AGEPH) 7.699  8.431 0.851   0.562
# 
# R-sq.(adj) =  -0.0426   Deviance explained = 6.15%
# -REML = 1.5106e+05  Scale est. = 84.748    n = 18333


























