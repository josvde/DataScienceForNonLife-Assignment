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
library(car)

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

# Explanation
  # exposure to risk = average claim cost per policy / average policy duration
  # = average yearly claim cost that a policy holder with that specific characteristic will have

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

# 1.2. construction of the main dataframe ####

# in this section we will program the conversion of all covariates to factor variables...
# with the before mentioned reference level as first level.
# Summary of reference level values: age group 17-36, car age 0-1, female, gasoil, monthly payments, private car,...
# no to fleet car, yes to sportcar, policy type MTPL, power of car >110 and finally West Flanders.

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

# 1.3. Poisson GLMs & expected frequency tables ####

# 1.3.1 Poisson GLMs with no interaction terms ####
GLMPois1Full <- glm(nbrtotc~AGEPH+agecar+sexp+fuelc+split+usec+fleetc+sportc+coverp+powerc+INS,offset=log(duree),data= Data, family=poisson(link="log"))
summary(GLMPois1Full)

# Output GLMPois1Full 
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
summary(GLMPois2)


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


# create GLMPois2 where we only leave out sexp compared to GLM2

GLMPois3  <- glm(nbrtotc~AGEPH+agecar+fuelc+split+fleetc+coverp+powerc+INS,offset=log(duree),data= Data, family=poisson(link="log"))
summary(GLMPois3)

# Output GLMPois3
# Coefficients:

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.98705    0.08066 -12.237  < 2e-16 ***
#  AGEPH37-56       -0.28988    0.01606 -18.054  < 2e-16 ***
# AGEPH57-76       -0.49289    0.02056 -23.974  < 2e-16 ***
# AGEPH>76         -0.32428    0.05222  -6.210 5.31e-10 ***
# agecar2-5        -0.25015    0.03294  -7.593 3.12e-14 ***
# agecar6-10       -0.19299    0.03361  -5.742 9.33e-09 ***
# agecar>10        -0.17569    0.03576  -4.913 8.99e-07 ***
# fuelcPetrol      -0.17471    0.01524 -11.465  < 2e-16 ***
# splitThrice       0.09378    0.02893   3.242 0.001188 ** 
# splitTwice       -0.13267    0.02317  -5.725 1.03e-08 ***
# splitOnce        -0.33011    0.02186 -15.102  < 2e-16 ***
# fleetcYes        -0.10072    0.04323  -2.330 0.019816 *  
# coverpMTPL+      -0.13795    0.01767  -7.809 5.77e-15 ***
# coverpMTPL+++    -0.15896    0.02472  -6.431 1.27e-10 ***
# powerc<66        -0.26919    0.06698  -4.019 5.84e-05 ***
# powerc66-110     -0.18640    0.06768  -2.754 0.005883 ** 
# INSAntwerp        0.03616    0.03127   1.156 0.247600    
# INSBrabant & BXL  0.25790    0.02831   9.111  < 2e-16 ***
# INSEast Flanders  0.11013    0.03120   3.529 0.000417 ***
#  INSHainaut        0.01007    0.02839   0.355 0.722890    
# INSLiege          0.08604    0.03108   2.769 0.005628 ** 
#  INSLimburg       -0.04143    0.04020  -1.031 0.302726    
# INSLuxembourg    -0.21820    0.05663  -3.853 0.000117 ***
#  INSNamur         -0.09697    0.04052  -2.393 0.016718 * 

# dataframes with the expected frequency numbers for reference group based on different GLMs and the correction values for other factor levels

TARFR1 <- data.frame(Name=names(coefficients(GLMPois1Full)),E_Freq=exp(coefficients(GLMPois1Full)))
TARFR2 <- data.frame(Name=names(coefficients(GLMPois2)),E_Freq=exp(coefficients(GLMPois2)))
TARFR3 <- data.frame(Name=names(coefficients(GLMPois3)),E_Freq=exp(coefficients(GLMPois3)))

# 1.3.2 Poisson GLMs with interaction terms ####

# important!":" and "*" yield different results when coding interaction terms

# quick way to get all possible interaction terms

a<-names(Data)
a<-a[!a %in% c("duree","nbrtotc","chargtot")]

r <- "AGEPH:agecar"
for(i in 1:length(a)){
  for(j in i:length(a)){
    if(!i==j) r<-paste(sep="",r,"+",a[i],":",a[j]) 
  }
  
}

cat(r)

##GLMPoisIT1 <- glm(nbrtotc~AGEPH:usec+AGEPH:fleetc+AGEPH:sportc+AGEPH:agecar+AGEPH:sexp+AGEPH:fuelc+AGEPH:split+AGEPH:coverp+AGEPH:powerc+AGEPH:INS,data= Data, family=poisson(link="log"))
##anova(GLMPoisIT1,test="Chisq")

# A first GLM was coded with all interaction terms of AGEPH (in the order they were given in the data set)
# at first glance AGEPH:usec, AGEPH:fleetc and AGEPH:sportc did not significantly contribiute a drop a deviance
# however, they were at the end of the list...
# when put in front AGEPH:usec significantly contribiutes (this is expected, as this is the first variable), but AGEPH:fleetc and AGEPH:sportc are also
# insignificant, even in 2nd and 3rd place.They will be left out!

##GLMPoisIT1_1 <- glm(nbrtotc~AGEPH:agecar+AGEPH:usec+AGEPH:sexp+AGEPH:fuelc+AGEPH:split+AGEPH:coverp+AGEPH:powerc+AGEPH:INS,data= Data, family=poisson(link="log"))
##anova(GLMPoisIT1_1,test="Chisq")

##AGEPH:usec now at 2nd place, performs poorly, will be left out, let's also put AGEPH:coverp and AGEPH:powerc at the end, to see if they still contribiute 
# lets also put AGEPH:INS to the front

##GLMPoisIT1_2 <- glm(nbrtotc~AGEPH:INS+AGEPH:fuelc+AGEPH:split+AGEPH:agecar+AGEPH:sexp+AGEPH:coverp+AGEPH:powerc,data= Data, family=poisson(link="log"))
##anova(GLMPoisIT1_2,test="Chisq")

# lets add non-interaction terms with the best 3 interaction terms...

##GLMPoisIT1_2 <- glm(nbrtotc~AGEPH+agecar+sexp+fuelc+split+usec+fleetc+sportc+coverp+powerc+INS+AGEPH:INS+AGEPH:fuelc+AGEPH:split,data= Data, family=poisson(link="log"))
##anova(GLMPoisIT1_2,test="Chisq")

# interaction terms are less impact full at the end of our model,lets leave out sex, usec, fleetc and sportc. Lets also investigate agecar at the end of the list

GLMPoisIT1_3 <- glm(nbrtotc~AGEPH+INS+fuelc+split+coverp+powerc+AGEPH:INS+AGEPH:fuelc+AGEPH:split+agecar,data= Data, family=poisson(link="log"))
anova(GLMPois1Full,GLMPoisIT1_3,test="LRT")



# 1.4. Data cleaning and formatting for Gamma GLM ####

# load dataset with no outliers
DataCleaned <- Data[which(Data$chargtot  > 0),]
Tmax2=142112
Data_no_out <- DataCleaned[which(DataCleaned$chargtot < Tmax2),]
Data_no_out$avgCA <- Data_no_out$chargtot/Data_no_out$nbrtotc

# 1.5. Gamma GLMs & expected severity tables ####
# Gamma regression for severity

GLMGamma1Full <- glm(avgCA ~ AGEPH + agecar + sexp + fuelc + split + usec + fleetc + sportc + coverp + powerc + INS, offset = log(duree), data = Data_no_out, family = Gamma(link = "log"))
summary(GLMGamma1Full)

# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       8.455687   0.870605   9.712  < 2e-16 ***
# AGEPH37-56       -0.267664   0.143904  -1.860  0.06290 .  
# AGEPH57-76       -0.453512   0.183605  -2.470  0.01352 *  
# AGEPH>76          0.164497   0.454063   0.362  0.71715    
# agecar2-5        -0.593912   0.292204  -2.033  0.04212 *  
# agecar6-10       -0.607928   0.299407  -2.030  0.04233 *  
# agecar>10        -0.414939   0.319215  -1.300  0.19366    
# sexpMale          0.029173   0.142457   0.205  0.83774    
# fuelcPetrol       0.035443   0.136768   0.259  0.79553    
# splitThrice      -0.211652   0.259240  -0.816  0.41426    
# splitTwice       -0.466715   0.205592  -2.270  0.02321 *  
# splitOnce        -0.262378   0.194253  -1.351  0.17681    
# usecProfessional  0.135740   0.293651   0.462  0.64391    
# fleetcYes        -0.194543   0.380743  -0.511  0.60939    
# sportcNo          0.013520   0.622625   0.022  0.98268    
# coverpMTPL+      -0.426445   0.157609  -2.706  0.00682 ** 
# coverpMTPL+++     0.027975   0.218238   0.128  0.89800    
# powerc<66         0.202851   0.627862   0.323  0.74664    
# powerc66-110      0.425574   0.627848   0.678  0.49789    
# INSAntwerp        0.200052   0.275885   0.725  0.46838    
# INSBrabant & BXL -0.084290   0.252537  -0.334  0.73855    
# INSEast Flanders  0.008149   0.277311   0.029  0.97656    
# INSHainaut       -0.120498   0.251617  -0.479  0.63202    
# INSLiege         -0.149485   0.276137  -0.541  0.58828    
# INSLimburg        0.365422   0.355543   1.028  0.30406    
# INSLuxembourg    -0.091139   0.502082  -0.182  0.85596    
# INSNamur         -0.100887   0.358046  -0.282  0.77812    

GLMGamma2 <- glm(avgCA~AGEPH+agecar+split+coverp+AGEPH:split,offset=log(duree),data= Data_no_out, family=Gamma(link="log"))
summary(GLMGamma2)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             9.08624    0.41172  22.069  < 2e-16 ***
#  AGEPH37-56             -1.08219    0.36230  -2.987  0.00282 ** 
# AGEPH57-76             -0.87443    0.51877  -1.686  0.09189 .  
# AGEPH>76               -0.38163    3.26923  -0.117  0.90707    
# agecar2-5              -0.43270    0.29876  -1.448  0.14753    
# agecar6-10             -0.45236    0.30537  -1.481  0.13853    
# agecar>10              -0.32122    0.32446  -0.990  0.32218    
# splitThrice            -0.97627    0.38694  -2.523  0.01164 *  
# splitTwice             -0.96201    0.33758  -2.850  0.00438 ** 
# splitOnce              -0.83986    0.33675  -2.494  0.01264 *  
# coverpMTPL+            -0.45622    0.15630  -2.919  0.00352 ** 
# coverpMTPL+++           0.12078    0.21940   0.551  0.58198    
# AGEPH37-56:splitThrice  1.39863    0.55741   2.509  0.01211 *  
# AGEPH57-76:splitThrice  0.16673    0.89583   0.186  0.85236    
# AGEPH>76:splitThrice    0.32794    4.35939   0.075  0.94004    
# AGEPH37-56:splitTwice   0.79928    0.44277   1.805  0.07106 .  
# AGEPH57-76:splitTwice   0.37702    0.62429   0.604  0.54591    
# AGEPH>76:splitTwice     0.06008    3.40493   0.018  0.98592    
# AGEPH37-56:splitOnce    1.05700    0.43061   2.455  0.01411 *  
# AGEPH57-76:splitOnce    0.55214    0.57997   0.952  0.34110    
# AGEPH>76:splitOnce      0.68251    3.31449   0.206  0.83686   


# 1.6. Model selection ####

# 1.6.1 Poisson ####

# AIC/BIC

      # Calculate AIC
      AIC_GLMPois1Full <- AIC(GLMPois1Full)
      AIC_GLMPois2 <- AIC(GLMPois2)
      AIC_GLMPois3 <- AIC(GLMPois3)
      
      # Calculate BIC
      BIC_GLMPois1Full <- BIC(GLMPois1Full)
      BIC_GLMPois2 <- BIC(GLMPois2)
      BIC_GLMPois3 <- BIC(GLMPois3)
      
          
      # Print the AIC and BIC values
      cat("AIC for GLMPois1Full:", AIC_GLMPois1Full, "\n")
      cat("AIC for GLMPois2:", AIC_GLMPois2, "\n")
      cat("AIC for GLMPois3:", AIC_GLMPois3, "\n")
      
      cat("BIC for GLMPois1Full:", BIC_GLMPois1Full, "\n")
      cat("BIC for GLMPois2:", BIC_GLMPois2, "\n")
      cat("BIC for GLMPois3:", BIC_GLMPois3, "\n")
      
  # Deviance
  
    # GLMPois1Full
      deviance(GLMPois1Full)
    # GLMPois2
      deviance(GLMPois2)

    # GLMPois3Dscrtv2
      deviance(GLMPois3)
      
  # Drop in deviance
  
   # A first general look at the drop in deviance by starting from the model with only an intercept and than adding the covariates one by one. 
    # gives us a first indication of if the factor variable matter or not     
    anova(GLMPois1Full,test="Chisq")
      
    # Drop-in-deviance test between GLMPois1Full and GLMPois2 model.
    GLMPois2$deviance - GLMPois1Full$deviance
  
      pchisq(GLMPois2$deviance - GLMPois1Full$deviance, df = df.residual(GLMPois2)-df.residual(GLMPois1Full) , lower = F) #0.310899 Not significant 
      
      # Drop-in-deviance test between GLMPois1Full and GLMPois3 model.
      GLMPois3$deviance - GLMPois1Full$deviance
      
      pchisq(GLMPois3$deviance - GLMPois1Full$deviance, df = df.residual(GLMPois3)-df.residual(GLMPois1Full), lower = F) #0.02186997 Not Significant on the 99% CI, but significant on the 95% CI

     
      # Drop-in-deviance test between GLMPois2 and GLMPois3 model.
      GLMPois3$deviance - GLMPois2$deviance
      
      pchisq(GLMPois3$deviance - GLMPois2$deviance, df = df.residual(GLMPois3)-df.residual(GLMPois2), lower = F) #0.006875533 Significant
      
      
      # we accepted GLMPois2 and rejected GLMPois1 (by the first chi-sq test)
      # The residual deviance between GLMPois1 and GLMPois2 are negligible 
      
      # Why almost accept model 3 when comparing to model 1, but reject model 3 when comparing to model 2? 
      # we can explain this because the drop in degrees of freedom is higher from model 1 to model 3, this results in a higher q-parameter for the chi-sq test statistic..
      # this difference is smaller when comparing model 3 to 2. There the same increase in deviance holds, but the difference in degrees of freedom is smaller...
 
      
# 1.6.2 Gamma ####
      
  # AIC/BIC
      
     # Calculate AIC
      AIC_GLMGAM1Full <- AIC(GLMGamma1Full)
      AIC_GLMGAM2 <- AIC(GLMGamma2)

      # Calculate BIC
      BIC_GLMGAM1Full <- BIC(GLMGamma1Full)
      BIC_GLMGAM2 <- BIC(GLMGamma2)

      
      # Print the AIC and BIC values
      cat("AIC for GLMGamma1Full:", AIC_GLMGAM1Full, "\n")
      cat("AIC for GLMGamma2:", AIC_GLMGAM2, "\n")

      cat("BIC for GLMGamma1Full:", BIC_GLMGAM1Full, "\n")
      cat("BIC for GLMGamma2:", BIC_GLMGAM2, "\n")

      
  # Deviance
      
      # GLMPois1Full
      deviance(GLMGamma1Full)
      # GLMPois2
      deviance(GLMGamma2)
      
  # Gamma Regression selection
      
      # Drop-in-deviance test between GLMGamma1Full and GLMGamma2 model.
      GLMGamma2JVDE$deviance - GLMGamma1Full$deviance
      
      #we don't need the chi-squared test, as we can see that the deviance of model 2 is smaller, with a smaller number of parameters.
      #so our preference will always go to model 2 (as the p-value of 100% below can confirm)
      
      pchisq(GLMGamma2$deviance - GLMGamma1Full$deviance, df = df.residual(GLMGamma2)-df.residual(GLMGamma1Full), lower = F) #
      
# 1.7. Technical premium for each risk profile based on GLMs ####
      
      # Values linear predictor poisson regression per risk profile
      
      # Profile A
      PRLa <- sum(coef(GLMPois2)[c("(Intercept)","AGEPH57-76","agecar2-5","fuelcPetrol","splitOnce","fleetcYes","coverpMTPL+++","powerc<66","INSLuxembourg")])
      
      #Profile B
      PRLb <- sum(coef(GLMPois2)[c("(Intercept)","AGEPH37-56","agecar>10","fuelcPetrol","fleetcYes","powerc66-110","INSAntwerp")])
      
      #Profile C
      PRLc <- sum(coef(GLMPois2)[c("(Intercept)", "splitThrice", "INSBrabant & BXL")])
        
      # Expected frequency per risk profile
      
      # Profile A
      EFa <- exp(PRLa)
      
      #Profile B
      EFb <- exp(PRLb)
      
      #Profile C
      EFc <- exp(PRLc)
      
      # Values linear predictor poisson regression per risk profile
      # Profile A
      GRLa <- sum(coef(GLMGamma2)[c("(Intercept)","AGEPH57-76","agecar2-5","splitOnce","coverpMTPL+++","AGEPH57-76:splitOnce")])
      
      #Profile B
      GRLb <- sum(coef(GLMGamma2)[c("(Intercept)","AGEPH37-56","agecar>10")]) #interaction term AGEPH37-56:splitMonthly is ENCAPTURED in the intercept
      
      #Profile C
      GRLc<- sum(coef(GLMGamma2)[c("(Intercept)","splitThrice")]) #interaction term AGEPH17-36:splitThrice is ENCAPTURED in the intercept
      
      #Expected severity per risk profile
      
      # Profile A
      ESa <- exp(GRLa)
      
      #Profile B
      ESb <- exp(GRLb)
      
      #Profile C
      ESc<- exp(GRLc)
      
      RESULTS_TECHNICAL_PREMIUM <- data.frame(Linear_Predictor_PoissonR=c(PRLa,PRLb,PRLc),Linear_Predictor_GammaR=c(GRLa,GRLb,GRLc),Expected_Frequency=c(EFa,EFb,EFc),Expected_Severity=c(ESa,ESb,ESc))
      row.names(RESULTS_TECHNICAL_PREMIUM) <-c("Profile A","Profile B","Profile C")
      RESULTS_TECHNICAL_PREMIUM$Premium <- RESULTS_TECHNICAL_PREMIUM$Expected_Frequency*RESULTS_TECHNICAL_PREMIUM$Expected_Severity
      RESULTS_TECHNICAL_PREMIUM
      
#1.8. Risk loading calculations ####

# We will use model 2, and thus frequency table TARFR2 and severity table TARSV2 to calculate the premium.

      # Lambda (Poisson)
      summary(GLMPois2)
      mean <- coef(GLMPois2)
      variance<- vcov(GLMPois2)
      
      
      # Low risk (PROFILE A)
      Low_risk <-c("(Intercept)", "AGEPH57-76", "agecar2-5","sexpMale", "fuelcPetrol", "splitOnce", "fleetcYes", "coverpMTPL+++", "powerc<66", "INSLuxembourg")
      mean_Poislow <- mean[Low_risk]
      mean_Poislow <- sum(mean_Poislow)
      mean_Poislow
      Lambda_low <- exp(mean_Poislow)
      
      # Medium risk (PROFILE B)
      Medium_risk <- c("(Intercept)", "AGEPH37-56", "agecar>10","sexpMale", "fuelcPetrol","fleetcYes","powerc66-110","INSAntwerp")
      mean_Poismedium <- mean[Medium_risk]
      mean_Poismedium <- sum(mean_Poismedium)
      mean_Poismedium
      Lambda_medium<- exp(mean_Poismedium)
      
      # High risk (PROFILE C)
      High_risk <- c("(Intercept)", "splitThrice", "INSBrabant & BXL")
      mean_Poishigh <- mean[High_risk]
      mean_Poishigh <- sum(mean_Poishigh)
      mean_Poishigh
      Lambda_high<- exp(mean_Poishigh)
      
    # Alpha and Beta (Gamma) (To be updated once we have our GammaGLM) TBC
      
      #Variance
      
      variance_covarianceGamma <- vcov(GLMGamma2)
      
      # Low risk (PROFILE A)
      Low_risk_gamma <-c("(Intercept)", "AGEPH57-76", "agecar2-5", "splitOnce", "coverpMTPL+++","AGEPH57-76:splitOnce")
      variance_covariance_Gammalow <- variance_covarianceGamma[Low_risk_gamma, Low_risk_gamma]
      Variance_gamma_low <- sum(variance_covariance_Gammalow)

      # Medium risk (PROFILE B)
      Medium_risk_gamma <- c("(Intercept)", "AGEPH37-56", "agecar>10","AGEPH37-56")
      variance_covariance_Gammamedium <- variance_covarianceGamma[Medium_risk_gamma, Medium_risk_gamma]
      Variance_gamma_medium <- sum(variance_covariance_Gammamedium)
      
      # High risk (PROFILE C)
      High_risk_gamma <- c("(Intercept)", "splitThrice")
      variance_covariance_Gammahigh <- variance_covarianceGamma[High_risk_gamma, High_risk_gamma]
      Variance_gamma_high <- sum(variance_covariance_Gammahigh)         

      # mean 
      
      mean_Gamma <- coef(summary(GLMGamma2))[, 1]
      
      # Low risk (PROFILE A)
      Mean_Gammalow <- mean_Gamma[Low_risk_gamma]
      Mean_low <- sum(Mean_Gammalow)
      
      # Medium risk (PROFILE B)
      Mean_Gammamedium <- mean_Gamma[Medium_risk_gamma]
      Mean_medium <- sum(Mean_Gammamedium)
      
      # High risk (PROFILE C)
      Mean_Gammahigh <- mean_Gamma[High_risk_gamma]
      Mean_high <- sum(Mean_Gammahigh)   
      
      #alpha <- (Mean_gamma / Variance_gamma)^2
      # beta <- Mean_gamma / Variance_gamma

      #alpha & beta
      # Low risk 
      alpha_low <- (Mean_low/Variance_gamma_low)^2
      alpha_low
      beta_low <- (Mean_low/Variance_gamma_low)
      beta_low
      
      # Medium risk
 
      alpha_medium <- (Mean_medium/Variance_gamma_medium)^2
      alpha_medium
      beta_medium <- (Mean_medium/Variance_gamma_medium)
      beta_medium
      
      
      # High risk
      alpha_high <- (Mean_high/Variance_gamma_high)^2
      alpha_high
      beta_high <- (Mean_high/Variance_gamma_high)
      beta_high
      
        
# Mean and variance loss function
        
  # Low risk
    Mean_L_low <- Lambda_low*(alpha_low/beta_low)
    Mean_L_low
    Variance_L_low <- Lambda_low*(alpha_low/(beta_low^2))+Lambda_low*(alpha_low/beta_low)^2+(alpha_low/(beta_low^2))*Lambda_low^2
    Variance_L_low
    # Medium risk
    Mean_L_medium <- Lambda_medium*(alpha_medium/beta_medium)
    Mean_L_medium
    Variance_L_medium <- Lambda_medium*(alpha_medium/(beta_medium^2))+Lambda_medium*(alpha_medium/beta_medium)^2+(alpha_medium/(beta_medium^2))*Lambda_medium^2
    Variance_L_medium
    # high risk
    Mean_L_high <- Lambda_high*(alpha_high/beta_high)
    Mean_L_high
    Variance_L_high <- Lambda_high*(alpha_high/(beta_high^2))+Lambda_high*(alpha_high/beta_high)^2+(alpha_high/(beta_high^2))*Lambda_high^2
    Variance_L_high

# Risk loaded premium
    
    # Expected Value Principle
      #low
      (1+0.01)*Mean_L_low
      #medium
      (1+0.01)*Mean_L_medium
      #high
      (1+0.01)*Mean_L_high
    # Variance Principle
      #low
      Mean_L_low+1.5*(Variance_L_low)
      #medium
      Mean_L_medium+1.5*(Variance_L_medium)
      #high
      Mean_L_high+1.5*(Variance_L_high)
    # Standard Deviation Principle
      #low
      Mean_L_low+3*sqrt(Variance_L_low)
      #medium
      Mean_L_medium+3*sqrt(Variance_L_medium)
      #high
      Mean_L_high+3*sqrt(Variance_L_high)
    # Compromise principle
      #low
      Mean_L_low+0.75*sqrt(Variance_L_low)+1.5*(Variance_L_low)
      #medium
      Mean_L_medium+0.75*sqrt(Variance_L_medium)+1.5*(Variance_L_medium)
      #high
      Mean_L_high+0.75*sqrt(Variance_L_high)+1.5*(Variance_L_high)

