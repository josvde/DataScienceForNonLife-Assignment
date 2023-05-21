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

# load dataset with no outliers
DataCleaned <- Data[which(Data$chargtot  > 0),]
Tmax2=142112
Data_no_out <- DataCleaned[which(DataCleaned$chargtot < Tmax2),]

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

# create GLM3PoisDscrtv based on a selection of covariates based on the descriptive analysis (see section 2.5)

GLMPois3Dscrtv <- glm(nbrtotc~AGEPH+agecar+fuelc+fleetc+coverp+powerc+INS,offset=log(duree),data= Data, family=poisson(link="log"))
summary(GLMPois3Dscrtv)

# Output GLMPois3Dscrtv
# Coefficients:
#                    Estimate    Std. Error z value Pr(>|z|)    
#   (Intercept)      -1.168133   0.092175  -12.673  < 2e-16 ***
#   AGEPH37-56       -0.320574   0.015945  -20.106  < 2e-16 ***
#   AGEPH57-76       -0.574470   0.020118  -28.556  < 2e-16 ***
#   AGEPH>76         -0.455401   0.051771  -8.796   < 2e-16 ***
#   agecar2-5        -0.253180   0.032948  -7.684  1.54e-14 ***
#   agecar6-10       -0.193536   0.033616  -5.757  8.55e-09 ***
#   agecar>10        -0.152046   0.035725  -4.256  2.08e-05 ***
#   fuelcPetrol      -0.175291   0.015246  -11.498  < 2e-16 ***
#   fleetcYes        -0.147814   0.043054  -3.433  0.000596 ***
#   sportcNo         -0.090198   0.070534  -1.279  0.200975    
#   coverpMTPL+      -0.129427   0.017635  -7.339  2.15e-13 ***
#   coverpMTPL+++    -0.126873   0.024512  -5.176  2.27e-07 ***
#   powerc<66        -0.229338   0.070373  -3.259  0.001118 ** 
#   powerc66-110     -0.147023   0.070427  -2.088  0.036834 *  
#   INSAntwerp        0.082053   0.031062   2.642  0.008253 ** 
#   INSBrabant & BXL  0.346496   0.027849  12.442   < 2e-16 ***
#   INSEast Flanders  0.123843   0.031192   3.970  7.18e-05 ***
#   INSHainaut        0.103185   0.027797   3.712  0.000206 ***
#   INSLiege          0.191432   0.030527   6.271  3.59e-10 ***
#   INSLimburg       -0.000784   0.040107  -0.020  0.984403    
#   INSLuxembourg    -0.121341   0.056349  -2.153  0.031287 *  
#   INSNamur          0.009628   0.040053   0.240  0.810030 

# create GLM3PoisDscrtv2 where we only leave out sexp compared to GLM2

GLMPois3  <- glm(nbrtotc~AGEPH+agecar+fuelc+split+fleetc+sportc+coverp+powerc+INS,offset=log(duree),data= Data, family=poisson(link="log"))

summary(GLMPois3)

# Output GLMPois3
# Coefficients:

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      -0.92408    0.09465  -9.764  < 2e-16 ***
#  AGEPH37-56       -0.28901    0.01607 -17.982  < 2e-16 ***
#  AGEPH57-76       -0.49167    0.02058 -23.886  < 2e-16 ***
# AGEPH>76         -0.32317    0.05223  -6.188 6.11e-10 ***
# agecar2-5        -0.25048    0.03294  -7.603 2.89e-14 ***
# agecar6-10       -0.19397    0.03362  -5.770 7.92e-09 ***
# agecar>10        -0.17652    0.03577  -4.935 8.01e-07 ***
# fuelcPetrol      -0.17545    0.01525 -11.504  < 2e-16 ***
# splitThrice       0.09250    0.02895   3.196 0.001394 ** 
# splitTwice       -0.13370    0.02319  -5.766 8.13e-09 ***
# splitOnce        -0.33079    0.02187 -15.128  < 2e-16 ***
# fleetcYes        -0.10084    0.04323  -2.333 0.019667 *  
# sportcNo         -0.08866    0.07058  -1.256 0.209063    
# coverpMTPL+      -0.13789    0.01767  -7.806 5.92e-15 ***
# coverpMTPL+++    -0.15856    0.02472  -6.415 1.41e-10 ***
# powerc<66        -0.24287    0.07037  -3.451 0.000558 ***
# powerc66-110     -0.16264    0.07042  -2.310 0.020907 *  
# INSAntwerp        0.03698    0.03128   1.182 0.237107    
# INSBrabant & BXL  0.25876    0.02832   9.138  < 2e-16 ***
# INSEast Flanders  0.11078    0.03121   3.549 0.000386 ***
# INSHainaut        0.01073    0.02839   0.378 0.705411    
# INSLiege          0.08663    0.03108   2.787 0.005313 ** 
# INSLimburg       -0.04103    0.04021  -1.021 0.307445    
# INSLuxembourg    -0.21773    0.05663  -3.844 0.000121 ***
# INSNamur         -0.09645    0.04053  -2.380 0.017317 *   

# dataframes with the expected frequency numbers for reference group based on different GLMs and the correction values for other factor levels

TARFR1 <- data.frame(Name=names(coefficients(GLMPois1Full)),E_Freq=exp(coefficients(GLMPois1Full)))
TARFR2 <- data.frame(Name=names(coefficients(GLMPois2)),E_Freq=exp(coefficients(GLMPois2)))
TARFR3 <- data.frame(Name=names(coefficients(GLMPois3Dscrtv)),E_Freq=exp(coefficients(GLMPois3Dscrtv)))
TARFR4 <- data.frame(Name=names(coefficients(GLMPois3)),E_Freq=exp(coefficients(GLMPois3)))

# 1.4. Draft section: one dummy gamma uploading for testing model selection and risk loading####

GLMGamma1Full <- glm(chargtot ~ AGEPH + agecar + sexp + fuelc + split + usec + fleetc + sportc + coverp + powerc + INS, offset = log(duree), data = Data_no_out, family = Gamma(link = "log"))
summary(GLMPois1Full)


# 1.5. Gamma GLMs & expected severity tables ####
# Gamma regression for severity

# 1.6. Model selection ####

# 1.6.1 Poisson ####

# AIC/BIC

      # Calculate AIC
      AIC_GLMPois1Full <- AIC(GLMPois1Full)
      AIC_GLMPois2 <- AIC(GLMPois2)
      AIC_GLMPois3Dscrtv <- AIC(GLMPois3Dscrtv)
      AIC_GLMPois3 <- AIC(GLMPois3)
      
      # Calculate BIC
      BIC_GLMPois1Full <- BIC(GLMPois1Full)
      BIC_GLMPois2 <- BIC(GLMPois2)
      BIC_GLMPois3Dscrtv <- BIC(GLMPois3Dscrtv)
      BIC_GLMPois3 <- BIC(GLMPois3)
      
          
      # Print the AIC and BIC values
      cat("AIC for GLMPois1Full:", AIC_GLMPois1Full, "\n")
      cat("AIC for GLMPois2:", AIC_GLMPois2, "\n")
      cat("AIC for GLMPois3Dscrtv:", AIC_GLMPois3Dscrtv, "\n")
      cat("AIC for GLMPois3:", AIC_GLMPois3, "\n")
      
      cat("BIC for GLMPois1Full:", BIC_GLMPois1Full, "\n")
      cat("BIC for GLMPois2:", BIC_GLMPois2, "\n")
      cat("BIC for GLMPois3Dscrtv:", BIC_GLMPois3Dscrtv, "\n")
      cat("BIC for GLMPois3:", BIC_GLMPois3, "\n")
      
  # Deviance
  
    # GLMPois1Full
      deviance(GLMPois1Full)
    # GLMPois2
      deviance(GLMPois2)
    # GLMPois3Dscrtv
      deviance(GLMPois3Dscrtv)
    # GLMPois3Dscrtv2
      deviance(GLMPois3)
      
  # Drop in deviance
  
   # A first general look at the drop in deviance by starting from the model with only an intercept and than adding the covariates one by one. 
    # gives us a first indication of if the factor variable matter or not     
    anova(GLMPois1Full,test="Chisq")
      
    # Drop-in-deviance test between GLMPois1Full and GLMPois2 model.
      GLMPois2$deviance - GLMPois1Full$deviance
  
      pchisq(GLMPois2$deviance - GLMPois1Full$deviance, df = df.residual(GLMPois2)-df.residual(GLMPois1Full) , lower = F) #0.310899 Not significant 
      
      # Drop-in-deviance test between GLMPois1Full and GLMPois3Dscrtv model.
      GLMPois3Dscrtv$deviance - GLMPois1Full$deviance
      
      pchisq(GLMPois3Dscrtv$deviance - GLMPois1Full$deviance, df = df.residual(GLMPois3Dscrtv)-df.residual(GLMPois1Full), lower = F) #1.709847e-89 Significant on the 95% CI -> Thus going from current model to full improves the fit significantly 
  
      # Drop-in-deviance test between GLMPois1Full and GLMPois3 model.
      GLMPois3$deviance - GLMPois1Full$deviance
      
      pchisq(GLMPois3$deviance - GLMPois1Full$deviance, df = df.residual(GLMPois3)-df.residual(GLMPois1Full), lower = F) #0.01742042 Not Significant on the 99% CI, but significant on the 95% CI 

# 1.7. Technical premium for each risk profile based on GLMs ####

# We will use model X, and thus frequency table TARFRX and severity table TARSVX to calculate the premium.
      
  # Lambda (Poisson)
      variance_covariancePois <- vcov(GLMPois3)

      # Low risk
      Low_risk <-c("(Intercept)", "AGEPH57-76", "fuelcPetrol", "splitOnce", "fleetcYes", "fleetcYes", "coverpMTPL+++", "powerc<66", "INSLuxembourg")
      variance_covariance_Poislow <- variance_covariancePois[Low_risk, Low_risk]
      Lambda_low <- sum(variance_covariance_Poislow)
      
      # Medium risk
      Medium_risk <- c("(Intercept)", "AGEPH37-56", "agecar>10", "fuelcPetrol","fleetcYes","coverpMTPL+","powerc66-110","INSAntwerp")
      variance_covariance_Poismedium <- variance_covariancePois[Medium_risk, Medium_risk]
      Lambda_medium <- sum(variance_covariance_Poismedium)
  
      # High risk
      High_risk <- c("(Intercept)", "splitThrice", "INSBrabant & BXL")
      variance_covariance_Poishigh <- variance_covariancePois[High_risk, High_risk]
      Lambda_high <- sum(variance_covariance_Poishigh)      
      
      
    # Alpha and Beta (Gamma) (To be updated once we have our GammaGLM) TBC
      
      #Variance
      
      variance_covarianceGamma <- vcov(GLMPois3)
      # Low risk
      variance_covariance_Gammalow <- variance_covarianceGamma[Low_risk, Low_risk]
      Variance_gamma_low <- sum(variance_covariance_Gammalow)

      # Medium risk
      variance_covariance_Gammamedium <- variance_covarianceGamma[Medium_risk, Medium_risk]
      Variance_gamma_medium <- sum(variance_covariance_Gammamedium)
      
      # High risk
      variance_covariance_Gammahigh <- variance_covarianceGamma[High_risk, High_risk]
      Variance_gamma_high <- sum(variance_covariance_Gammahigh)         

      # mean 
      
      mean_Gamma <- coef(summary(GLMPois3))[, 1]
      
      # Low risk
      Mean_Gammalow <- mean_Gamma[Low_risk]
      Mean_low <- sum(Mean_Gammalow)
      
      # Medium risk
      Mean_Gammamedium <- mean_Gamma[Medium_risk]
      Mean_medium <- sum(Mean_Gammamedium)
      
      # High risk
      Mean_Gammahigh <- mean_Gamma[High_risk]
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

# JF calculated the final frequency value already in a Word table. Check if R reports the same value!

# 2. Extra ####
# Investigate what would be relevant and appropriate interaction terms of 2 or more covariates
# Do this for the Poisson GLM as well as the Gamma GLM.
