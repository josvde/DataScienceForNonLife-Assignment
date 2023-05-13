##########################################
## Assignment Data science for None-Life##
##########################################

## Dataframes
# Data -> Data provided on Toledo
# DataCleaned -> Data where policyholders that reported zero claims are omitted 
# Data_no_out -> DataCleaned + removal outliers
# Tmax -> Claim amounts above this value is considered as an outlier



############## Import Data ##############

library(ggplot2)
library(data.table)
library(dplyr)

# 0. Import data

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
rm(dir)

### 0.1 Data of "Assinment.csv" and "inspost.xls" merged using VLOOKUP

### 0.2 import dataframe

Data <- as.data.table(read.csv("DataCombined.csv", header=TRUE, sep=";", dec="."))

############## Frequency ##############

frequency <- Data$nbrtotc / Data$duree #number of claims during (period of) exposure di. Please not the second observation of (very) high (21). This is because one claim happened during a very short period (di=0.04)
DataFreq <- cbind(Data, frequency)
table(Data$nbrtotc) #number of claims
sum(Data$nbrtotc)

avg_freq <- sum(DataFreq$nbrtotc)/sum(DataFreq$duree)
avg_freq #claim mean = 0.134 per year
Frequec
#### Frequency of claims per variable #### 

# Age of policyholder

frequency_by_age <- DataFreq %>%
  group_by(AGEPH) %>%
  summarise(avg_freq = sum(nbrtotc)/sum(duree))

print(frequency_by_age,n = 79)
summary(frequency_by_age)

Freq.age <- ggplot(frequency_by_age, aes(x = AGEPH, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency by age policyholder")
Freq.age

# Age car

frequency_by_agecar <- DataFreq %>%
  group_by(agecar) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_agecar
summary(frequency_by_agecar)

Freq.agecar <- ggplot(frequency_by_agecar, aes(x = agecar, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency by age policyholder")


# Gender

frequency_by_sex <- DataFreq %>%
  group_by(sexp) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_sex 

Freq.sex <- ggplot(frequency_by_sex, aes(x = sexp, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency by per gender")



# Type of fuel

frequency_by_fuel <- DataFreq %>%
  group_by(fuelc) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_fuel 

Freq.fuel <- ggplot(frequency_by_fuel, aes(x = fuelc, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per fuel type")


# Split of the premium

frequency_by_split <- DataFreq %>%
  group_by(split) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_split

Freq.split <- ggplot(frequency_by_split, aes(x = split, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per split")


# Use of the car

frequency_by_use <- DataFreq %>%
  group_by(usec) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_use 

Freq.use <- ggplot(frequency_by_use, aes(x = usec, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per use of the car")
Freq.use

# Car beloning to a Fleet

frequency_by_fleet <- DataFreq %>%
  group_by(fleetc) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_fleet

Freq.fleet <- ggplot(frequency_by_fleet, aes(x = fleetc, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per fleet type")
Freq.fleet

# Sport car

frequency_by_sport <- DataFreq %>%
  group_by(sportc) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_sport

Freq.sportc <- ggplot(frequency_by_sport, aes(x = sportc, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per sport type")


# Coverage

frequency_by_cover <- DataFreq %>%
  group_by(coverp) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_cover 

Freq.cover <- ggplot(frequency_by_cover, aes(x = coverp, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per coverage type")


# Power of the car

frequency_by_power <- DataFreq %>%
  group_by(powerc) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_power

Freq.power <- ggplot(frequency_by_power, aes(x = powerc, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per power type")

# Location of policyholder

frequency_by_Location <- DataFreq %>%
  select(LONG, LAT,frequency)

Freq.location <- ggplot(frequency_by_Location, aes(x = LONG, y = LAT, color=frequency)) + 
  geom_point() +
  labs(title="Claim frequency by location")

## Plot Frequency graphs

# [RD] I have troubles combined the different plots in one, view. Somebody who can help?
Freq.age
Freq.agecar
Freq.sex
Freq.fuel
Freq.split
Freq.sportc
Freq.cover
Freq.power
Freq.location


############## Severity ##############
#### Severity of claims per variable #### 

avgr_claim <- Data$chargtot / Data$nbrtotc
DataSev <- cbind(Data, avgr_claim)
DataSev

## remove observations without no claims

DataCleaned <- DataSev[which(DataSev$nbrtotc  > 0),]
DataCleaned <- DataSev[which(DataSev$chargtot  > 0),]


## Check for outliers #Boys, not sure how we can detect outliers in a assymetric distribution. 

boxplot(DataCleaned$chargtot) #many outliers, one extreme outlier 1989567.9 
summary(DataCleaned$chargtot) #summary says that there are claims amounts equal to 0.0 but this is due to rounding. Is it not strange that we have chargetot of 0.02


# get mean and Standard deviation
mean = mean(DataCleaned$chargtot)
std = sd(DataCleaned$chargtot)

# get threshold values for outlines
Tmax = mean+(3*std) 
Tmax #outliers are all values above 54 286

# find outlier
table(Data$chargtot > Tmax)["TRUE"]

Data_no_out <- DataCleaned[which(DataCleaned$chargtot < Tmax),]

boxplot(Data_no_out$chargtot)

#### Severity of claims per variable #### 

# Age of policyholder

severity_by_age <- Data_no_out %>%
  group_by(AGEPH) %>%
  summarise(Claim_sev = mean(avgr_claim))

print(severity_by_age,n=79)
summary(severity_by_age)

Sev.age <- ggplot(severity_by_age, aes(x = AGEPH, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim severity by age policyholder")


# Age car

severity_by_agecar <- Data_no_out %>%
  group_by(agecar) %>%
  summarize(Claim_sev = mean(avgr_claim))

severity_by_agecar
summary(severity_by_agecar)

Sev.agecar <- ggplot(severity_by_agecar, aes(x = agecar, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim severity by age policyholder")


# Gender

# [RD] Can somebody double check my code. In the code of Liesbeth, the results are the other way around

severity_by_sex <- Data_no_out %>%
  group_by(sexp) %>%
  summarize(Claim_sev = mean(avgr_claim))

severity_by_sex 

Sev.sex <- ggplot(severity_by_sex, aes(x = sexp, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim severity by per gender")



# Type of fuel

severity_by_fuel <- Data_no_out %>%
  group_by(fuelc) %>%
  summarize(Claim_sev = mean(avgr_claim))

severity_by_fuel 

Sev.fuel <- ggplot(severity_by_fuel, aes(x = fuelc, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim severity per fuel type")


# Split of the premium

severity_by_split <- Data_no_out %>%
  group_by(split) %>%
  summarize(Claim_sev = mean(avgr_claim))

severity_by_split

Sev.split <- ggplot(severity_by_split, aes(x = split, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim severity per split")


# Use of the car

severity_by_use <- Data_no_out %>%
  group_by(usec) %>%
  summarize(Claim_sev = mean(avgr_claim))

severity_by_use 

Sev.use <- ggplot(severity_by_use, aes(x = usec, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim severity per use of the car")
Sev.use

# Car beloning to a Fleet

severity_by_fleet <- Data_no_out %>%
  group_by(fleetc) %>%
  summarize(Claim_sev = mean(avgr_claim))

severity_by_fleet

Sev.fleet <- ggplot(severity_by_fleet, aes(x = fleetc, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim severity per fleet type")
Sev.fleet

# Sport car

severity_by_sport <- Data_no_out %>%
  group_by(sportc) %>%
  summarize(Claim_sev = mean(avgr_claim))

severity_by_sport

Sev.sportc <- ggplot(severity_by_sport, aes(x = sportc, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim severity per sport type")


# Coverage

severity_by_cover <- Data_no_out %>%
  group_by(coverp) %>%
  summarize(Claim_sev = mean(avgr_claim))

severity_by_cover 

Sev.cover <- ggplot(severity_by_cover, aes(x = coverp, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim severity per coverage type")


# Power of the car

severity_by_power <- Data_no_out %>%
  group_by(powerc) %>%
  summarize(Claim_sev = mean(avgr_claim))

severity_by_power

Sev.power <- ggplot(severity_by_power, aes(x = powerc, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim severity per power type")

# Location of policyholder

severity_by_Location <- Data_no_out %>%
  select(LONG, LAT,avgr_claim)

Sev.location <- ggplot(severity_by_Location, aes(x = LONG, y = LAT, color=avgr_claim)) + 
  geom_point() +
  labs(title="Claim severity by location")

## Plot severity graphs

# [RD] I have troubles combined the different plots in one, view. Somebody who can help?
Sev.age
Sev.agecar
Sev.sex
Sev.fuel
Sev.split
Sev.sportc
Sev.cover
Sev.power
Sev.location


