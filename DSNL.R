##########################################
## Assignment Data science for None-Life##
##########################################


############## Import Data ##############


# 0. Import data

setwd("C:/Users/rdessein/oneDrive - Deloitte (O365D)/Documents/GitHub/DataScienceForNonLife-Assignment")

### 0.1 Data of "Assinment.csv" and "inspost.xls" merged using VLOOKUP

### 0.2 import dataframe

Data <- as.data.table(read.csv("DataCombined.csv", header=TRUE, sep=";", dec="."))

############## Frequency ##############

frequency <- Data$nbrtotc / Data$duree #number of claims during (period of) exposure di. Please not the second observation of (very) high (21). This is because one claim happened during a very short period (di=0.04)
DataFreq <- cbind(Data, frequency)
table(Data$nbrtotc) #number of claims

avg_freq <- sum(DataFreq$nbrtotc)/sum(DataFreq$duree)
avg_freq #claim mean = 0.134 per year

#### Frequency of claims per variable #### 

# wrapper functions

# +++ Charts to be added +++ #

# Age of policyholder

frequency_by_age <- DataFreq %>%
  group_by(AGEPH) %>%
  summarise(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_age
summary(frequency_by_age)

Freq_age <- ggplot(frequency_by_age, aes(x = AGEPH, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency by age policyholder")


# Age car

frequency_by_agecar <- DataFreq %>%
  group_by(agecar) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_agecar
summary(frequency_by_agecar)

Freq_agecar <- ggplot(frequency_by_agecar, aes(x = agecar, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency by age policyholder")
Freq_agecar

# Gender

frequency_by_sex <- DataFreq %>%
  group_by(sexp) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_sex 

plot.freq.sex <- ggplot(frequency_by_sex, aes(x = sexp, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency by per gender")
plot.freq.sex


# Fueltype

frequency_by_fuel <- DataFreq %>%
  group_by(fuelc) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_fuel 

plot.freq.fuel <- ggplot(frequency_by_fuel, aes(x = fuelc, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per fuel type")
plot.freq.fuel

# split

frequency_by_split <- DataFreq %>%
  group_by(split) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_split

plot.freq.split <- ggplot(frequency_by_split, aes(x = split, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per split")
plot.freq.split

# Use of the car

frequency_by_use <- DataFreq %>%
  group_by(usec) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_use 

plot.freq.use <- ggplot(frequency_by_use, aes(x = usec, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per use of the car")
plot.freq.use

# Fleet

frequency_by_fleet <- DataFreq %>%
  group_by(fleetc) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_fleet

plot.freq.fleet <- ggplot(frequency_by_fleet, aes(x = fleetc, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per fleet type")
plot.freq.fleet

# Sport

frequency_by_sport <- DataFreq %>%
  group_by(sportc) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_sport

plot.freq.sportc <- ggplot(frequency_by_sport, aes(x = sportc, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per sport type")
plot.freq.sportc

# Cover

frequency_by_cover <- DataFreq %>%
  group_by(coverp) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_cover 

plot.freq.cover <- ggplot(frequency_by_cover, aes(x = coverp, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per cover type")
plot.freq.cover


# Power

frequency_by_power <- DataFreq %>%
  group_by(powerc) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_power

plot.freq.power <- ggplot(frequency_by_power, aes(x = powerc, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim frequency per power type")
plot.freq.power

