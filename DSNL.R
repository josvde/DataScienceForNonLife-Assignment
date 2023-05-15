##########################################
## Assignment Data science for None-Life ##
##########################################


# 0. Import data

setwd("C:/Users/rdessein/oneDrive - Deloitte (O365D)/Documents/GitHub/DataScienceForNonLife-Assignment")

### 0.1 Data of "Assinment.csv" and "inspost.xls" merged using VLOOKUP

### 0.2 import dataframe
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyverse) # for data manipulation
library(sf) # for spatial data manipulation
library(tmap) # for creating maps
library(gridExtra)
library(grid)
library(scales)


dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
rm(dir)

Data <- as.data.table(read.csv("DataCombined.csv", header=TRUE, sep=";", dec="."))

## Frequency

  frequency <- Data$nbrtotc / Data$duree #number of claims during (period of) exposure di. Please not the second observation of (very) high (21). This is because one claim happened during a very short period (di=0.04)
  DataFreq <- cbind(Data, frequency)
  table(Data$nbrtotc) #number of claims
  sum(Data$nbrtotc)
  
  avg_freq <- sum(DataFreq$nbrtotc)/sum(DataFreq$duree)
  avg_freq #claim mean = 0.134 per year

## Severity

  severity <- Data$chargtot / Data$nbrtotc
  DataSev <- cbind(Data, severity)
  DataSev
  
############ Section 2.1 Frequency Data #############

#### 2.1.1 Age of policyholder ####

  frequency_by_age <- DataFreq %>%
    group_by(AGEPH) %>%
    summarise(avg_freq = sum(nbrtotc)/sum(duree))
  
  print(frequency_by_age,n = 79)
  summary(frequency_by_age)
  
  Freq.age <- ggplot(frequency_by_age, aes(x = AGEPH, y = avg_freq)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Age policyholder") +
    labs(x = "", y = "Claim Frequency") +
    theme(plot.title = element_text(hjust = 0.5))

#### 2.1.2 Age of car ####
  
  frequency_by_agecar <- DataFreq %>%
    group_by(agecar) %>%
    summarize(avg_freq = sum(nbrtotc)/sum(duree))
  
  frequency_by_agecar
  summary(frequency_by_agecar)

  # Convert agecar to a factor with desired order
  frequency_by_agecar$agecar <- factor(frequency_by_agecar$agecar, levels = unique(frequency_by_agecar$agecar))
  
  Freq.agecar <- ggplot(frequency_by_agecar, aes(x = agecar, y = avg_freq)) +
    theme_bw() +
    geom_bar(stat = "Age Car", alpha = 0.5) +
    ggtitle("Age policyholder") +
    labs(x = "", y = "Claim Frequency") +
    theme(plot.title = element_text(hjust = 0.5))

#### 2.1.3 Gender of policyholder ####
  
  frequency_by_sex <- DataFreq %>%
    group_by(sexp) %>%
    summarize(avg_freq = sum(nbrtotc)/sum(duree))
  
  frequency_by_sex 
  
  Freq.sex <- ggplot(frequency_by_sex, aes(x = sexp, y = avg_freq)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Gender") +
    labs(x = "", y = "Claim Frequency") +
    theme(plot.title = element_text(hjust = 0.5))


#### 2.1.4 Type of fuel ####
  
  frequency_by_fuel <- DataFreq %>%
    group_by(fuelc) %>%
    summarize(avg_freq = sum(nbrtotc)/sum(duree))
  
  frequency_by_fuel 
  
  Freq.fuel <- ggplot(frequency_by_fuel, aes(x = fuelc, y = avg_freq)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Fuel type") +
    labs(x = "", y = "Claim Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
  
#### 2.1.5 Frequency premium ####
  

  frequency_by_split <- DataFreq %>%
    group_by(split) %>%
    summarize(avg_freq = sum(nbrtotc)/sum(duree))
  
  frequency_by_split
  
  Freq.split <- ggplot(frequency_by_split, aes(x = split, y = avg_freq)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Age policyholder") +
    labs(x = "", y = "Claim split") +
    theme(plot.title = element_text(hjust = 0.5))

#### 2.1.6 Use of Car ####
  
  frequency_by_use <- DataFreq %>%
    group_by(usec) %>%
    summarize(avg_freq = sum(nbrtotc)/sum(duree))
  
  frequency_by_use 
  
  Freq.use <- ggplot(frequency_by_use, aes(x = usec, y = avg_freq)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Use of the car") +
    labs(x = "", y = "Claim Frequency") +
    theme(plot.title = element_text(hjust = 0.5))

#### 2.1.7 Fleet ####
  
  frequency_by_fleet <- DataFreq %>%
    group_by(fleetc) %>%
    summarize(avg_freq = sum(nbrtotc)/sum(duree))
  
  frequency_by_fleet
  
  Freq.fleet <- ggplot(frequency_by_fleet, aes(x = fleetc, y = avg_freq)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Fleet type") +
    labs(x = "", y = "Claim Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
  
#### 2.1.8 Sportcar ####

  frequency_by_sport <- DataFreq %>%
    group_by(sportc) %>%
    summarize(avg_freq = sum(nbrtotc)/sum(duree))
  
  frequency_by_sport
  
  Freq.sportc <- ggplot(frequency_by_sport, aes(x = sportc, y = avg_freq)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Sport type") +
    labs(x = "", y = "Claim Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
  
#### 2.1.9 Coverage type ####
  
  frequency_by_cover <- DataFreq %>%
    group_by(coverp) %>%
    summarize(avg_freq = sum(nbrtotc)/sum(duree))
  
  frequency_by_cover 
  
  Freq.cover <- ggplot(frequency_by_cover, aes(x = coverp, y = avg_freq)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Coverage") +
    labs(x = "", y = "Claim Frequency") +
    theme(plot.title = element_text(hjust = 0.5))
  
#### 2.1.10 Horsepower of the car ####
  
frequency_by_power <- DataFreq %>%
  group_by(powerc) %>%
  summarize(avg_freq = sum(nbrtotc)/sum(duree))

frequency_by_power

Freq.power <- ggplot(frequency_by_power, aes(x = powerc, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Horsepower") +
  labs(x = "", y = "Claim Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

#### 2.1.11 Location of policyholder  ####
  # municipal level (including map based on Long/Lat)

frequency_by_Location_muni <- DataFreq %>%
  select(LONG, LAT,frequency)

Freq.location_muni <- ggplot(frequency_by_Location_muni, aes(x = LONG, y = LAT, color=frequency,size = frequency)) + 
  geom_point(alpha = 0.5) +
  labs(title="Claim frequency by location (municipal)", color = "Frequency")

  # district level

DataFreq_INS2 <- DataFreq %>%
  mutate(INS = substr(INS, 1, 2))

INS_freq <- DataFreq %>%
  group_by(INS) %>%
  summarise(avg_freq = sum(nbrtotc)/sum(duree))

print(INS_freq)

  # province level

DataFreq_INS1 <- DataFreq %>%
  mutate(INS = substr(INS, 1, 1))

INS_freq <- DataFreq %>%
  group_by(INS) %>%
  summarise(avg_freq = sum(nbrtotc)/sum(duree))

print(INS_freq)


#### Plot Frequency graphs  ####

grid.arrange(Freq.age, nrow = 1)
grid.arrange(Freq.agecar, Freq.sex, Freq.fuel, Freq.split, Freq.use, Freq.fleet, Freq.sportc, Freq.cover, Freq.power, nrow = 3)
grid.arrange(Freq.location_muni, nrow = 1)

############ Section 2.2 Severity Data #############


#### 2.2.0 Remove outliers  ####

  ## remove observations without no claims
  
    DataCleaned <- DataSev[which(DataSev$nbrtotc  > 0),]
    DataCleaned <- DataSev[which(DataSev$chargtot  > 0),]
  
  
  ## Check for outliers 
    boxplot(DataCleaned$chargtot) #many outliers, one extreme outlier 1989567.9 
    hist(DataCleaned$chargtot)
    
    
    # get mean and Standard deviation
      mean = mean(DataCleaned$chargtot)
      std = sd(DataCleaned$chargtot)
    
    # get threshold values for outlines
      Tmax = mean+(3*std) 
      Tmax #outliers are all values above 54 286
      
    # We have to be carefull as the distribution is assymetrix. Therefore we investigate the tail above 54 286
      
      ggplot(DataCleaned, aes(x = chargtot)) +
        geom_histogram(bins = 100, fill = "blue", color = "black", data = subset(DataCleaned, chargtot > Tmax)) +
        scale_x_continuous(breaks = seq(min(DataCleaned$chargtot), max(DataCleaned$chargtot), length.out = 15), labels = comma_format(accuracy = 1)) +
        labs(x = "Total Claim amount", y = "Frequency", title = "Histogram of tail values") +
        theme(plot.title = element_text(hjust = 0.5))
    
    # We visually identify ???142,112 as a valid outlier
      
      Tmax=142112
      
        
    # find outlier
      table(Data$chargtot > Tmax)["TRUE"] #12 values omitted
      
      Data_no_out <- DataCleaned[which(DataCleaned$chargtot < Tmax),]
      
      boxplot(Data_no_out$chargtot)
  
  ## Check average claim 
  
    # With Outliers    
      avgr_claim <- sum(DataCleaned$chargtot)/sum(DataCleaned$nbrtotc)
      avgr_claim #claim mean = 1662.054 per year 
    
      # Without Outliers    
      avgr_claim_no_out <- sum(Data_no_out$chargtot)/sum(Data_no_out$nbrtotc)
      avgr_claim_no_out #claim mean = 1347.709 per year  
      

#### 2.2.1 Age of policyholder ####
  
  severity_by_age <- Data_no_out %>%
    group_by(AGEPH) %>%
    summarise(Claim_sev = sum(chargtot) / sum(nbrtotc))
  
  print(severity_by_age,n=79)
  summary(severity_by_age)
  
  Sev.age <- ggplot(severity_by_age, aes(x = AGEPH, y = Claim_sev)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Age policyholder") +
    labs(x = "", y = "Claim Severity") +
    theme(plot.title = element_text(hjust = 0.5))


#### 2.2.2 Age of car #### 

  severity_by_agecar <- Data_no_out %>%
    group_by(agecar) %>%
    summarize(Claim_sev = sum(chargtot) / sum(nbrtotc))
  
  severity_by_agecar
  summary(severity_by_agecar)
  
  # Convert agecar to a factor with desired order
  severity_by_agecar$agecar <- factor(severity_by_agecar$agecar, levels = unique(severity_by_agecar$agecar))
  
  Sev.agecar <- ggplot(severity_by_agecar, aes(x = agecar, y = Claim_sev)) +
    theme_bw() +
    geom_bar(stat = "identity", alpha = 0.5) +
    ggtitle("Age car") +
    labs(x = "", y = "Claim Severity") +
    theme(plot.title = element_text(hjust = 0.5))


#### 2.2.3 Gender of policyholder #### 

  severity_by_sex <- Data_no_out %>%
    group_by(sexp) %>%
    summarize(Claim_sev = sum(chargtot) / sum(nbrtotc))
  
  severity_by_sex 
  
  Sev.sex <- ggplot(severity_by_sex, aes(x = sexp, y = Claim_sev)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Gender") +
    labs(x = "", y = "Claim Severity") +
    theme(plot.title = element_text(hjust = 0.5))

#### 2.2.4 Type of fuel #### 

  severity_by_fuel <- Data_no_out %>%
    group_by(fuelc) %>%
    summarize(Claim_sev = sum(chargtot) / sum(nbrtotc))
  
  severity_by_fuel 
  
  Sev.fuel <- ggplot(severity_by_fuel, aes(x = fuelc, y = Claim_sev)) + 
    theme_bw() +
    geom_bar(stat = "identity", alpha = .5) +
    ggtitle("Fuel type") +
    labs(x = "", y = "Claim Severity") +
    theme(plot.title = element_text(hjust = 0.5))


#### 2.2.5 Frequency premium ####

severity_by_split <- Data_no_out %>%
  group_by(split) %>%
  summarize(Claim_sev = sum(chargtot) / sum(nbrtotc))

severity_by_split

Sev.split <- ggplot(severity_by_split, aes(x = split, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Claim split") +
  labs(x = "", y = "Claim Severity") +
  theme(plot.title = element_text(hjust = 0.5))


#### 2.2.6 Use of Car ####

severity_by_use <- Data_no_out %>%
  group_by(usec) %>%
  summarize(Claim_sev = sum(chargtot) / sum(nbrtotc))

severity_by_use 

Sev.use <- ggplot(severity_by_use, aes(x = usec, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Use of the car") +
  labs(x = "", y = "Claim Severity") +
  theme(plot.title = element_text(hjust = 0.5))


#### 2.2.7 Fleet #### 

severity_by_fleet <- Data_no_out %>%
  group_by(fleetc) %>%
  summarize(Claim_sev = sum(chargtot) / sum(nbrtotc))

severity_by_fleet

Sev.fleet <- ggplot(severity_by_fleet, aes(x = fleetc, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Fleet type") +
  labs(x = "", y = "Claim Severity") +
  theme(plot.title = element_text(hjust = 0.5))

#### 2.2.8 Sportcar ####

severity_by_sport <- Data_no_out %>%
  group_by(sportc) %>%
  summarize(Claim_sev = sum(chargtot) / sum(nbrtotc))

severity_by_sport

Sev.sportc <- ggplot(severity_by_sport, aes(x = sportc, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Sport type") +
  labs(x = "", y = "Claim Severity") +
  theme(plot.title = element_text(hjust = 0.5))


#### 2.2.9 Coverage type ####

severity_by_cover <- Data_no_out %>%
  group_by(coverp) %>%
  summarize(Claim_sev = sum(chargtot) / sum(nbrtotc))

severity_by_cover 

Sev.cover <- ggplot(severity_by_cover, aes(x = coverp, y = avg_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Coverage") +
  labs(x = "", y = "Claim Severity") +
  theme(plot.title = element_text(hjust = 0.5))
  
#### 2.2.10 Horsepower of the car ####

severity_by_power <- Data_no_out %>%
  group_by(powerc) %>%
  summarize(Claim_sev = sum(chargtot) / sum(nbrtotc))

severity_by_power

Sev.power <- ggplot(severity_by_power, aes(x = powerc, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Horsepower") +
  labs(x = "", y = "Claim Severity") +
  theme(plot.title = element_text(hjust = 0.5))

#### 2.2.11 Location of policyholder ####

#[RD to do]

#### Plot Severity graphs  ####

grid.arrange(Sev.age, nrow = 1)
grid.arrange(Sev.agecar, Sev.sex, Sev.fuel, Sev.split, Sev.use, Sev.fleet, Sev.sportc, Sev.cover, Sev.power, nrow = 3)
#grid.arrange(Freq.location_muni, nrow = 1) #The be added when sev.location is made


############ Section 2.3 Check for relationship between two variables ############ 

#### 2.3.1 Between age policy holder and age car ####

  age_policyholder <- Data$AGEPH
  age_car <- Data$agecar
  df <- data.frame(age_policyholder, age_car)
  
  # create a function to convert age_car to a numeric variable
  age_car_numeric <- function(age_car) {
    ifelse(age_car == "0-1", 0.5,
           ifelse(age_car == "2-5", 3.5,
                  ifelse(age_car == "6-10", 8,
                         ifelse(age_car == ">10", 11.5, NA))))
  }
  
  df$age_car_numeric <- age_car_numeric(df$age_car)
  
  # calculate the correlation 
  cor(df$age_policyholder, df$age_car_numeric) 

  
  cor_result <- cor.test(df$age_policyholder, df$age_car_numeric, method = "pearson", conf.level = 0.95)
  
  # print the correlation coefficient and its confidence interval
  cat("Correlation coefficient:", round(cor_result$estimate, 4), "\n") #0.03325993 there is a  very (weak) positive correlation btwn age of policyholder and age of car)
  cat("95% confidence interval:", round(cor_result$conf.int, 4), "\n")
  
  
#### 2.3.2 Between Sport Car and Horse Power ####

  df <- data.frame(Data$sportc, Data$powerc)
  
  # convert the string variables to numeric values
  df$sport_car_numeric <- ifelse(df$Data.sportc == 'Yes', 1, 0)
  df$power_car_numeric <- ifelse(df$Data.powerc == '<66', 0, 
                                 ifelse(df$Data.powerc == '66-110', 1, 2))
  summary(df)
  
  # calculate the correlation 
  cor(df$sport_car_numeric, df$power_car_numeric) 
  cor_result <- cor.test(df$sport_car_numeric, df$power_car_numeric, method = "pearson", conf.level = 0.95)
  
  # print the correlation coefficient and its confidence interval
  cat("Correlation coefficient:", round(cor_result$estimate, 4), "\n") #0.2028925 there is a positive correlation btwn age of policyholder and age of car)
  cat("95% confidence interval:", round(cor_result$conf.int, 4), "\n")
  
#### 2.3.3 Between Sport Car and Fuel type ####
  
  df <- data.frame(Data$fuelc, Data$powerc)
  
  # convert the string variables to numeric values
  df$fuel_type_numeric <- ifelse(df$Data.fuelc == 'Gasoil', 1, 0)
  df$power_car_numeric <- ifelse(df$Data.powerc == '<66', 0, 
                                 ifelse(df$Data.powerc == '66-110', 1, 2))
  summary(df)
  
  # calculate the correlation 
  cor(df$fuel_type_numeric, df$power_car_numeric) 
  cor_result <- cor.test(df$fuel_type_numeric, df$power_car_numeric, method = "pearson", conf.level = 0.95)
  
  # print the correlation coefficient and its confidence interval
  cat("Correlation coefficient:", round(cor_result$estimate, 4), "\n") #-0.0904  there is almost no correlation btwn power and fuel type)
  cat("95% confidence interval:", round(cor_result$conf.int, 4), "\n")
  
