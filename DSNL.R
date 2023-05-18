##########################################
## Assignment Data science for None-Life##
## Jean-Ferdinand van Cauwenbergh       ##
## Jos Van den Eynde                    ## 
## Robin Dessein                        ##
##########################################

############# 0. Import Data #############


## Dataframes
# Data -> Data provided on Toledo
# DataCleaned -> Data where policyholders that reported zero claims are omitted 
# Data_no_out -> DataCleaned + removal outliers
# Tmax -> Claim amounts above this value is considered as an outlier

### 0.1 Data of "Assignment.csv" and "inspost.xls" merged using VLOOKUP

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

lapply(Data,class)

############# Section 2. Descriptive analysis #############

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
    geom_bar(stat = "identity", alpha = 0.5) +
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
    ggtitle("Claim split") +
    labs(x = "", y = "Claim Frequency") +
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

frequency_by_power$powerc <- factor(frequency_by_power$powerc,
                                    levels = c("<66", "66-110", ">110"))

Freq.power <- ggplot(frequency_by_power, aes(x = powerc, y = avg_freq)) +
  theme_bw() +
  geom_bar(stat = "identity", alpha = 0.5) +
  ggtitle("Horsepower") +
  labs(x = "", y = "Claim Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

#### 2.1.11 Location of policyholder  ####
  # municipal level 

  averaged_frequency <- DataFreq %>%
    group_by(LONG, LAT) %>%
    summarize(avg_frequency = mean(frequency))
  
  Freq.location <- ggplot(averaged_frequency, aes(x = LONG, y = LAT, size = avg_frequency)) + 
    geom_point(alpha = 0.5) +
    labs(title = "Average claim frequency by location (municipal)") +
    scale_size(range = c(1, 10))
  
    frequency_by_Commune <- DataFreq %>%
      group_by(COMMUNE) %>%
      summarize(avg_freq = sum(nbrtotc)/sum(duree))

  # province level

    # limit the link 
    DataFreq_INS1 <- DataFreq %>%
      mutate(INS = ifelse(substr(INS, 1, 1) == "2", 
                      ifelse(substr(INS, 1, 2) == "21", "21",
                             ifelse(substr(INS, 1, 2) %in% c("23", "24"), "23",
                                    ifelse(substr(INS, 1, 2) == "25", "25", "2"))),
                      substr(INS, 1, 1)))

    INS_freq <- DataFreq_INS1 %>%
      group_by(INS) %>%
      summarise(avg_freq = sum(nbrtotc) / sum(duree))

    # Define the mapping dataset for Belgium provinces
    province_mapping <- data.frame(INS = c("1","21","23", "25", "3", "4", "5", "6", "7", "8", "9"),
                                 Province = c("Antwerp", "Brussels Capital Region", "Flemish Brabant", "Walloon Brabant", "West Flanders",
                                              "East Flanders", "Hainaut", "Li�ge",
                                              "Limburg", "Luxembourg", "Namur"))
  
    # Merge the mapping dataset with INS_freq based on the INS column
    INS_freq_with_province <- merge(INS_freq, province_mapping, by = "INS")
    
    INS_freq_with_province_sorted <- INS_freq_with_province %>%
      arrange(desc(avg_freq))
    
    print(INS_freq_with_province_sorted)

#### Plot Frequency graphs  ####

grid.arrange(Freq.age, nrow = 1)
grid.arrange(Freq.agecar, Freq.sex, Freq.fuel, Freq.split, Freq.use, Freq.fleet, Freq.sportc, Freq.cover, Freq.power, nrow = 3)
grid.arrange(Freq.location, nrow = 1)


############ Section 2.2 Severity Data #############


#### 2.2.0 Remove outliers  ####

  ## remove observations without no claims
  
    DataCleaned <- DataSev[which(DataSev$chargtot  > 0),]

  ## plot geometric density graphs

    plot.eda1 <- ggplot(data = DataCleaned, aes(chargtot)) +
      geom_density(adjust = 3, fill = "lightgrey", alpha = 0.5) +
      ylab("Relative Frequency") +
      xlab("Severity") +
      theme_bw() +
      scale_y_continuous(labels = scales::comma)
    
    plot.eda1
    
    
    plot.eda2 <- ggplot(data = DataCleaned, aes(chargtot)) +
      geom_density(adjust = 3, fill = "lightgrey", alpha = 0.5) +
      xlim(0, 150000) +
      ylab("Relative Frequency") +
      xlab("Severity") +
      theme_bw() +
      scale_y_continuous(labels = scales::comma)
    
    plot.eda2
    
    
    plot.eda3 <- ggplot(data = DataCleaned, aes(chargtot)) +
      geom_density(adjust = 3, fill = "lightgrey", alpha = 0.5) +
      xlim(0, 10000) +
      ylab("Relative Frequency") +
      xlab("Severity") +
      theme_bw() +
      scale_y_continuous(labels = scales::comma)
    
    plot.eda3

  ## Check for outliers 
    boxplot(DataCleaned$chargtot, main = "Total claim amount") #many outliers, one extreme outlier 1989567.9 

    plot(DataCleaned$chargtot, DataSev$nbrtotc, xlab = "Severity", ylab = "Frequency", main = "Frequency vs. Severity", pch = 16)
    

    # get mean and Standard deviation
      mean = mean(DataCleaned$chargtot)
      std = sd(DataCleaned$chargtot)
    
    # get threshold values for outlines
      Tmax = mean+(3*std) 
      Tmax #outliers are all values above 54 286

      table(Data$chargtot > Tmax)["TRUE"] #35 values above Tmax2
      
    # We have to be carefull as the distribution is assymetrix. Therefore we investigate the tail above 54 286
      
      ggplot(DataCleaned, aes(x = chargtot)) +
        geom_histogram(bins = 100, data = subset(DataCleaned, chargtot > Tmax)) +
        scale_x_continuous(breaks = seq(min(DataCleaned$chargtot), max(DataCleaned$chargtot), length.out = 15), labels = comma_format(accuracy = 1)) +
        labs(x = "Total claim amount", y = "Frequency", title = "Histogram of tail values") +
        theme(plot.title = element_text(hjust = 0.5))
    
    # We visually identify 142,112 as a valid outlier
      
      Tmax2=142112
      
        
    # find outlier
      table(Data$chargtot > Tmax2)["TRUE"] #12 values above Tmax2
      
      Data_no_out <- DataCleaned[which(DataCleaned$chargtot < Tmax2),]
      

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

Sev.cover <- ggplot(severity_by_cover, aes(x = coverp, y = Claim_sev)) + 
  theme_bw() +
  geom_bar(stat = "identity", alpha = .5) +
  ggtitle("Coverage") +
  labs(x = "", y = "Claim Severity") +
  theme(plot.title = element_text(hjust = 0.5))
  

#### 2.2.10 Horsepower of the car ####

severity_by_power <- Data_no_out %>%
  group_by(powerc) %>%
  summarize(Claim_sev = sum(chargtot) / sum(nbrtotc))

severity_by_power$powerc <- factor(severity_by_power$powerc,
                                   levels = c("<66", "66-110", ">110"))

Sev.power <- ggplot(severity_by_power, aes(x = powerc, y = Claim_sev)) +
  theme_bw() +
  geom_bar(stat = "identity", alpha = 0.5) +
  ggtitle("Horsepower") +
  labs(x = "", y = "Claim Severity") +
  theme(plot.title = element_text(hjust = 0.5))


#### 2.2.11 Location of policyholder ####

  # municipal level 
  
    averaged_severity <- Data_no_out %>%
      group_by(LONG, LAT) %>%
      summarise(avg_severity = mean(severity))
        
    Sev.location <- ggplot(averaged_severity, aes(x = LONG, y = LAT, size = avg_severity)) + 
      geom_point(alpha = 0.5) +
      labs(title = "Average claim severity by location (municipal)") +
      scale_size(range = c(1, 10),
                 labels = function(x) sprintf("%.1f", x))
    
    print(Sev.location)

  
  # province level
  
    # limit the link 
    DataSev_INS1 <- Data_no_out %>%
      mutate(INS = ifelse(substr(INS, 1, 1) == "2", 
                          ifelse(substr(INS, 1, 2) == "21", "21",
                                 ifelse(substr(INS, 1, 2) %in% c("23", "24"), "23",
                                        ifelse(substr(INS, 1, 2) == "25", "25", "2"))),
                          substr(INS, 1, 1)))
    
    INS_Sev <- DataSev_INS1 %>%
      group_by(INS) %>%
      summarise(Claim_sev = sum(chargtot) / sum(nbrtotc))
    
    # Define the mapping dataset for Belgium provinces
    province_mapping <- data.frame(INS = c("1","21","23", "25", "3", "4", "5", "6", "7", "8", "9"),
                                   Province = c("Antwerp", "Brussels Capital Region", "Flemish Brabant", "Walloon Brabant", "West Flanders",
                                                "East fFlanders", "Hainaut", "Li�ge",
                                                "Limburg", "Luxembourg", "Namur"))
    
    # Merge the mapping dataset with INS_freq based on the INS column
    INS_sev_with_province <- merge(INS_Sev, province_mapping, by = "INS")
    
    INS_sev_with_province_sorted <- INS_sev_with_province %>%
      arrange(desc(Claim_sev))
    
    print(INS_sev_with_province_sorted)


#### Plot Severity graphs  ####

grid.arrange(Sev.age, nrow = 1)
grid.arrange(Sev.agecar, Sev.sex, Sev.fuel, Sev.split, Sev.use, Sev.fleet, Sev.sportc, Sev.cover, Sev.power, nrow = 3)
grid.arrange(Sev.location, nrow = 1) 


############ Section 2.3 Correlation matrix ############ 


  
df <- data.frame(Data$AGEPH, Data$agec, Data$sexp, Data$fuelc, Data$split, Data$usec, Data$fleet, Data$sportc, Data$powerc, Data$coverp)

# Rename the columns
colnames(df) <- c("age", "age_car", "gender", "fuel", "split", "use_car", "fleet", "sportcar", "power_car", "coverage")

# Convert the string variables to numeric values
df$age <- df$age

age_car_numeric <- function(age_car) {
  ifelse(age_car == "0-1", 0.5,
         ifelse(age_car == "2-5", 3.5,
                ifelse(age_car == "6-10", 8,
                       ifelse(age_car == ">10", 11.5, NA))))
}

df$age_car <- age_car_numeric(df$age_car)
df$gender <- ifelse(df$gender == 'Male', 1, 0)
df$fuel <- ifelse(df$fuel == 'Gasoil', 1, 0)
df$split <- ifelse(df$split == '<Once', 0,
                   ifelse(df$split == 'Twice', 1,
                          ifelse(df$split == 'Thrice', 2, 3)))

df$use_car <- ifelse(df$use_car == 'Private', 1, 0)
df$fleet <- ifelse(df$fleet == 'Yes', 1, 0)
df$sportcar <- ifelse(df$sportcar == 'Yes', 1, 0)
df$coverage <- ifelse(df$coverage == '<MTPL', 0,
                      ifelse(df$coverage == 'MTPL+', 1, 2))
df$power_car <- ifelse(df$power_car == '<66', 0,
                       ifelse(df$power_car == '66-110', 1, 2))

correlation_table <- round(cor(df[, c("age", "age_car", "gender", "use_car",
                                      "split", "fleet", "sportcar",
                                      "power_car", "coverage")]), 2)

print(correlation_table)



  


  