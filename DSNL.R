##########################################
## Assignment Data science for None-Life ##
##########################################


# 0. Import data

setwd("C:/Users/rdessein/oneDrive - Deloitte (O365D)/Documents/GitHub/DataScienceForNonLife-Assignment")

### 0.1 Data of "Assinment.csv" and "inspost.xls" merged using VLOOKUP

### 0.2 import dataframe

Data <- as.data.table(read.csv("DataCombined.csv", header=TRUE, sep=";", dec="."))

# 1. frequency

frequency <- Data$nbrtotc / Data$duree #number of claims during (period of) exposure di
DataFreq <- cbind(Data, frequency)
DataFreq

summary(DataFreq$nbrtotc)
summary(Data$emp_freq)
avg_freq <- sum(DataFreq$nbrtotc)/sum(DataFreq$duree)