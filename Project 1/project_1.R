
library(tidyverse)
library(caret)
library(writexl)

data <- read.csv("data/Customer_Dataset_Data.csv", na.strings = "?", stringsAsFactors = F)
data <- data %>% column_to_rownames(., var = "CustomerID")

# Drop irrelevant columns
data <- data[, -c(7,18,19,20,21,23,24,25,26,27,28,29,30,31,32)]


# Convert monetary columns from character to numeric

data$VoiceOverTenure <- parse_number(data$VoiceOverTenure)
data$VoiceLastMonth <- parse_number(data$VoiceLastMonth)

data$EquipmentOverTenure <- parse_number(data$EquipmentOverTenure)
data$EquipmentLastMonth <- parse_number(data$EquipmentLastMonth)

data$DataOverTenure <- parse_number(data$DataOverTenure)
data$DataLastMonth <- parse_number(data$DataLastMonth)

data$CardSpendMonth <- parse_number(data$CardSpendMonth)


# Convert null values in monetary columns to 0

data$VoiceOverTenure[is.na(data$VoiceOverTenure)] <- 0
data$VoiceLastMonth[is.na(data$VoiceLastMonth)] <- 0

data$EquipmentOverTenure[is.na(data$EquipmentOverTenure)] <- 0
data$EquipmentLastMonth[is.na(data$EquipmentLastMonth)] <- 0

data$DataOverTenure[is.na(data$DataOverTenure)] <- 0
data$DataLastMonth[is.na(data$DataLastMonth)] <- 0

data$CardSpendMonth[is.na(data$CardSpendMonth)] <- 0

# Derived Feature

data$HHIncome <- parse_number(data$HHIncome)
data$TotalOverTenure <- data$VoiceOverTenure + data$EquipmentOverTenure + data$DataOverTenure
data$TotalByTenure <- data$TotalOverTenure / data$PhoneCoTenure
data$TotalLastMonth <- data$VoiceLastMonth + data$EquipmentLastMonth + data$DataLastMonth


data_occupation <- data %>%
  group_by(JobCategory) %>%
  summarise(avgLastMonth = mean(TotalLastMonth))

a <- ggplot(data = data_occupation) +
  geom_col(aes(x=JobCategory, y=avgLastMonth))
print(a)

b <- ggplot(data = data) +
  geom_histogram(aes(x=Age), bins=30, color = "black", fill = "blue")
print(b)

c <- ggplot(data = data) +
  geom_point(aes(x=PhoneCoTenure, y=TotalLastMonth), color = "lightblue")
print(c)


data_equip <- data %>%
  add_count(EquipmentRental) %>%
  group_by(EquipmentRental,n) %>%
  summarise(avgLastMonth = mean(TotalLastMonth))

d <- ggplot(data = data %>% filter(EquipmentRental == "Yes")) +
  geom_histogram(aes(x=Age), color="black", fill="yellow", bins=30)
print(d)


data_wireless <- data %>%
  add_count(WirelessData) %>%
  group_by(WirelessData,n) %>%
  summarise(avgLastMonth = mean(TotalLastMonth))

data <- data %>% mutate(segment = case_when(
  EquipmentRental == "Yes" & WirelessData == "Yes" ~ 1,
  EquipmentRental == "Yes" & WirelessData == "No" ~ 2,
  EquipmentRental == "No" & WirelessData == "Yes" ~ 3,
  EquipmentRental == "No" & WirelessData == "No" ~ 4,
))

data_segmented <- data %>% add_count(segment) %>%
  group_by(segment,n) %>%
  select_if(is.numeric) %>%
  summarise_all("median")

e <- ggplot(data = data) +
  geom_histogram(aes(x=PhoneCoTenure), color="black", fill="darkred") +
  geom_vline(xintercept = mean(data$PhoneCoTenure), color="blue")
print(e)