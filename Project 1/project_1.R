
library(tidyverse)
library(caret)
library(writexl)

original_data <- read.csv("data/Customer_Dataset_Data.csv", na.strings = "?", stringsAsFactors = F)
original_data <- original_data %>% column_to_rownames(., var = "CustomerID")

data <- original_data

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


# Handle null values in monetary columns

data$VoiceOverTenure <- replace_na(data$VoiceOverTenure, median(data$VoiceOverTenure))
data$VoiceLastMonth <- replace_na(data$VoiceLastMonth, median(data$VoiceLastMonth))


data$VoiceOverTenure[is.na(data$VoiceOverTenure)] <- median(data$VoiceOverTenure)
data$VoiceLastMonth[is.na(data$VoiceLastMonth)] <- median(data$VoiceLastMonth)

handle_na <- function(data, col1, col2) {
  med_val <- median(data[[col1]], na.rm = TRUE)  # Calculate median of Column 1
  for (i in 1:nrow(data)) {
    if (is.na(data[i, col1])) {
      if (data[i, col2] == "Yes") {
        data[i, col1] <- med_val
      } else {
        data[i, col1] <- 0
      }
    }
  }
  return(data)
}

data <- handle_na(data, "EquipmentOverTenure", "EquipmentRental")
data <- handle_na(data, "EquipmentLastMonth", "EquipmentRental")

data <- handle_na(data, "DataOverTenure", "WirelessData")
data <- handle_na(data, "DataLastMonth", "WirelessData")

data$CardSpendMonth[is.na(data$CardSpendMonth)] <- median(data$CardSpendMonth)

# Derived Feature

data$HHIncome <- parse_number(data$HHIncome)
data$TotalOverTenure <- data$VoiceOverTenure + data$EquipmentOverTenure + data$DataOverTenure
data$TotalByTenure <- data$TotalOverTenure / data$PhoneCoTenure
data$TotalLastMonth <- data$VoiceLastMonth + data$EquipmentLastMonth + data$DataLastMonth

data$LogIncome <- log10(data$HHIncome)
data$LogTotalLastMonth <- log10(data$TotalLastMonth)


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

f <- ggplot(data = data) +
  geom_point(aes(x=LogIncome, y=LogTotalLastMonth), color="blue")
print(f)

cor(data$LogIncome, data$LogTotalLastMonth)

g <- ggplot(data = data %>% select(Age, TVWatchingHours) %>%
              group_by(Age) %>%
              summarise(AvgTVHours = mean(TVWatchingHours))) +
  geom_col(aes(x=Age, y=AvgTVHours), fill="forestgreen")
print(g)