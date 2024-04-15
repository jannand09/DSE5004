
library(tidyverse)

data <- read.csv("data/Customer_Dataset_Data.csv", na.strings = "?", stringsAsFactors = F)

data <- data %>% column_to_rownames(., var = "CustomerID")


