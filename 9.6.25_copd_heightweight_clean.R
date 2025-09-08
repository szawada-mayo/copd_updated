library(dplyr)
library(lubridate)
library(circular)
data <- read.csv("df_height.csv") 

data <- data[!(is.na(data$Result) | data$Result == ""), ] ##glucose
data <- data[grepl("[0-9]", data$Result), ]
data$Result <- as.numeric(data$Result)
data <- data[data$Result >= 45, ]
data <- data[data$Result <= 200, ]
data$Result_in <- ifelse(data$Result > 100, data$Result * 0.393701, data$Result)
data$Result_m <- ifelse(data$Result_in > 1, data$Result_in * 0.0254, data$Result_in)
data$Result_m2 <- ifelse(data$Result_m > 1, data$Result_m * data$Result_m, data$Result_m)

data <- data[!(is.na(data$Weight) | data$Weight == ""), ] ##glucose
data <- data[grepl("[0-9]", data$Weight), ]
data$Weight <- as.numeric(data$Weight)

data$Weight_kg <- ifelse(data$Weight > 1, data$Weight * 0.453592, data$Weight)

data$bmi <- (data$Weight_kg)/(data$Result_m2)

write.csv(data, "df_bmi.csv", row.names = FALSE)
