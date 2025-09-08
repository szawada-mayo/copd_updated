#install.packages("sandwich")
library(sandwich)
library(dplyr)
library(lubridate)
data <- read.csv("df_baselinecharacteristics.csv")

median(data$age_2_a, na.rm = FALSE)
quantile(data$age_2_a, probs = c(0.25, 0.75), na.rm = TRUE)
#data$sex_numeric <- ifelse(data$Gender == "Male", 1, 0)
mean(data$sex_numeric_a == 1, na.rm = TRUE) * 100
table(data$race)
table(data$ethnicity)
median(data$bmi_a, na.rm = FALSE)
quantile(data$bmi_a, probs = c(0.25, 0.75), na.rm = TRUE)
mean(data$bmi_level == 2, na.rm = TRUE) * 100
mean(data$bmi_level == 3, na.rm = TRUE) * 100
table(data$mmrc_a)
data$charlson_a_level <- 0
charlsonlevels <- ifelse(data$charlson_a < 3, 1,
                                   ifelse(data$charlson_a <5, 2, 3))
table(charlsonlevels)
fev1_levels <- ifelse(data$fev1_updated_a < 50, 1,
                         ifelse(data$fev1_updated_a <70, 2, 3))
table(fev1_levels)
table(data$education_a)
table(data$marital_status_a)
table(data$previous_smoker_a)
table(data$current_smoker_a)

median(data$sd_time, na.rm = FALSE)
quantile(data$sd_time, probs = c(0.25, 0.75), na.rm = TRUE)

data_waso <- read.csv("df_wasobaseline.csv")

median(data_waso$sd_waso, na.rm = FALSE)
quantile(data_waso$sd_waso, probs = c(0.25, 0.75), na.rm = TRUE)

###phenotypes

data$sd_waso <- data_waso$sd_waso
data$sleep_waso_level <- ifelse(data$sd_waso < 25, 1,
                                    ifelse(data$sd_waso <45, 2, 3))

data <- data %>%
  mutate(sd_level = if_else(sleep_dur_level %in% c(2, 3), 1, 0))


data <- data %>%
  mutate(summary_c = case_when(
    sleep_waso_level == 1 & sd_level == 0 ~ 0,
    sleep_waso_level == 2 & sd_level == 0 ~ 1,
    sleep_waso_level == 1 & sd_level == 1 ~ 2,
    sleep_waso_level == 2 & sd_level == 1 ~ 3
  ),
  summary_c = as.factor(summary_c))

table(data$summary_c)
