#install.packages("sandwich")
library(sandwich)
library(dplyr)
library(lubridate)
data <- read.csv("df_bmi.csv")
data_date_first <- read.csv("BatchSleepExportDetails(2025-08-06_06-46-28).csv") 

data <- data[ , -c(15:29)]

data_date_first$Subject.Name <- tolower(data_date_first$Subject.Name)

data_date_end <- data_date_first[!grepl("end", data_date_first$Subject.Name), ]
data_date_mid <- data_date_end[!grepl("mid", data_date_end$Subject.Name), ]
data_date_3m <- data_date_mid[!grepl("3m", data_date_mid$Subject.Name), ]
data_date_6m <- data_date_3m[!grepl("6m", data_date_3m$Subject.Name), ]
data_date_9m <- data_date_6m[!grepl("9m", data_date_6m$Subject.Name), ]
data_date_9m <- data_date_9m[!grepl("9 m", data_date_9m$Subject.Name), ]
data_date_9m <- data_date_9m[!grepl("final", data_date_9m$Subject.Name), ]
data_date_9m <- data_date_9m[!grepl("endline", data_date_9m$Subject.Name), ]

data_date_9m$Subject.Name <- sub(" base| baseline| base \\(2\\)| base second", "", data_date_9m$Subject.Name)
data_date_9m$Subject.Name <- sub(" \\(2\\)", "", data_date_9m$Subject.Name)

data_date_9m$Subject.Name <- as.character(data_date_9m$Subject.Name)
data$Subject.Name <- as.character(data$Subject.Name)

df <- left_join(data_date_9m, data, by = "Subject.Name")
#write.csv(result, "checkresult.csv", row.names = FALSE)

df <- df[!is.na(df$bmi), ] ##bmi
df <- df[!is.na(df$age2), ]
df <- df[!is.na(df$In.Bed.Time.Clean), ]
df <- df[!is.na(df$charlson), ]
df <- df[!is.na(df$mmrc), ]
df <- df[!is.na(df$fev1_updated), ]

###clean each biomarker only data not characters
dfcheck <- df %>%
  filter(!is.na(suppressWarnings(as.numeric(bmi))))

#drop any naps
df <- df %>%
  mutate(
    dt1 = mdy_hm(In.Bed.Time),
    dt2 = mdy_hm(Out.Bed.Time),
    check_time = if_else(as.Date(dt1) == as.Date(dt2), "yes", "no")
  )

#review naps
df %>% select(In.Bed.Time, Out.Bed.Time, check_time)

df <- df %>%
  mutate(
    dt1 = mdy_hm(In.Bed.Time),
    dt2 = mdy_hm(Out.Bed.Time),
    #check_time = if_else(as.Date(dt1) == as.Date(dt2), "yes", "no")
    # Check if time difference is within 1 hour
    dt1_next = lead(dt1),   # next row's column1
    same_date = as.Date(dt1_next) == as.Date(dt2),
    
    continuous_time = if_else(
      same_date & abs(as.numeric(difftime(dt1_next, dt2, units = "hours"))) <= 1,
      "yes", "no"
    )
  )

# View result
df %>% select(Out.Bed.Time, In.Bed.Time, same_date, continuous_time)

df_filtered <- df %>%
  filter(!(check_time == "yes" & continuous_time == "no"))

# Add `night_time` column
df_filtered <- df_filtered %>%
  mutate(
    dt1 = mdy_hm(In.Bed.Time),
    hour = hour(dt1),
    night_time = if_else(hour >= 20 | hour < 5, "yes", "no")
  )

# View result
df_filtered %>% select(In.Bed.Time, night_time)

df_night <- df_filtered %>%
  filter(night_time == "yes")
#filter those with fewer than 3 nights of data from the 7 day baseline
df_night <- df_night %>%
  group_by(mcn) %>%
  filter(n() >= 3) %>%
  ungroup()

# Save DataFrame to a CSV file
write.csv(df_night, "sleepoutput_clean_bmionly.csv", row.names = FALSE)

length(unique(df_night$mcn))

sd_table <- df_night %>%
  group_by(mcn) %>%
  summarise(
    mean_time = mean(Total.Sleep.Time, na.rm = TRUE),
    sd_time = sd(Total.Sleep.Time, na.rm = TRUE)
    ) %>%
  ungroup()

sd_table$sleep_dur_level <- ifelse(sd_table$sd_time < 60, 1,
                                   ifelse(sd_table$sd_time <120, 2, 3))

sum(sd_table$sleep_dur_level == 1, na.rm = TRUE)
sum(sd_table$sleep_dur_level == 2, na.rm = TRUE)
sum(sd_table$sleep_dur_level == 3, na.rm = TRUE)

hist(sd_table$sd_time, 
     main = "Distribution of X", 
     xlab = "X values", 
     col = "skyblue", 
     border = "white")

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, bmi)

# Step 2: Left join to bring SYS into df2 as SYS_COPY
sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(bmi_a = bmi)

sd_table$bmi_level <- ifelse(sd_table$bmi_a < 25, 1,
                             ifelse(sd_table$bmi_a <30, 2, 3))

sum(sd_table$bmi_level == 1, na.rm = TRUE)
sum(sd_table$bmi_level == 2, na.rm = TRUE)
sum(sd_table$bmi_level == 3, na.rm = TRUE)

df_healthyweight <- sd_table %>%
  filter(bmi_level == 1)

df_healthyweight <- df_healthyweight %>%
  mutate(sleep_dur_level = factor(sleep_dur_level, levels = c(1, 2, 3)))  # Drop 4 if not relevant

#############

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, age2)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(age_2_a = age2)

data$sex_numeric <- ifelse(data$Gender == "Male", 1, 0)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, sex_numeric)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(sex_numeric_a = sex_numeric)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, mmrc)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(mmrc_a = mmrc)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, charlson)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(charlson_a = charlson)


data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, fev1_updated)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(fev1_updated_a = fev1_updated)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, race)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(race_a = race)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, ethnicity)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(ethnicity_a = ethnicity)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, education)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(education_a = education)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, marital_status)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(marital_status_a = marital_status)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, previous_smoker)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(previous_smoker_a = previous_smoker)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, current_smoker)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(current_smoker_a = current_smoker)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, Subject.Name)

sd_table <- sd_table %>%
  left_join(data_first, by = "mcn") %>%
  rename(Subject.Name_a = Subject.Name)


######end covariates

write.csv(sd_table, "sdcheck.csv", row.names = FALSE)
######end covariates


####ppick up
quantile(sd_table$age_2_a, probs = c(0.25, 0.75), na.rm = TRUE)

Q1 <- quantile(sd_table$age_2_a, 0.25, na.rm = TRUE)
Q3 <- quantile(sd_table$age_2_a, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

# Define outlier thresholds
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

outliers <- sd_table$age_2_a[sd_table$age_2_a < lower_bound | sd_table$age_2_a > upper_bound]
print(outliers)

#sd_table <- sd_table[sd_table$age_2_a >= lower_bound & sd_table$age_2_a <= upper_bound, ]
sd_table <- sd_table[sd_table$bmi_a <= 50, ]
#sd_table <- sd_table[sd_table$bmi_a >= 18.5, ]
#sd_table <- sd_table[sd_table$age_2_a >= 65, ]
#sd_table <- sd_table[sd_table$charlson_a <= 15, ]

write.csv(sd_table, "df_baselinecharacteristics.csv", row.names = FALSE)

#######

sd_table <- sd_table %>%
  mutate(
    sleep_dur_level = factor(sleep_dur_level,
                             levels = c(1, 2, 3),
                             labels = c("<60 min", "60–120 min", ">120 min")
    )
  )

#sd_table_overweight <- sd_table %>%
#  mutate(high_bmi = if_else(bmi_level %in% c(2, 3), 1, 0))

#sd_table_overweight <- sd_table %>%
#  mutate(high_bmi = if_else(bmi_level == 2, 1, 0))

sd_table$sleep_dur_level_3 <- as.numeric(sd_table$sleep_dur_level == ">120 min")
sd_table$bmi_level <- factor(sd_table$bmi_level, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_dur_level_3 ~ bmi_level +age_2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = sd_table)

# Robust SEs
robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)

est <- coef(model)
se <- sqrt(diag(robust_se))

PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)

# Combine into readable table
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)

print(PR_table)

########
#sd_table_overweight_2 <- sd_table %>%
#  mutate(high_bmi = if_else(bmi_level == 2, 1, 0))

sd_table$sleep_dur_level_2 <- as.numeric(sd_table$sleep_dur_level == levels(sd_table$sleep_dur_level)[2])
sd_table$bmi_level <- factor(sd_table$bmi_level, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_dur_level_2 ~ bmi_level +age_2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = sd_table)

# Robust SEs

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)

est <- coef(model)
se <- sqrt(diag(robust_se))

PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)

# Combine into readable table
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)

print(PR_table)

####less than 60 sleep

sd_table$sleep_dur_level_1 <- as.numeric(sd_table$sleep_dur_level == "<60 min")
sd_table$bmi_level <- factor(sd_table$bmi_level, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_dur_level_1 ~ bmi_level +age_2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = sd_table)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)

est <- coef(model)
se <- sqrt(diag(robust_se))

PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)

# Combine into readable table
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)

print(PR_table)

#####################

#sd_table_overweight_4 <- sd_table %>%
#  mutate(high_bmi = if_else(bmi_level %in% c(2, 3), 1, 0))

sd_table$bmi_level_2 <- as.numeric(as.character(sd_table$bmi_level))
sd_table$bmi_level_2 <- ifelse(sd_table$bmi_level_2 < 2, 1, 2)
sd_table$bmi_level_2 <- factor(sd_table$bmi_level_2, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_dur_level_3 ~ bmi_level_2 +age_2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = sd_table)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

########

sd_table$bmi_level_2 <- as.numeric(as.character(sd_table$bmi_level))
sd_table$bmi_level_2 <- ifelse(sd_table$bmi_level_2 < 2, 1, 2)
sd_table$bmi_level_2 <- factor(sd_table$bmi_level_2, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_dur_level_2 ~ bmi_level_2 +age_2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = sd_table)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

#####

sd_table$bmi_level_2 <- as.numeric(as.character(sd_table$bmi_level))
sd_table$bmi_level_2 <- ifelse(sd_table$bmi_level_2 < 2, 1, 2)
sd_table$bmi_level_2 <- factor(sd_table$bmi_level_2, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_dur_level_1 ~ bmi_level_2 +age_2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = sd_table)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

################WASO

##this works up to here
freq_of_sleep_levels <- table(sd_table$sleep_dur_level)
print(freq_of_sleep_levels)

#####waso
sd_table <- df_night %>%
  group_by(mcn) %>%
  summarise(
    mean_waso = mean(WASO, na.rm = TRUE),
    sd_waso = sd(WASO, na.rm = TRUE)
    ) %>%
  ungroup()


sd_table$sleep_waso_level <- ifelse(sd_table$sd_waso < 25, 1,
                                    ifelse(sd_table$sd_waso <45, 2, 3))

sum(sd_table$sleep_waso_level == 1, na.rm = TRUE)
sum(sd_table$sleep_waso_level == 2, na.rm = TRUE)
sum(sd_table$sleep_waso_level == 3, na.rm = TRUE)


#####
result <- sd_table

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, bmi)

# Step 2: Left join to bring SYS into df2 as SYS_COPY
result <- result %>%
  left_join(data_first, by = "mcn") %>%
  rename(bmi_a = bmi)

result$bmi_level <- ifelse(result$bmi_a < 25, 1,
                           ifelse(result$bmi_a <30, 2, 3))

#############

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, age2)

result <- result %>%
  left_join(data_first, by = "mcn") %>%
  rename(age2_a = age2)

data$sex_numeric <- ifelse(data$Gender == "Male", 1, 0)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, sex_numeric)

result <- result %>%
  left_join(data_first, by = "mcn") %>%
  rename(sex_numeric_a = sex_numeric)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, mmrc)

result <- result %>%
  left_join(data_first, by = "mcn") %>%
  rename(mmrc_a = mmrc)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, charlson)

result <- result %>%
  left_join(data_first, by = "mcn") %>%
  rename(charlson_a = charlson)

data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, fev1_updated)

result <- result %>%
  left_join(data_first, by = "mcn") %>%
  rename(fev1_updated_a = fev1_updated)


data_first <- data %>%
  group_by(mcn) %>%
  slice(1) %>%  # keep only the first matching row
  ungroup() %>%
  select(mcn, Subject.Name)

result <- result %>%
  left_join(data_first, by = "mcn") %>%
  rename(Subject.Name_a = Subject.Name)


######end covariates

result <- result[result$bmi_a <= 50, ]
######end covariates

result <- result %>%
  mutate(
    sleep_waso_level = factor(sleep_waso_level,
                              levels = c(1, 2, 3),
                              labels = c("<30 min", "30–60 min", ">60 min")
    )
  )

#sd_table_overweight_waso1 <- result %>%
#  mutate(high_bmi = if_else(bmi_level %in% c(2, 3), 1, 0))

result$sleep_waso_level_3 <- as.numeric(result$sleep_waso_level == ">60 min")
result$bmi_level <- factor(result$bmi_level, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_waso_level_3 ~ bmi_level +age2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = result)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

###
result$sleep_waso_level_2 <- as.numeric(result$sleep_waso_level == levels(result$sleep_waso_level)[2])
result$bmi_level <- factor(result$bmi_level, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_waso_level_2 ~ bmi_level +age2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = result)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

####
result$sleep_waso_level_1 <- as.numeric(result$sleep_waso_level == "<30 min")
result$bmi_level <- factor(result$bmi_level, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_waso_level_1 ~ bmi_level +age2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = result)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

#########

result$bmi_level_2 <- as.numeric(as.character(result$bmi_level))
result$bmi_level_2 <- ifelse(result$bmi_level_2 < 2, 1, 2)
result$bmi_level_2 <- factor(result$bmi_level_2, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_waso_level_1 ~ bmi_level_2 +age2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = result)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

#########
model <- glm(sleep_waso_level_2 ~ bmi_level_2 +age2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = result)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

####
model <- glm(sleep_waso_level_3 ~ bmi_level_2 +age2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = result)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

freq_of_sleep_wasolevels <- table(sd_table$sleep_waso_level)
print(freq_of_sleep_wasolevels)

#####update age, try again, try 180, 180-90, 90

write.csv(result, "df_wasobaseline.csv", row.names = FALSE)
data_dur <- read.csv("df_baselinecharacteristics.csv")

data_dur <- data_dur %>%
  mutate(sd_level = if_else(sleep_dur_level %in% c(2, 3), 1, 0))

result$sd_level <- data_dur$sd_level

result$sleep_waso_level <- ifelse(result$sd_waso < 25, 1,
                                ifelse(result$sd_waso <45, 2, 3))

result <- result %>%
  mutate(summary_c = case_when(
    sleep_waso_level == 1 & sd_level == 0 ~ 0,
    sleep_waso_level == 2 & sd_level == 0 ~ 1,
    sleep_waso_level == 1 & sd_level == 1 ~ 2,
    sleep_waso_level == 2 & sd_level == 1 ~ 3
  ),
  summary_c = as.factor(summary_c))

table(result$summary_c)

######analysis
result$sleep_pheno_3 <- as.numeric(result$summary_c == 3)
result$bmi_level <- factor(result$bmi_level, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_pheno_3 ~ bmi_level +age2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = result)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

result$sleep_pheno_2 <- as.numeric(result$summary_c == 2)
result$bmi_level <- factor(result$bmi_level, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_pheno_2 ~ bmi_level +age2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = result)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

##
result$sleep_pheno_1 <- as.numeric(result$summary_c == 1)
result$bmi_level <- factor(result$bmi_level, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_pheno_1 ~ bmi_level +age2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = result)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)
##

result$sleep_pheno_0 <- as.numeric(result$summary_c == 0)
result$bmi_level <- factor(result$bmi_level, levels = c(1, 2, 3))  # sets 'low' as reference

model <- glm(sleep_pheno_0 ~ bmi_level +age2_a + sex_numeric_a + mmrc_a + charlson_a +fev1_updated_a, 
             family = poisson(link = "log"), 
             data = result)

robust_se <- vcovHC(model, type = "HC0")
results <- coeftest(model, vcov = robust_se)
print(results)
est <- coef(model)
se <- sqrt(diag(robust_se))
PR <- exp(est)
lower_CI <- exp(est - 1.96 * se)
upper_CI <- exp(est + 1.96 * se)
PR_table <- data.frame(
  Predictor = names(est),
  PR = round(PR, 2),
  CI_lower = round(lower_CI, 2),
  CI_upper = round(upper_CI, 2)
)
print(PR_table)

result$sd_time <- data_dur$sd_time
result$sleep_dur_level <- data_dur$sleep_dur_level
result$mean_time <- data_dur$mean_time
write.csv(result, "df_foradherence.csv", row.names = FALSE)
