# Install if needed
#install.packages("circular")
library(dplyr)
library(lubridate)
library(circular)
data <- read.csv("R01_baseline_data.csv") 
data_date <- read.csv("BatchSleepExportDetails(2025-08-06_06-46-28).csv") 

# Assuming your data frame is called df
data_date_first <- data_date %>%
  group_by(Subject.Name) %>%
  slice(1) %>%     # keep only the first row for each ID-CODE group
  ungroup()

data_date_first$Subject.Name <- tolower(data_date_first$Subject.Name)

data_date_end <- data_date_first[!grepl("end", data_date_first$Subject.Name), ]
data_date_mid <- data_date_end[!grepl("mid", data_date_end$Subject.Name), ]
data_date_3m <- data_date_mid[!grepl("3m", data_date_mid$Subject.Name), ]
data_date_6m <- data_date_3m[!grepl("6m", data_date_3m$Subject.Name), ]
data_date_9m <- data_date_6m[!grepl("9m", data_date_6m$Subject.Name), ]
data_date_9m <- data_date_9m[!grepl("9 m", data_date_9m$Subject.Name), ]
data_date_9m <- data_date_9m[!grepl("final", data_date_9m$Subject.Name), ]
data_date_9m <- data_date_9m[!grepl("endline", data_date_9m$Subject.Name), ]

write.csv(data_date_9m, "df_sort.csv", row.names = FALSE)

data_date_9m$Subject.Name <- sub(" base| baseline| base \\(2\\)| base second", "", data_date_9m$Subject.Name)
data_date_9m$Subject.Name <- sub(" \\(2\\)", "", data_date_9m$Subject.Name)

data_date_9m$Subject.Name <- as.character(data_date_9m$Subject.Name)
data$subject_id_demographics <- as.character(data$subject_id_demographics)

data_date_9m$mcn <- data$mcn[match(data_date_9m$Subject.Name, data$subject_id_demographics)]
data_date_9m$age2 <- data$age[match(data_date_9m$Subject.Name, data$subject_id_demographics)]
data_date_9m$mmrc <- data$mmrc[match(data_date_9m$Subject.Name, data$subject_id_demographics)]
data_date_9m$charlson <- data$charlson[match(data_date_9m$Subject.Name, data$subject_id_demographics)]
data_date_9m$fev1_updated <- data$fev1_updated[match(data_date_9m$Subject.Name, data$subject_id_demographics)]
data_date_9m$education <- data$education[match(data_date_9m$Subject.Name, data$subject_id_demographics)]
data_date_9m$marital_status <- data$marital_status[match(data_date_9m$Subject.Name, data$subject_id_demographics)]
data_date_9m$race <- data$race[match(data_date_9m$Subject.Name, data$subject_id_demographics)]
data_date_9m$ethnicity <- data$ethnicity[match(data_date_9m$Subject.Name, data$subject_id_demographics)]
data_date_9m$current_smoker <- data$current_smoker[match(data_date_9m$Subject.Name, data$subject_id_demographics)]
data_date_9m$previous_smoker <- data$previous_smoker[match(data_date_9m$Subject.Name, data$subject_id_demographics)]




write.csv(data_date_9m, "df_copdbaselineenrollees.csv", row.names = FALSE)

cat(paste(data_date_9m$mcn, collapse = ","))

write.csv(data_date_9m, "df_clean_copy.csv", row.names = FALSE)
###
data_height <- read.csv("natural_language_query_download_2025_9_6-14-42-23.csv") 

data_date_9m$In.Bed.Time.Clean <- as.POSIXct(data_date_9m$In.Bed.Time, format = "%m/%d/%y")

data_height$Assessment.Date.Clean <- sub("\\..*", "", data_height$Assessment.Date)  # removes .747512
data_height$Assessment.Date.Clean <- as.POSIXct(data_height$Assessment.Date.Clean, format = "%Y-%m-%dT%H:%M:%S")
data_height$Assessment.Date.Clean <- format(data_height$Assessment.Date.Clean, "%Y-%m-%d")

names(data_height)[names(data_height) == "PAT_PATIENT_CLINIC_NUMBER"] <- "mcn"

data_date_9m$In.Bed.Time.Clean <- as.Date(data_date_9m$In.Bed.Time.Clean)
data_height$Assessment.Date.Clean <- as.Date(data_height$Assessment.Date.Clean)

check_df <- data_height %>%
  left_join(data_date_9m, by = "mcn") %>%  # Join on ID within last 600 days
  group_by(mcn) %>%
  mutate(check_col = if_else(Assessment.Date.Clean >= (In.Bed.Time.Clean - 100000) & Assessment.Date.Clean <= In.Bed.Time.Clean, "yes", "no")) %>%
  ungroup()

sum(check_df$check_col == "yes", na.rm = TRUE)
length(unique(check_df$mcn))

#check_df <- check_df %>% filter(check_col != "no")

check_df_filtered <- check_df %>%
  mutate(time_diff = abs(as.numeric(In.Bed.Time.Clean - Assessment.Date.Clean))) %>%
  group_by(mcn) %>%
  slice_min(order_by = time_diff, with_ties = FALSE) %>%
  ungroup() %>%
  select(-time_diff) 

length(unique(check_df_filtered$mcn))
write.csv(check_df_filtered, "df_height.csv", row.names = FALSE)
#12/27/18 0:14