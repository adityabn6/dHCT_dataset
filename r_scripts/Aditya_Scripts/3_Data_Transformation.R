# 3_Data_Transformation.R

# Load all required libraries
#source("~/Roadmap_BMT_Codebase/final_scripts/0_Load_Libraries.R")

# Load custom functions
#source("~/Roadmap_BMT_Codebase/final_scripts/1_Required_Functions.R")

# NOTE: Filtering steps using merge_and_filter_summary functions and days from transplant
# have been removed. Ensure this doesn't cause issues in downstream analysis.

# Demographic & Clinical Information Processing
roadmap_ids <- roadmap_ids %>%
  mutate(
    pt_consent_date = as.POSIXct(pt_consent_date, format = "%m/%d/%y"),
    pt_day_120 = as.POSIXct(pt_day_120, format = "%m/%d/%y")
  )

bmt_demo_data <- bmt_demo_data %>%
  select(-time) %>%
  select(-1) %>%
  distinct() %>%
  rename(STUDY_PRTCPT_ID = roadmapID)

# Mood Data Processing
mood <- mood %>%
  assign_group("STUDY_PRTCPT_ID", patient_ids, caregiver_ids) %>%
  mutate(
    time_zone = str_extract(INT_SRVY_RSPNS_DT, "\\b[A-Z]{3,5}\\b"),
    time_stamp = as.POSIXct(INT_SRVY_RSPNS_DT, format = ifelse(is.na(time_zone), 
                                                               "%Y-%m-%dT%H:%M:%S", 
                                                               "%Y-%m-%d %H:%M:%S")),
    Date = as.Date(INT_SRVY_RSPNS_DT)
  ) %>%
  rename(MOOD = INT_SRVY_RSPNS)

# Sleep Data Processing
sleep <- sleep %>%
  assign_group("STUDY_PRTCPT_ID", patient_ids, caregiver_ids) %>%
  mutate(
    Date = as.Date(SLEEP_DATE, format = "%d-%b-%y"),
    SLEEP_START_DATE = as.POSIXct(SLEEP_START_DATE, format = "%d-%b-%y %I.%M.%S.%OS %p"),
    SLEEP_END_DATE = as.POSIXct(SLEEP_END_DATE, format = "%d-%b-%y %I.%M.%S.%OS %p")
  )

sleep_classic <- sleep %>% filter(TYPE == "classic")
sleep_stages <- sleep %>% filter(TYPE == "stages")

# Heart Rate Data Processing
hr <- hr %>%
  assign_group("PRTCPT_DVC_ID", patient_device_ids, caregiver_device_ids) %>%
  mutate(Date = as.Date(STUDY_METRIC_MSR_START_DT))

# Steps Data Processing
steps <- steps %>%
  assign_group("PRTCPT_DVC_ID", patient_device_ids, caregiver_device_ids) %>%
  mutate(Date = as.Date(STUDY_METRIC_MSR_START_DT))

step_count <- steps %>% filter(STUDY_METRIC_TYP_ID == 2)
activity <- steps %>% filter(!STUDY_METRIC_TYP_ID %in% c(2, 20))

# Clean up
rm(steps)

# Print summary of processed data
cat("Data processing summary:\n")
cat("Processed mood entries:", nrow(mood), "\n")
cat("Processed classic sleep entries:", nrow(sleep_classic), "\n")
cat("Processed staged sleep entries:", nrow(sleep_stages), "\n")
cat("Processed heart rate entries:", nrow(hr), "\n")
cat("Processed step count entries:", nrow(step_count), "\n")
cat("Processed activity entries:", nrow(activity), "\n")

# NOTE: The removal of filtering steps may result in larger datasets.
# Ensure that subsequent analyses account for this change and implement
# appropriate filtering as needed.