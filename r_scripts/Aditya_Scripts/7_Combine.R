library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

# Process Mood data
mood_processed <- mood_censored %>%
  mutate(time_stamp = as.POSIXct(time_stamp, format = "%H:%M:%S"))

library(dplyr)
library(lubridate)

process_sleep_data <- function(sleep_data) {
  sleep_data %>%
    mutate(
      date = as.Date(DaysFromTransplant, origin = "1970-01-01"),
      sleep_start = as.POSIXct(paste(date, SLEEP_START_TIME), format="%Y-%m-%d %H:%M:%S"),
      sleep_end = as.POSIXct(paste(date, SLEEP_END_TIME), format="%Y-%m-%d %H:%M:%S")
    ) %>%
    # Adjust for sleep periods crossing midnight
    mutate(
      sleep_end = if_else(sleep_end < sleep_start, sleep_end + days(1), sleep_end),
      sleep_duration = as.numeric(difftime(sleep_end, sleep_start, units = "mins")),
      sleep_efficiency = ASLEEP_VALUE / INBED_VALUE,
      # Categorize sleep periods
      sleep_category = case_when(
        hour(sleep_start) >= 20 | hour(sleep_start) < 4 ~ "Night",
        hour(sleep_start) >= 4 & hour(sleep_start) < 12 ~ "Morning",
        hour(sleep_start) >= 12 & hour(sleep_start) < 20 ~ "Afternoon"
      )
    ) %>%
    # Group by participant, day, and sleep category
    group_by(STUDY_PRTCPT_ID, DaysFromTransplant) %>%
    # Select the best sleep bout within each category
    slice_max(order_by = sleep_efficiency * sleep_duration, n = 1) %>%
    ungroup() %>%
    # If there are still multiple bouts, select the longest night sleep
    group_by(STUDY_PRTCPT_ID, DaysFromTransplant) %>%
    slice_max(order_by = sleep_duration, n = 1) %>%
    ungroup()
}

# Process Sleep Classic data
longest_sleep_classic <- process_sleep_data(sleep_c_censored)

# Process Sleep Stages data
longest_sleep_stages <- process_sleep_data(sleep_s_censored)
# Combine all datasets
combined_data <- censored_demo_data %>%
  full_join(mood_processed, by = c("STUDY_PRTCPT_ID")) %>%
  full_join(longest_sleep_classic, by = c("STUDY_PRTCPT_ID", "DaysFromTransplant", "Group"), suffix = c("", "_classic")) %>%
  full_join(longest_sleep_stages, by = c("STUDY_PRTCPT_ID", "DaysFromTransplant", "Group"), suffix = c("", "_stages")) %>%
  full_join(daily_hr_summary, by = c("STUDY_PRTCPT_ID", "DaysFromTransplant", "Group")) %>%
  full_join(daily_step_summary, by = c("STUDY_PRTCPT_ID", "DaysFromTransplant", "Group")) %>%
  full_join(daily_activity_summary, by = c("STUDY_PRTCPT_ID", "DaysFromTransplant", "Group"))

# Add a dyad identifier
combined_data <- combined_data %>%
  group_by(dyad_id) %>%
  mutate(patient_id = STUDY_PRTCPT_ID[role == "Patients"],
         caregiver_id = STUDY_PRTCPT_ID[role == "Caregivers"]) %>%
  ungroup()