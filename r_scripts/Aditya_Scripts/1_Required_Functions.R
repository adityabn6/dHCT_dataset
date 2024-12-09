# Consolidated_Required_Functions.R

# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

#' Remove duplicates from a dataframe based on specified columns
#'
#' @param df A dataframe
#' @param ... Columns to consider for uniqueness
#' @return A dataframe with duplicates removed
remove_duplicates <- function(df, ...) {
  df %>% distinct(..., .keep_all = TRUE)
}

#' Assign groups to participants based on their IDs
#'
#' @param df A dataframe containing participant data
#' @param id_col The column name containing participant IDs
#' @param patient_ids A vector of patient IDs
#' @param caregiver_ids A vector of caregiver IDs
#' @return A dataframe with a new 'Group' column and non-matching rows removed
assign_group <- function(df, id_col, patient_ids, caregiver_ids) {
  df$Group <- case_when(
    df[[id_col]] %in% patient_ids ~ "Patients",
    df[[id_col]] %in% caregiver_ids ~ "Caregivers",
    TRUE ~ "Other"
  )
  df %>% filter(Group != "Other")
}

#' Check if any value in a vector is not NA
#'
#' @param x A vector
#' @return TRUE if any value is not NA, FALSE otherwise
not_all_na <- function(x) any(!is.na(x))

#' Check if all values in a vector are not NA
#'
#' @param x A vector
#' @return TRUE if all values are not NA, FALSE otherwise
not_any_na <- function(x) all(!is.na(x))

#' Preprocess IDs and create dictionaries for pseudo IDs
#'
#' @param roadmap_ids A dataframe containing roadmap IDs
#' @param device_ids A dataframe containing device IDs
#' @return A list containing combined IDs and dictionaries for pseudo IDs
preprocess_ids <- function(roadmap_ids, device_ids) {
  roadmap_ids_filtered <- roadmap_ids %>%
    filter(cohort != "Peds")
  
  device_ids <- device_ids %>%
    mutate(
      PRTCPT_DVC_START_DT = as.POSIXct(PRTCPT_DVC_START_DT, format = "%d-%b-%y %I.%M.%OS %p", tz = "UTC"),
      PRTCPT_DVC_END_DT = as.POSIXct(PRTCPT_DVC_END_DT, format = "%d-%b-%y %I.%M.%OS %p", tz = "UTC")
    )
  
  combined_ids <- bind_rows(
    roadmap_ids_filtered %>%
      left_join(device_ids, by = c("pt_rm_access_code" = "STUDY_PRTCPT_ID")) %>%
      mutate(STUDY_PRTCPT_ID = pt_rm_access_code),
    roadmap_ids_filtered %>%
      left_join(device_ids, by = c("cg_rm_access_code" = "STUDY_PRTCPT_ID")) %>%
      mutate(STUDY_PRTCPT_ID = cg_rm_access_code)
  ) %>%
    distinct()
  
  participant_dict <- setNames(paste0("P", sprintf("%03d", seq_along(unique(combined_ids$STUDY_PRTCPT_ID)))), 
                               unique(combined_ids$STUDY_PRTCPT_ID))
  
  device_dict <- setNames(paste0(participant_dict[combined_ids$STUDY_PRTCPT_ID], "_D", 
                                 sprintf("%02d", sequence(rle(combined_ids$STUDY_PRTCPT_ID)$lengths))), 
                          combined_ids$PRTCPT_DVC_ID)
  
  list(combined_ids = combined_ids, participant_dict = participant_dict, device_dict = device_dict)
}

#' Censor and filter mood data
#'
#' @param mood_df A dataframe containing mood data
#' @param roadmap_ids A dataframe containing roadmap IDs
#' @param participant_dict A dictionary for participant IDs
#' @param censor Boolean indicating whether to censor the data
#' @return A filtered and optionally censored dataframe
censored_merge_filter_mood <- function(mood_df, roadmap_ids, participant_dict, censor = FALSE) {
  merged_df <- mood_df %>%
    left_join(roadmap_ids, by = c("STUDY_PRTCPT_ID" = "pt_rm_access_code")) %>%
    left_join(roadmap_ids, by = c("STUDY_PRTCPT_ID" = "cg_rm_access_code")) %>%
    mutate(
      pt_consent_date = coalesce(pt_consent_date.x, pt_consent_date.y),
      pt_day_120 = coalesce(pt_day_120.x, pt_day_120.y)
    ) %>%
    select(-ends_with(".x"), -ends_with(".y")) %>%
    mutate(
      transplant_date = pt_day_120 - days(120),
      DaysFromTransplant = as.integer(difftime(Date, transplant_date, units = "days"))
    ) %>%
    filter(DaysFromTransplant >= 0 & DaysFromTransplant <= 120)
  
  if (censor) {
    censored_df <- merged_df %>%
      mutate(
        STUDY_PRTCPT_ID = participant_dict[STUDY_PRTCPT_ID]
      ) %>%
      select(-Date, -pt_consent_date, -pt_day_120, -transplant_date,
             -INT_SRVY_RSPNS_DT, -time_zone, -INT_SRVY_RSPNS_ID, -STUDY_ID, -INT_SRVY_ID, -INT_SRVY_QSTN_ID, -cg_rm_access_code, -pt_rm_access_code) %>%
      mutate(time_stamp = format(time_stamp, format = "%H:%M:%S"))
    
    return(censored_df)
  } else {
    return(merged_df)
  }
}

#' Censor and filter sleep data
#'
#' @param sleep_df A dataframe containing sleep data
#' @param roadmap_ids A dataframe containing roadmap IDs
#' @param participant_dict A dictionary for participant IDs
#' @param censor Boolean indicating whether to censor the data
#' @return A filtered and optionally censored dataframe
censored_merge_filter_sleep <- function(sleep_df, roadmap_ids, participant_dict, censor = FALSE) {
  merged_df <- sleep_df %>%
    left_join(roadmap_ids, by = c("STUDY_PRTCPT_ID" = "pt_rm_access_code")) %>%
    left_join(roadmap_ids, by = c("STUDY_PRTCPT_ID" = "cg_rm_access_code")) %>%
    mutate(
      pt_consent_date = coalesce(pt_consent_date.x, pt_consent_date.y),
      pt_day_120 = coalesce(pt_day_120.x, pt_day_120.y)
    ) %>%
    select(-ends_with(".x"), -ends_with(".y")) %>%
    mutate(
      transplant_date = pt_day_120 - days(120),
      DaysFromTransplant = as.integer(difftime(Date, transplant_date, units = "days"))
    ) %>%
    filter(DaysFromTransplant >= 0 & DaysFromTransplant <= 120)
  
  if (censor) {
    censored_df <- merged_df %>%
      mutate(
        STUDY_PRTCPT_ID = participant_dict[STUDY_PRTCPT_ID]
      ) %>%
      select(-SLEEP_DATE, -Date, -pt_rm_access_code, -cg_rm_access_code, -pt_consent_date, -pt_day_120) %>%
      mutate(
        SLEEP_START_TIME = format(SLEEP_START_DATE, format = "%H:%M:%S"),
        SLEEP_END_TIME = format(SLEEP_END_DATE, format = "%H:%M:%S")
      ) %>%
      select(-SLEEP_START_DATE, -SLEEP_END_DATE, -SLEEP_SUMMARY_ID, -STUDY_ID, -transplant_date) %>%
      select_if(~!all(is.na(.)))
    
    return(censored_df)
  } else {
    return(merged_df)
  }
}

#' Censor demographic data
#'
#' @param demo_df A dataframe containing demographic data
#' @param participant_dict A dictionary for participant IDs
#' @return A censored dataframe with categorized age and income
censor_demographic_data <- function(demo_df, participant_dict) {
  censored_df <- demo_df %>%
    mutate(
      STUDY_PRTCPT_ID = participant_dict[STUDY_PRTCPT_ID],
      age = case_when(
        age == "18-39" ~ "18-39",
        age == "40-60" ~ "40-60",
        age == "61+" ~ "61+",
        TRUE ~ NA_character_
      ),
      monthly_income = case_when(
        monthly_income == "Less than $1,000" ~ "< $1,000",
        monthly_income == "$1,000 to $2,999" ~ "$1,000 - $2,999",
        monthly_income == "$3,000 to $4,999" ~ "$3,000 - $4,999",
        monthly_income == "$5,000 to $6,999" ~ "$5,000 - $6,999",
        monthly_income == "$7,000 or more" ~ "≥ $7,000",
        TRUE ~ NA_character_
      )
    )
  
  return(censored_df)
}

#' Censor and filter heart rate data
#'
#' @param hr_df A dataframe containing heart rate data
#' @param device_ids A dataframe containing device IDs
#' @param roadmap_ids A dataframe containing roadmap IDs
#' @param preprocessed_data A list containing preprocessed data
#' @param participant_dict A dictionary for participant IDs
#' @return A censored and filtered dataframe
censor_and_filter_hr <- function(hr_df, device_ids, roadmap_ids, preprocessed_data, participant_dict) {
  setDT(hr_df)
  setDT(device_ids)
  setDT(roadmap_ids)
  
  # Merge with device_ids to add STUDY_PRTCPT_ID
  hr_df <- merge(hr_df, device_ids[, .(PRTCPT_DVC_ID, STUDY_PRTCPT_ID)], by = "PRTCPT_DVC_ID", all.x = TRUE)
  
  # Merge with roadmap_ids to get pt_consent_date and pt_day_120
  hr_df <- merge(hr_df, roadmap_ids[, .(pt_rm_access_code, pt_consent_date, pt_day_120)],
                 by.x = "STUDY_PRTCPT_ID", by.y = "pt_rm_access_code", all.x = TRUE)
  
  hr_df <- merge(hr_df, roadmap_ids[, .(cg_rm_access_code, pt_consent_date, pt_day_120)],
                 by.x = "STUDY_PRTCPT_ID", by.y = "cg_rm_access_code", all.x = TRUE)
  
  # Combine pt_consent_date and pt_day_120 from both merges
  hr_df[, ':='(
    pt_consent_date = coalesce(pt_consent_date.x, pt_consent_date.y),
    pt_day_120 = coalesce(pt_day_120.x, pt_day_120.y)
  )]
  hr_df[, c("pt_consent_date.x", "pt_consent_date.y", "pt_day_120.x", "pt_day_120.y") := NULL]
  
  # Add device start and end dates
  combined_ids <- as.data.table(preprocessed_data$combined_ids)
  device_dates <- combined_ids[, .(PRTCPT_DVC_ID, PRTCPT_DVC_START_DT, PRTCPT_DVC_END_DT, 
                                   transplant_date = pt_day_120 - days(120))]
  setkey(device_dates, PRTCPT_DVC_ID)
  
  hr_df[device_dates, ':='(
    PRTCPT_DVC_START_DT = i.PRTCPT_DVC_START_DT,
    PRTCPT_DVC_END_DT = i.PRTCPT_DVC_END_DT,
    transplant_date = i.transplant_date
  ), on = "PRTCPT_DVC_ID"]
  
  # Filter based on device dates and study period
  hr_filtered <- hr_df[STUDY_METRIC_MSR_START_DT >= PRTCPT_DVC_START_DT & 
                         (STUDY_METRIC_MSR_START_DT <= PRTCPT_DVC_END_DT | is.na(PRTCPT_DVC_END_DT)) &
                         Date >= pt_consent_date & Date <= pt_day_120]
  
  hr_filtered[, DaysFromTransplant := as.integer(difftime(Date, transplant_date, units = "days"))]
  hr_filtered <- hr_filtered[DaysFromTransplant >= 0 & DaysFromTransplant <= 120]
  
  # Censor the data
  hr_censored <- hr_filtered[, .(
    STUDY_PRTCPT_ID = participant_dict[STUDY_PRTCPT_ID],
    PRTCPT_DVC_ID = preprocessed_data$device_dict[as.character(PRTCPT_DVC_ID)],
    STUDY_METRIC_MSR_VAL,
    STUDY_METRIC_MSR_TIME = format(as.ITime(STUDY_METRIC_MSR_START_DT), "%H:%M:%S"),
    Group,
    DaysFromTransplant
  )]
  
  return(hr_censored)
}

#' Censor and filter step data
#'
#' @param step_df A dataframe containing step data
#' @param hr_censored A dataframe containing censored heart rate data
#' @param preprocessed_data A list containing preprocessed data
#' @param participant_dict A dictionary for participant IDs
#' @return A censored and filtered dataframe
censor_and_filter_steps <- function(steps_df, device_ids, roadmap_ids, preprocessed_data, participant_dict, hr_censored) {
  setDT(steps_df)
  setDT(device_ids)
  setDT(roadmap_ids)
  setDT(hr_censored)
  
  # Merge with device_ids to add STUDY_PRTCPT_ID
  steps_df <- merge(steps_df, device_ids[, .(PRTCPT_DVC_ID, STUDY_PRTCPT_ID)], by = "PRTCPT_DVC_ID", all.x = TRUE)
  
  # Merge with roadmap_ids to get pt_consent_date and pt_day_120
  steps_df <- merge(steps_df, roadmap_ids[, .(pt_rm_access_code, pt_consent_date, pt_day_120)],
                    by.x = "STUDY_PRTCPT_ID", by.y = "pt_rm_access_code", all.x = TRUE)
  
  steps_df <- merge(steps_df, roadmap_ids[, .(cg_rm_access_code, pt_consent_date, pt_day_120)],
                    by.x = "STUDY_PRTCPT_ID", by.y = "cg_rm_access_code", all.x = TRUE)
  
  # Combine pt_consent_date and pt_day_120 from both merges
  steps_df[, ':='(
    pt_consent_date = coalesce(pt_consent_date.x, pt_consent_date.y),
    pt_day_120 = coalesce(pt_day_120.x, pt_day_120.y)
  )]
  steps_df[, c("pt_consent_date.x", "pt_consent_date.y", "pt_day_120.x", "pt_day_120.y") := NULL]
  
  # Add device start and end dates
  combined_ids <- as.data.table(preprocessed_data$combined_ids)
  device_dates <- combined_ids[, .(PRTCPT_DVC_ID, PRTCPT_DVC_START_DT, PRTCPT_DVC_END_DT, 
                                   transplant_date = pt_day_120 - days(120))]
  setkey(device_dates, PRTCPT_DVC_ID)
  
  steps_df[device_dates, ':='(
    PRTCPT_DVC_START_DT = i.PRTCPT_DVC_START_DT,
    PRTCPT_DVC_END_DT = i.PRTCPT_DVC_END_DT,
    transplant_date = i.transplant_date
  ), on = "PRTCPT_DVC_ID"]
  
  # Filter based on device dates and study period
  steps_filtered <- steps_df[STUDY_METRIC_MSR_START_DT >= PRTCPT_DVC_START_DT & 
                               (STUDY_METRIC_MSR_START_DT <= PRTCPT_DVC_END_DT | is.na(PRTCPT_DVC_END_DT)) &
                               Date >= pt_consent_date & Date <= pt_day_120]
  
  steps_filtered[, DaysFromTransplant := as.integer(difftime(Date, transplant_date, units = "days"))]
  steps_filtered <- steps_filtered[DaysFromTransplant >= 0 & DaysFromTransplant <= 120]
  
  # Censor the data
  steps_censored <- steps_filtered[, .(
    STUDY_PRTCPT_ID = participant_dict[STUDY_PRTCPT_ID],
    PRTCPT_DVC_ID = preprocessed_data$device_dict[as.character(PRTCPT_DVC_ID)],
    STUDY_METRIC_TYP_ID,
    STUDY_METRIC_MSR_VAL,
    STUDY_METRIC_MSR_TIME = format(as.ITime(STUDY_METRIC_MSR_START_DT), "%H:%M:%S"),
    Group,
    DaysFromTransplant
  )]
  
  
  # Additional filtering step using hr_censored after censoring
  hr_valid_times <- unique(hr_censored[, .(PRTCPT_DVC_ID, STUDY_METRIC_MSR_TIME, DaysFromTransplant)])
  setkey(hr_valid_times, PRTCPT_DVC_ID, STUDY_METRIC_MSR_TIME, DaysFromTransplant)
  setkey(steps_censored, PRTCPT_DVC_ID, STUDY_METRIC_MSR_TIME, DaysFromTransplant)
  
  steps_final <- steps_censored[hr_valid_times, nomatch=0]
  
  return(steps_final)
}


#' Create daily heart rate summary
#'
#' @param censored_hr_data A data.table containing filtered and censored heart rate data
#' @return A data.table with daily heart rate summary statistics
create_daily_hr_summary <- function(hr_censored) {
  # Ensure input is a data.table
  setDT(hr_censored)
  
  # Convert STUDY_METRIC_MSR_TIME to POSIXct
  hr_censored[, STUDY_METRIC_MSR_TIME := as.POSIXct(STUDY_METRIC_MSR_TIME, format = "%H:%M:%S")]
  
  # Extract hour from STUDY_METRIC_MSR_TIME
  hr_censored[, Hour := hour(STUDY_METRIC_MSR_TIME)]
  
  # Aggregate to minute-level
  minute_level_hr <- hr_censored[, .(
    hr_minute = median(STUDY_METRIC_MSR_VAL, na.rm = TRUE),
    Group = first(Group)  # Include Group in the minute-level aggregation
  ), by = .(STUDY_PRTCPT_ID, DaysFromTransplant, Minute = floor_date(STUDY_METRIC_MSR_TIME, "minute"), Hour)]
  
  # Create daily summary
  daily_hr <- minute_level_hr[, .(
    mean_hr = mean(hr_minute, na.rm = TRUE),
    median_hr = median(hr_minute, na.rm = TRUE),
    min_hr = min(hr_minute, na.rm = TRUE),
    max_hr = max(hr_minute, na.rm = TRUE),
    sd_hr = sd(hr_minute, na.rm = TRUE),
    q1_hr = quantile(hr_minute, 0.25, na.rm = TRUE),
    q3_hr = quantile(hr_minute, 0.75, na.rm = TRUE),
    n_minutes = .N,
    first_measurement = min(Minute),
    last_measurement = max(Minute),
    
    # Diurnal rhythm features
    morning_hr = mean(hr_minute[Hour >= 6 & Hour < 12], na.rm = TRUE),
    afternoon_hr = mean(hr_minute[Hour >= 12 & Hour < 18], na.rm = TRUE),
    evening_hr = mean(hr_minute[Hour >= 18 & Hour < 22], na.rm = TRUE),
    night_hr = mean(hr_minute[Hour >= 22 | Hour < 6], na.rm = TRUE),
    
    hr_range = max(hr_minute, na.rm = TRUE) - min(hr_minute, na.rm = TRUE),
    hr_acrophase = Hour[which.max(hr_minute)],
    hr_bathyphase = Hour[which.min(hr_minute)],
    
    hr_mesor = mean(hr_minute, na.rm = TRUE),
    hr_amplitude = (max(hr_minute, na.rm = TRUE) - min(hr_minute, na.rm = TRUE)) / 2,
    
    Group = first(Group)  # Include Group in the daily summary
  ), by = .(STUDY_PRTCPT_ID, DaysFromTransplant)]
  
  # Calculate additional metrics
  daily_hr[, day_coverage := as.numeric(difftime(last_measurement, first_measurement, units = "mins")) + 1]
  daily_hr[, `:=`(
    iqr_hr = q3_hr - q1_hr,
    time_coverage = round(n_minutes / 1440, 4),  # 1440 minutes in a day
    theoretical_coverage = round(day_coverage / 1440, 4),
    
    # Diurnal variation metrics
    diurnal_variation = max(morning_hr, afternoon_hr, evening_hr, night_hr, na.rm = TRUE) - 
      min(morning_hr, afternoon_hr, evening_hr, night_hr, na.rm = TRUE),
    
    morning_evening_difference = morning_hr - evening_hr,
    
    circadian_quotient = hr_amplitude / hr_mesor
  )]
  
  return(daily_hr)
}



#' Create daily step summary
#'
#' @param filtered_step_data A data.table containing filtered step data
#' @return A data.table with daily step summary statistics
create_daily_step_summary <- function(filtered_step_data) {
  setDT(filtered_step_data)
  
  # Aggregate steps to daily level
  daily_steps <- filtered_step_data[, .(
    total_steps = sum(STUDY_METRIC_MSR_VAL, na.rm = TRUE),
    n_measurements = .N,
    first_measurement = min(as.POSIXct(STUDY_METRIC_MSR_TIME, format = "%H:%M:%S")),
    last_measurement = max(as.POSIXct(STUDY_METRIC_MSR_TIME, format = "%H:%M:%S")),
    Group = first(Group)
  ), by = .(STUDY_PRTCPT_ID, DaysFromTransplant)]
  
  daily_steps[, ':='(
    day_coverage = as.numeric(difftime(last_measurement, first_measurement, units = "hours")),
    time_coverage = round(n_measurements / (24 * 60), 4)  # Assuming one measurement per minute
  )]
  
  return(daily_steps)
}

#' Create daily activity summary
#'
#' @param activity_censored A data.table containing filtered and censored activity data
#' @return A data.table with daily activity summary statistics
create_daily_activity_summary <- function(activity_censored) {
  # Ensure input is a data.table
  setDT(activity_censored)
  
  # Convert STUDY_METRIC_MSR_TIME to POSIXct
  activity_censored[, STUDY_METRIC_MSR_TIME := as.POSIXct(STUDY_METRIC_MSR_TIME, format = "%H:%M:%S")]
  
  # Create a mapping for activity types
  activity_types <- c(
    "4" = "sedentary",
    "5" = "lightly_active",
    "6" = "moderately_active",
    "7" = "very_active"
  )
  
  # Add activity type column
  activity_censored[, activity_type := activity_types[as.character(STUDY_METRIC_TYP_ID)]]
  
  # Aggregate activity data
  daily_activity <- activity_censored[, .(
    minutes = .N,  # Each row represents one minute
    Group = first(Group)  # Include Group in the aggregation
  ), by = .(STUDY_PRTCPT_ID, DaysFromTransplant, activity_type)]
  
  # Reshape the data to wide format
  daily_activity_wide <- dcast(daily_activity, 
                               STUDY_PRTCPT_ID + DaysFromTransplant + Group ~ activity_type, 
                               value.var = "minutes", 
                               fill = 0)
  
  # Calculate total active time and total measured time
  daily_activity_wide[, `:=`(
    total_active_time = lightly_active + moderately_active + very_active,
    total_measured_time = sedentary + lightly_active + moderately_active + very_active
  )]
  
  # Calculate activity percentages
  daily_activity_wide[, `:=`(
    percent_sedentary = sedentary / total_measured_time * 100,
    percent_lightly_active = lightly_active / total_measured_time * 100,
    percent_moderately_active = moderately_active / total_measured_time * 100,
    percent_very_active = very_active / total_measured_time * 100,
    percent_active = total_active_time / total_measured_time * 100
  )]
  
  # Calculate coverage
  daily_activity_wide[, `:=`(
    time_coverage = round(total_measured_time / 1440, 4)  # 1440 minutes in a day
  )]
  
  return(daily_activity_wide)
}
