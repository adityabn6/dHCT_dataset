# 5_Data_Censoring.R

# Load required libraries and functions
#source("~/Roadmap_BMT_Codebase/final_scripts/1_Load_Libraries.R")
#source("~/Roadmap_BMT_Codebase/final_scripts/Consolidated_Required_Functions.R")

# Preprocess IDs
preprocessed_data <- preprocess_ids(roadmap_ids, device_ids)

# Censor and filter mood data
mood_censored <- censored_merge_filter_mood(mood, 
                                            roadmap_ids,
                                            preprocessed_data$participant_dict, 
                                            censor = TRUE)

# Censor and filter sleep data
sleep_c_censored <- censored_merge_filter_sleep(sleep_classic, 
                                                roadmap_ids,
                                                preprocessed_data$participant_dict, 
                                                censor = TRUE)

sleep_s_censored <- censored_merge_filter_sleep(sleep_stages, 
                                                roadmap_ids,
                                                preprocessed_data$participant_dict, 
                                                censor = TRUE)

# Censor demographic data
censored_demo_data <- censor_demographic_data(bmt_demo_data, preprocessed_data$participant_dict)

# Censor and filter heart rate data
hr_censored <- censor_and_filter_hr(hr, device_ids, roadmap_ids, preprocessed_data, preprocessed_data$participant_dict)

# Censor and filter step data
steps_censored <- censor_and_filter_steps(step_count, device_ids, roadmap_ids, preprocessed_data, preprocessed_data$participant_dict, hr_censored)
activity_censored <- censor_and_filter_steps(activity, device_ids, roadmap_ids, preprocessed_data, preprocessed_data$participant_dict, hr_censored)

# Drop duplicates based on PRTCPT_DVC_ID, STUDY_METRIC_MSR_TIME, DaysFromTransplant
# and retain the row with largest STUDY_METRIC_TYP_ID value
steps_censored <- steps_censored[order(-STUDY_METRIC_TYP_ID)]
steps_censored <- unique(steps_censored, by = c("PRTCPT_DVC_ID", "STUDY_METRIC_MSR_TIME", "DaysFromTransplant"))

activity_censored <- activity_censored[order(-STUDY_METRIC_TYP_ID)]
activity_censored <- unique(activity_censored, by = c("PRTCPT_DVC_ID", "STUDY_METRIC_MSR_TIME", "DaysFromTransplant"))

# Print summary of censored data
cat("Data censoring summary:\n")
cat("Censored mood entries:", nrow(mood_censored), "\n")
cat("Censored classic sleep entries:", nrow(sleep_c_censored), "\n")
cat("Censored staged sleep entries:", nrow(sleep_s_censored), "\n")
cat("Censored demographic entries:", nrow(censored_demo_data), "\n")
cat("Censored heart rate entries:", nrow(hr_censored), "\n")
cat("Censored step entries:", nrow(steps_censored), "\n")
cat("Censored activity entries:", nrow(activity_censored), "\n")

# Optional: Save censored datasets
# save(MOOD_censored, SLEEP_C_censored, SLEEP_S_censored, censored_demo_data, hr_censored, steps_censored,
#      file = "~/Roadmap_BMT_Codebase/censored_data.RData")