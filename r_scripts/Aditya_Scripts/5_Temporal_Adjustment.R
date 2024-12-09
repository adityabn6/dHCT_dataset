# 6_Temporal_Adjustment.R


# Create daily summaries
daily_hr_summary <- create_daily_hr_summary(hr_censored)
daily_step_summary <- create_daily_step_summary(steps_censored)
daily_activity_summary <- create_daily_activity_summary(activity_censored)

# Print summary of temporal adjustments
cat("Temporal adjustment summary:\n")
cat("Daily heart rate summaries:", nrow(daily_hr_summary), "\n")
cat("Daily step summaries:", nrow(daily_step_summary), "\n")

# Save daily summary datasets
#save(daily_hr_summary, daily_step_summary, file = "~/Roadmap_BMT_Codebase/daily_summaries.RData")

# Optional: Display first few rows of each summary
print(head(daily_hr_summary))
print(head(daily_step_summary))

write.csv(mood_censored,"mood_censored.csv")
write.csv(sleep_c_censored,"sleep_c_censored.csv")
write.csv(sleep_s_censored,"sleep_s_censored.csv")
write.csv(daily_hr_summary,"daily_hr_summary.csv")
write.csv(daily_step_summary,"daily_step_summary.csv")
write.csv(daily_activity_summary,"daily_activity_summary.csv")


write.csv(activity_censored,"activity_censored.csv")
write.csv(steps_censored,"steps_censored.csv")
write.csv(hr_censored,"hr_censored.csv")

