# Prepare mood data
mood_avg <- mood_censored %>%
  group_by(Group, DaysFromTransplant) %>%
  summarize(avg_mood = mean(MOOD, na.rm = TRUE))

# Create mood plot
mood_plot <- ggplot(mood_avg, aes(x = DaysFromTransplant, y = avg_mood, color = Group)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_viridis_d() +
  labs(title = "Average Mood Over Time",
       x = "Days From Transplant",
       y = "Average Mood Score",
       color = "Group") +
  theme_minimal()

print(mood_plot)

# Prepare classic sleep data
sleep_classic_avg <- sleep_c_censored %>%
  group_by(Group, DaysFromTransplant) %>%
  summarize(
    avg_total_sleep_time = mean(ASLEEP_VALUE, na.rm = TRUE) / 60,  # Convert to hours
    avg_time_in_bed = mean(INBED_VALUE, na.rm = TRUE) / 60,  # Convert to hours
    avg_sleep_efficiency = (mean(ASLEEP_VALUE, na.rm = TRUE)/ mean(INBED_VALUE, na.rm = TRUE))* 100,  # Convert to percentage
    .groups = "drop"
  ) %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "metric", values_to = "value")

# Create classic sleep plot
sleep_classic_plot <- ggplot(sleep_classic_avg, aes(x = DaysFromTransplant, y = value, color = Group)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ metric, scales = "free_y", 
             labeller = labeller(metric = c(avg_total_sleep_time = "Total Sleep Time (hours)",
                                            avg_time_in_bed = "Time in Bed (hours)",
                                            avg_sleep_efficiency = "Sleep Efficiency (%)"))) +
  scale_color_viridis_d() +
  labs(title = "Average Daily Sleep Metrics (Classic) Over Time",
       x = "Days From Transplant",
       y = "Value",
       color = "Group") +
  theme_minimal()

print(sleep_classic_plot)



# Prepare sleep stages data
sleep_stages_avg <- sleep_s_censored %>%
  group_by(Group, DaysFromTransplant) %>%
  summarize(
    avg_light_sleep = mean(LIGHT_MIN, na.rm = TRUE) / 60,  # Convert to hours
    avg_deep_sleep = mean(DEEP_MIN, na.rm = TRUE) / 60,  # Convert to hours
    avg_rem_sleep = mean(REM_MIN, na.rm = TRUE) / 60,  # Convert to hours
    .groups = "drop"
  ) %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "stage", values_to = "hours")

# Create sleep stages plot
sleep_stages_plot <- ggplot(sleep_stages_avg, aes(x = DaysFromTransplant, y = hours, color = Group)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ stage, scales = "free_y", 
             labeller = labeller(stage = c(avg_light_sleep = "Light Sleep",
                                           avg_deep_sleep = "Deep Sleep",
                                           avg_rem_sleep = "REM Sleep"))) +
  scale_color_viridis_d() +
  labs(title = "Average Daily Sleep Stages Over Time",
       x = "Days From Transplant",
       y = "Hours",
       color = "Group") +
  theme_minimal()

print(sleep_stages_plot)


# Prepare heart rate data
hr_avg <- daily_hr_summary %>%
  group_by(Group, DaysFromTransplant) %>%
  summarize(avg_hr = mean(mean_hr, na.rm = TRUE), .groups = "drop")

# Create heart rate plot
hr_plot <- ggplot(hr_avg, aes(x = DaysFromTransplant, y = avg_hr, color = Group)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_viridis_d() +
  labs(title = "Average Daily Heart Rate Over Time",
       x = "Days From Transplant",
       y = "Average Heart Rate (bpm)",
       color = "Group") +
  theme_minimal()

print(hr_plot)

# Prepare step count data
steps_avg <- daily_step_summary %>%
  group_by(Group, DaysFromTransplant) %>%
  summarize(avg_steps = mean(total_steps, na.rm = TRUE), .groups = "drop")

# Create step count plot
steps_plot <- ggplot(steps_avg, aes(x = DaysFromTransplant, y = avg_steps, color = Group)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Daily Step Count Over Time",
       x = "Days From Transplant",
       y = "Average Steps",
       color = "Group") +
  theme_minimal()

print(steps_plot)


# Prepare activity data
activity_avg <- daily_activity_summary %>%
  group_by(Group, DaysFromTransplant) %>%
  summarize(
    avg_sed = mean(sedentary, na.rm = TRUE),
    avg_light = mean(lightly_active, na.rm = TRUE),
    avg_moderate = mean(moderately_active, na.rm = TRUE),
    avg_very = mean(very_active, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(avg_sed, avg_light, avg_moderate, avg_very),
               names_to = "activity_type", values_to = "avg_minutes")

# Create activity plot
activity_plot <- ggplot(activity_avg, aes(x = DaysFromTransplant, y = avg_minutes, color = Group)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ activity_type, scales = "free_y", 
             labeller = labeller(activity_type = c(avg_sed = "Sedentary", 
                                                   avg_light = "Light Activity",
                                                   avg_moderate = "Moderate Activity",
                                                   avg_very = "Very Active"))) +
  scale_color_viridis_d() +
  labs(title = "Average Daily Activity Minutes Over Time",
       x = "Days From Transplant",
       y = "Average Minutes",
       color = "Group") +
  theme_minimal()

print(activity_plot)

library(tidyverse)
library(ggplot2)
library(viridis)

# Create dyad mapping with transplant dates
dyad_mapping <- censored_demo_data %>%
  select(STUDY_PRTCPT_ID, dyad_id) %>%
  distinct() %>%
  left_join(enframe(preprocessed_data$participant_dict, name = "original_id", value = "STUDY_PRTCPT_ID"),
            by = "STUDY_PRTCPT_ID") %>%
  left_join(roadmap_ids %>% 
              select(pt_rm_access_code, cg_rm_access_code, pt_transplant_date) %>%
              mutate(pt_transplant_date = as.Date(pt_transplant_date, format = "%m/%d/%y")),
            by = c("original_id" = "pt_rm_access_code")) %>%
  left_join(roadmap_ids %>% 
              select(pt_rm_access_code, cg_rm_access_code, pt_transplant_date) %>%
              mutate(pt_transplant_date = as.Date(pt_transplant_date, format = "%m/%d/%y")),
            by = c("original_id" = "cg_rm_access_code")) %>%
  mutate(pt_transplant_date = coalesce(pt_transplant_date.x, pt_transplant_date.y)) %>%
  select(-pt_transplant_date.x, -pt_transplant_date.y) %>%
  distinct(dyad_id, .keep_all = TRUE) %>%
  arrange(pt_transplant_date) %>%
  mutate(dyad_order = row_number())

# Function to prepare heatmap data
prepare_heatmap_data <- function(data) {
  data %>%
    select(STUDY_PRTCPT_ID, Group, DaysFromTransplant) %>%
    distinct() %>%
    left_join(dyad_mapping, by = "STUDY_PRTCPT_ID") %>%
    group_by(dyad_id, DaysFromTransplant, dyad_order) %>%
    summarise(
      patient_reporting = any(Group == "Patients"),
      caregiver_reporting = any(Group == "Caregivers"),
      .groups = "drop"
    ) %>%
    mutate(reporting = case_when(
      patient_reporting & caregiver_reporting ~ "Both",
      patient_reporting & !caregiver_reporting ~ "Patient Only",
      !patient_reporting & caregiver_reporting ~ "Caregiver Only",
      !patient_reporting & !caregiver_reporting ~ "Neither"
    ))
}

# Prepare mood heatmap data
mood_heatmap_data <- prepare_heatmap_data(mood_censored)

# Prepare sleep heatmap data
sleep_heatmap_data <- prepare_heatmap_data(sleep_c_censored)

# Function to create heatmap
create_heatmap <- function(data, title) {
  ggplot(data, aes(x = DaysFromTransplant, y = reorder(factor(dyad_id), dyad_order), fill = reporting)) +
    geom_tile() +
    scale_fill_manual(values = c("Both" = "#1E90FF",         # Dodger Blue
                                 "Patient Only" = "#FFA500", # Orange
                                 "Caregiver Only" = "#FF1493", # Deep Pink
                                 "Neither" = "#FFFFFF"),     # White
                      name = "Reporting") +
    labs(title = title,
         x = "Days From Transplant",
         y = "Dyad ID (ordered by transplant date)") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())
}

# Create mood heatmap
mood_heatmap <- create_heatmap(mood_heatmap_data, "Heatmap of MOOD Reporting")
print(mood_heatmap)

# Create sleep heatmap
sleep_heatmap <- create_heatmap(sleep_heatmap_data, "Heatmap of SLEEP Reporting")
print(sleep_heatmap)