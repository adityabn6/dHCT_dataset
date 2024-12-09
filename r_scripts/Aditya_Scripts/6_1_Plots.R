# Set up theme for publication quality plots
publication_theme <- theme_minimal() +
  theme(
    text = element_text(size = 14),  # Base font size
    axis.title = element_text(size = 16, face = "bold"),  # Larger axis titles
    axis.text = element_text(size = 12),  # Readable axis text
    legend.text = element_text(size = 12),  # Legend text size
    legend.title = element_text(size = 14, face = "bold"),  # Legend title size
    strip.text = element_text(size = 14, face = "bold"),  # Facet label size
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Centered, large title
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# Define the custom color scale
custom_colors <- c("Patients" = "#00CED1", "Caregivers" = "#FF69B4")

# Function to save plots with consistent settings
save_plot <- function(plot, filename, width = 10, height = 8) {
  ggsave(
    filename = paste0(filename, ".png"),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
  
  ggsave(
    filename = paste0(filename, ".pdf"),
    plot = plot,
    width = width,
    height = height,
    device = cairo_pdf,
    bg = "white"
  )
}

# Mood plot
mood_plot <- ggplot(mood_avg, aes(x = DaysFromTransplant, y = avg_mood, color = Group)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 1.5) +
  scale_color_manual(values = custom_colors) +
  labs(title = "Average Mood Over Time",
       x = "Days From Transplant",
       y = "Average Mood Score",
       color = "Group") +
  publication_theme

# Sleep classic plot
sleep_classic_plot <- ggplot(sleep_classic_avg, aes(x = DaysFromTransplant, y = value, color = Group)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 1.5) +
  facet_wrap(~ metric, scales = "free_y", 
             labeller = labeller(metric = c(avg_total_sleep_time = "Total Sleep Time (hours)",
                                            avg_time_in_bed = "Time in Bed (hours)",
                                            avg_sleep_efficiency = "Sleep Efficiency (%)"))) +
  scale_color_manual(values = custom_colors) +
  labs(title = "Average Daily Sleep Metrics Over Time",
       x = "Days From Transplant",
       y = "Value",
       color = "Group") +
  publication_theme

# Sleep stages plot
sleep_stages_plot <- ggplot(sleep_stages_avg, aes(x = DaysFromTransplant, y = hours, color = Group)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 1.5) +
  facet_wrap(~ stage, scales = "free_y", 
             labeller = labeller(stage = c(avg_light_sleep = "Light Sleep",
                                           avg_deep_sleep = "Deep Sleep",
                                           avg_rem_sleep = "REM Sleep"))) +
  scale_color_manual(values = custom_colors) +
  labs(title = "Average Daily Sleep Stages Over Time",
       x = "Days From Transplant",
       y = "Hours",
       color = "Group") +
  publication_theme

# Heart rate plot
hr_plot <- ggplot(hr_avg, aes(x = DaysFromTransplant, y = avg_hr, color = Group)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 1.5) +
  scale_color_manual(values = custom_colors) +
  labs(title = "Average Daily Heart Rate Over Time",
       x = "Days From Transplant",
       y = "Average Heart Rate (bpm)",
       color = "Group") +
  publication_theme

# Steps plot
steps_plot <- ggplot(steps_avg, aes(x = DaysFromTransplant, y = avg_steps, color = Group)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 1.5) +
  scale_color_manual(values = custom_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Daily Step Count Over Time",
       x = "Days From Transplant",
       y = "Average Steps",
       color = "Group") +
  publication_theme

# Activity plot
activity_plot <- ggplot(activity_avg, aes(x = DaysFromTransplant, y = avg_minutes, color = Group)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = TRUE, size = 1.5) +
  facet_wrap(~ activity_type, scales = "free_y", 
             labeller = labeller(activity_type = c(avg_sed = "Sedentary", 
                                                   avg_light = "Light Activity",
                                                   avg_moderate = "Moderate Activity",
                                                   avg_very = "Very Active"))) +
  scale_color_manual(values = custom_colors) +
  labs(title = "Average Daily Activity Minutes Over Time",
       x = "Days From Transplant",
       y = "Average Minutes",
       color = "Group") +
  publication_theme

# Modified heatmap function
create_publication_heatmap <- function(data, title) {
  ggplot(data, aes(x = DaysFromTransplant, y = reorder(factor(dyad_id), dyad_order), fill = reporting)) +
    geom_tile() +
    scale_fill_manual(values = c("Both" = "#1E90FF",
                                 "Patient Only" = "#00CED1",
                                 "Caregiver Only" = "#FF69B4",
                                 "Neither" = "#FFFFFF"),
                      name = "Reporting") +
    labs(title = title,
         x = "Days From Transplant",
         y = "Dyad ID (ordered by transplant date)") +
    publication_theme +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())
}

# Create heatmaps
mood_heatmap <- create_publication_heatmap(mood_heatmap_data, "Heatmap of Mood Reporting")
sleep_heatmap <- create_publication_heatmap(sleep_heatmap_data, "Heatmap of Sleep Reporting")

# Save all plots
save_plot(mood_plot, "mood_plot", width = 10, height = 8)
save_plot(sleep_classic_plot, "sleep_classic_plot", width = 12, height = 10)
save_plot(sleep_stages_plot, "sleep_stages_plot", width = 12, height = 10)
save_plot(hr_plot, "heart_rate_plot", width = 10, height = 8)
save_plot(steps_plot, "steps_plot", width = 10, height = 8)
save_plot(activity_plot, "activity_plot", width = 12, height = 10)
save_plot(mood_heatmap, "mood_heatmap", width = 12, height = 8)
save_plot(sleep_heatmap, "sleep_heatmap", width = 12, height = 8)