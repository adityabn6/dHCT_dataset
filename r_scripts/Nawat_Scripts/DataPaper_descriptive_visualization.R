library(readxl)
library(dplyr)
library(tidyr)
library(Cairo)
library(ggplot2)
library(ggtext)
library(patchwork)
library(stringr)
library(tools)
library(reshape2)
data_dir <- "/home/nawatsw/UMM/BMT/dataset/cleaned/DataPaper"
fig_dir <- "/home/nawatsw/UMM/BMT/figures/DataPaper"

timepoint_list <- c(
  "1", # 1
  "2", # 2
  "3", # 3
  "1", # 4
  "2", # 5
  "3"  # 6
)

prtcpt_list <- c(
  "Patients",   # 1
  "Patients",   # 2
  "Patients",   # 3
  "Caregivers", # 4
  "Caregivers", # 5
  "Caregivers"  # 6
)

PROMIS_score_dict <- c(
  "companionship" = "comp",
  "anger" = "anger",
  "emotional support" = "emosup",
  "global mental health" = "glohlth_mh",
  "global physical health" = "glohlth_ph",
  "informational support" = "infosup",
  "meaning and purpose" = "meanpur",
  "positive affect" = "posaff",
  "ability to participate in social roles and activities" = "abroact",
  "anxiety" = "anxty",
  "cognitive function" = "cogfun",
  "depression" = "deprss",
  "fatigue" = "fatig",
  "pain interference" = "paininf",
  "physical function" = "physfun",
  "sleep disturbance" = "slpdist",
  "satisfaction with social roles and activities" = "satroact",
  "sleep related impairment" = "slpimp",
  "social isolation" = "soiso",
  "self-efficacy for managing daily activities" = "managda",
  "self-efficacy for managing symptoms" = "managsm"
)
PROMIS_name_dict <- setNames(names(PROMIS_score_dict), PROMIS_score_dict)

custom_colors <- c("Patients" = "#00CED1", "Caregivers" = "#FF69B4")

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

############
# function #
############

save_png <- function(
  save_path, img,
  width = 10,
  height = 8,
  dpi = 300,
  units = "in",
  bg = "white"
) {
  CairoPNG(filename = save_path,
    width = width,
    height = height,
    dpi = dpi,
    units = units,
    bg = bg)
  show(img)
  dev.off()
}

##############
# read files #
##############

tscore_data <- read.csv(paste0(data_dir, "/PROMIS_tscore_censored.csv"))
outcome_data <- read.csv(paste0(data_dir, "/outcome_censored.csv"))
readmss_data <- read.csv(paste0(data_dir, "/readmissions_censored.csv"))
infect_data <- read.csv(paste0(data_dir, "/infections_censored.csv"))

########################
# create visualization #
########################


# PROMISE T score box plot

col <- "t_anxty"

col_to_show <- sub(".*t_", "", col)
tscore_data$Timestamp <- factor(tscore_data$Timestamp, levels = c("Baseline", "Day30", "Day120"))
tscore_data[[col]] <- as.numeric(as.character(tscore_data[[col]]))

img <- ggplot(tscore_data, aes(x = Timestamp, y = !!sym(col), color = Group)) +
  geom_boxplot(lwd = 0.5) +
  geom_jitter(alpha = 0.15) +
  stat_summary(
    color = "black",
    fun.data = function(x) data.frame(y = max(x) + 2, label = paste("n =", length(x))),
    geom = "text",
    vjust = 0
  ) +
  labs(
    title = toTitleCase(paste0(PROMIS_name_d[col_to_show], " T score")),
    x = "time", y = PROMIS_name_d[col_to_show]
  ) +
  publication_theme +
  facet_wrap(~ Group) + 
  scale_color_manual(values = custom_colors)

save_path <- paste0(fig_dir, "/overall_", col, ".png")
save_png(save_path, img)



# clinical out stacked bar plot

outcome_pivot_data <- outcome_data %>% 
  pivot_longer(
    cols = c("grfs_agvhd_3_4", "grfs_cgvhd", "grfs_relapse", "grfs_death"),
    names_to = "outcome_name",
    values_to = "outcome_flag"
  ) %>%
  select(STUDY_PRTCPT_ID, outcome_name, outcome_flag)

img <- ggplot(outcome_pivot_data, aes(x = outcome_name, fill = as.factor(outcome_flag))) +
  geom_bar(position = "fill") +  # "fill" makes the bars 100% stacked
  scale_y_continuous(labels = scales::percent_format()) +  # Show percentages on y-axis
  labs(x = "Clinical Outcome", y = "Percentage", fill = "Value", title = "Stacked Bar Plot of Clinical Outcome") +
  scale_x_discrete(labels = c("Acute GVHD", "Chronic GVHD", "Death", "Relapse")) +
  publication_theme +
  
  scale_fill_manual(values = c("0" = "#00CED1", "1" = "#FF69B4"))
  
save_path <- paste0(fig_dir, "/outcome_plot.png")
save_png(save_path, img)



# admission and infection histogram

sum_data <- readmss_data %>%
  group_by(STUDY_PRTCPT_ID) %>%
  summarize(count = max(number_admission)) %>%
  mutate(event = "number of readmissions") %>%
  select(STUDY_PRTCPT_ID, count, event) %>%
  rbind(
    infect_data %>%
      group_by(STUDY_PRTCPT_ID) %>%
      summarize(count = max(number_infection)) %>%
      mutate(event = "number of infections") %>%
      select(STUDY_PRTCPT_ID, count, event)
  )

img <- ggplot(sum_data, aes(x = count, fill = event)) +
  geom_histogram(position = position_dodge(width = 0.5), alpha = 0.8, bins = 30, binwidth = 0.5) +
  labs(x = "Number of Events", y = "Count of Patients", title = "Histogram of Readmission and Infection Occurrence") +
  publication_theme +
  scale_fill_manual(values = c("number of readmissions" = "#00CED1", "number of infections" = "#FF69B4")) +
  theme(
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = alpha('white', 0.5))
  )

save_path <- paste0(fig_dir, "/readmission_infection_plot.png")
save_png(save_path, img)



# PROMIS T score corr heatmap plot

tscore_for_corr_data <- tscore_data %>%
  select(
    t_comp, t_glohlth_mh, t_glohlth_ph, t_abroact, t_cogfun, t_physfun, t_managda, t_managsm, # positive metrices
    t_anxty, t_deprss, t_fatig, t_paininf, t_slpdist # negative metrices
  )

cor_matrix <- cor(tscore_for_corr_data, use = "complete.obs")
cor_melted <- reshape2::melt(cor_matrix)

custom_labels <- c(
  "companionship", "global mental health", "global physical health",
  "ability to participate in\nsocial roles and activities", "cognitive function",
  "physical function", "self-efficacy for\nmanaging daily activities",
  "self-efficacy for\nmanaging symptoms", "anxiety", "depression",
  "fatigue", "pain interference", "sleep disturbance"
)

img <- ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Create the tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                      midpoint = 0, limit = c(-1, 1), space = "Lab", 
                      name = "Correlation") +  # Set color gradient
  publication_theme +
  labs(x = NULL, y = NULL, title = "PROMIS T Score Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  scale_x_discrete(labels = custom_labels) +
  scale_y_discrete(labels = custom_labels) +
  coord_fixed()  # Ensures squares are maintained

save_path <- paste0(fig_dir, "/tscore_corr.png")
save_png(save_path, img)



# clinical event time series plot

merged_data <- outcome_data %>% 
  pivot_longer(
    cols = c("grfs_agvhd_date", "grfs_cgvhd_date", "grfs_relapse_date", "grfs_death_date"),
    names_to = "event",
    values_to = "date"
  ) %>%
  select(STUDY_PRTCPT_ID, event, date) %>%
  rbind(
    readmss_data %>% 
      pivot_longer(
        cols = c("date_admit", "date_discharge"),
        names_to = "event",
        values_to = "date",
        values_drop_na = TRUE
      ) %>%
      select(STUDY_PRTCPT_ID, event, date)
  ) %>%
  rbind(
    infect_data %>%
      rename(date = date_culture_drawn) %>%
      mutate(event = "date_culture_drawn") %>%
      select(STUDY_PRTCPT_ID, event, date)
  ) %>%
  drop_na() %>%
  filter(date >= 0) %>%

  group_by(STUDY_PRTCPT_ID) %>%
  mutate(last_event_date = max(date)) %>%
  ungroup() %>%
  mutate(STUDY_PRTCPT_ID = reorder(as.factor(STUDY_PRTCPT_ID), last_event_date))

img <- ggplot(merged_data, aes(x = date, y = STUDY_PRTCPT_ID, color = event, shape = event)) +
  geom_point(size = 3) +  # Points to represent events
  labs(x = "Days after Transplant", y = "Patient ID", title = "Clinical Event Plot") +
  theme_minimal() +
  scale_shape_manual(values = c(15, 8, 18, 3, 4, 17, 19)) +  # Custom shapes for events
  scale_color_manual(values = c("yellow", "orange", "green", "blue", "pink", "red", "purple")) +  # Custom colors
  publication_theme +
  theme(
    axis.text.y = element_blank(),  # Hides Y-axis values (IDs)
    axis.ticks.y = element_blank(),  # Hides Y-axis ticks
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate X-axis labels
    legend.position = c(0.95, 0.05),  # Position legend at bottom-right (x=0.95, y=0.05)
    legend.justification = c("right", "bottom"),  # Anchor legend to bottom-right corner
    legend.background = element_rect(fill = alpha('white', 0.7))  # Semi-transparent legend background
  )

save_path <- paste0(fig_dir, "/clinical_event_plot.png")
save_png(save_path, img, width = 8, height = 10)
