library(readxl)
library(dplyr)
library(tidyr)
library(Cairo)
library(ggplot2)
library(ggtext)
library(patchwork)
library(stringr)
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

# load(paste0(data_path, "/dataset/pseudoID_dict.RData"))

# PROMIS_subscore_dict <- c(
#   "global mental health" = "mh",
#   "global physical health" = "ph"
# )

############
# function #
############


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

gen_tscore_plot <- function(data, col, PROMIS_name_d=PROMIS_name_dict) {
  col_to_show <- sub(".*t_", "", col)
  data[[col]] <- as.numeric(as.character(data[[col]]))
  print(col_to_show)

  img <- ggplot(data, aes(x = Timestamp, y = !!sym(col), colour = Group)) +
    geom_boxplot(lwd=0.5) +
    geom_jitter(alpha = 0.15) +
    stat_summary(
      fun.data = function(x) data.frame(y = max(x) + 2, label = paste("n =", length(x))),
      geom = "text",
      vjust = 0,
      colour = "black"
    ) +
    labs(
      title = paste0(str_to_title(PROMIS_name_d[col_to_show]), " T score"),
      x = "time", y = PROMIS_name_d[col_to_show]
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    ) +
    publication_theme +
    facet_wrap(~ Group) +
    scale_colour_manual(values = c("Patients" = "#00CED1", "Caregivers" = "#FF69B4"))
  
  return(img)
}

gen_baseline_event_plot <- function(
  data, promis_col, intv_col, event_col, PROMIS_name_d=PROMIS_name_dict
) {
  temp_data <- data %>%
    filter(!!sym(event_col) != 'NA') %>%
    mutate(
      !!sym(promis_col) := as.numeric(as.character(!!sym(promis_col))),
      !!sym(event_col) := as.factor(!!sym(event_col)),
      !!sym(intv_col) := as.factor(!!sym(intv_col)),
      facet_col =  as.factor(!!sym(event_col))
    )

  img <- ggplot(temp_data, aes(x = !!sym(intv_col), y = !!sym(promis_col))) +
    geom_boxplot(notch = FALSE, lwd=0.5, col="blue") +
    geom_jitter(alpha = 0.35) +
    stat_summary(
      fun.data = function(x) data.frame(y = max(x) + 2, label = paste("n =", length(x))),
      geom = "text",
      vjust = +0.4,
    ) +
    labs(
      x = "Intervention Flag", y = NULL
    ) +
    theme_bw() +
    facet_wrap(vars(!!sym(event_col)), labeller = label_both)

  return(img)
}

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

tscore_data <- read.csv(paste0(data_dir, "/PROMIS_tscore_censored.csv")) %>%
  mutate(Timestamp = ifelse(Timestamp == "Day30", "Day030", Timestamp))

outcome_data <- read.csv(paste0(data_dir, "/outcome_censored.csv"))

readmss_data <- read.csv(
  paste0(data_dir, "/readmissions_censored.csv")
) %>%
  select(STUDY_PRTCPT_ID, number_admission, days_in_hospital) %>%
  group_by(STUDY_PRTCPT_ID) %>%
  summarize(days_in_hospital = sum(days_in_hospital), number_admission = max(number_admission)) %>%
  mutate(days_in_hospital = ifelse(is.na(days_in_hospital), 0, days_in_hospital))

infect_data <- read.csv(
  paste0(data_dir, "/infections_censored.csv")
) %>%
  select(STUDY_PRTCPT_ID, number_infection) %>%
  group_by(STUDY_PRTCPT_ID) %>%
  summarize(number_infection = max(number_infection))


outcome_data <- read.csv(
  paste0(data_path, "/dataset/cleaned/Roadmap BMT Clinical Data_Outcome_cleaned.csv")
)

readmss_data <- read.csv(
  paste0(data_path, "/dataset/cleaned/Roadmap BMT Clinical Data_Readmissions_cleaned.csv")
) %>%
  select(pt_rm_access_code, number_admission, days_in_hospital) %>%
  group_by(pt_rm_access_code) %>%
  summarize(days_in_hospital = sum(days_in_hospital), number_admission = max(number_admission)) %>%
  mutate(days_in_hospital = ifelse(is.na(days_in_hospital), 0, days_in_hospital))

infect_data <- read.csv(
  paste0(data_path, "/dataset/cleaned/Roadmap BMT Clinical Data_Infections_cleaned.csv")
) %>%
  select(STUDY_PRTCPT_ID, number_infection) %>%
  group_by(pt_rm_access_code) %>%
  summarize(number_infection = max(number_infection))

baseline_intv_data <- outcome_data %>%
  merge(readmss_data, all = TRUE) %>%
  merge(infect_data, all = TRUE) %>%
  merge(tscore_data %>% filter(Timestamp == "Baseline" & Group == "Patients"), all = TRUE)

#################################
# plot PROMISE T score box plot #
#################################

for (col in names(tscore_data)) {
  if (!grepl("t_", col) | col == "prtcpt_type") {
    next
  }
  img <- gen_tscore_plot(tscore_data, col)

  save_path <- paste0(fig_dir, "/overall_", col, ".png")
  save_png(save_path, img)
  # break
}

# filter only participants that answer all 3 surveys (Baseline, Day30, Day120)

for (col in names(tscore_data)) {
  if (!grepl("t_", col) | col == "prtcpt_type") {
    next
  }

  tscore_data_filtered <- tscore_data %>%
    filter(!is.na(!!(sym(col)))) %>%
    select(STUDY_PRTCPT_ID, Timestamp, Group, !!(sym(col))) %>%
    group_by(STUDY_PRTCPT_ID, Group) %>%          # Group by the column of interest
    filter(n() == 3) %>%            # Keep only groups with exactly 3 occurrences
    ungroup()

  img <- gen_tscore_plot(tscore_data_filtered, col)

  save_path <- paste0(fig_dir, "/overall_filtered_", col, ".png")
  save_png(save_path, img)
  # break
}


# baseline vs intervention flag vs event flag visualization

intv_col <- "cg_arm"
event_col_list <- c("grfs_agvhd_3_4", "grfs_cgvhd", "grfs_relapse", "grfs_death")
for (promis_col in names(baseline_intv_data)) {
  if (!grepl("t_", promis_col) | promis_col %in% c("prtcpt_type", "pt_rm_access_code")) {
    next
  }

  promis_col_to_show <- sub(".*t_", "", promis_col)
  print(promis_col_to_show)

  i <- 1
  img_list <- list()
  for (event_col in event_col_list) {
    img_list[[i]] <- gen_baseline_event_plot(baseline_intv_data, promis_col, intv_col, event_col)
    i <- i+1
  }

  combined_plot <- (img_list[[1]] | img_list[[2]]) / (img_list[[3]] | img_list[[4]]) + 
    plot_annotation(
      title = paste0(
        "Baseline T score vs outcome variables\n", PROMIS_name_dict[promis_col_to_show]
      )
    )
  save_path <- paste0(fig_dir, "/baseline_intv_", promis_col_to_show, ".png")
  save_png(save_path, combined_plot)
  # break
}
