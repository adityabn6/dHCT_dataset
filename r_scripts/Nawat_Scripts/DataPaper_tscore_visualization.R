library(readxl)
library(dplyr)
library(tidyr)
library(Cairo)
library(ggplot2)
library(ggtext)
library(patchwork)
library(stringr)
data_path <- "/home/nawatsw/UMM/BMT"

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

load(paste0(data_path, "/dataset/pseudoID_dict.RData"))

# PROMIS_subscore_dict <- c(
#   "global mental health" = "mh",
#   "global physical health" = "ph"
# )

############
# function #
############


gen_tscore_plot <- function(data, col, PROMIS_name_d=PROMIS_name_dict) {
  col_to_show <- sub(".*t_", "", col)
  data[[col]] <- as.numeric(as.character(data[[col]]))
  print(col_to_show)

  img <- ggplot(data, aes(x = timepoint, y = !!(sym(col))),) +
    geom_boxplot(notch = TRUE, lwd=0.5, col="blue") +
    geom_jitter(alpha = 0.15) +
    stat_summary(
      fun.data = function(x) data.frame(y = max(x) + 2, label = paste("n =", length(x))),
      geom = "text",
      vjust = 0,
    ) +
    labs(
      title = paste0("Roadmap BMT \n", PROMIS_name_d[col_to_show], " T score"),
      x = "time", y = PROMIS_name_d[col_to_show]
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    ) +
    facet_wrap(~ prtcpt_type)

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



save_png <- function(save_path, img) {
  CairoPNG(filename = save_path)
  show(img)
  dev.off()
}

##############
# read files #
##############

tscore_data <- read.csv(
  paste0(data_path, "/dataset/cleaned/Roadmap_BMT_rawtscore_long.csv")
) %>% mutate(
  timepoint = timepoint_list[timestamp],
  prtcpt_type = prtcpt_list[timestamp]
) %>% mutate(
  timestamp = as.character(timestamp)
)


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
  select(pt_rm_access_code, number_infection) %>%
  group_by(pt_rm_access_code) %>%
  summarize(number_infection = max(number_infection))

baseline_intv_data <- outcome_data %>%
  merge(readmss_data) %>%
  merge(infect_data) %>%
  merge(
    tscore_data %>% filter(timepoint == 1),
    by.x = "pt_rm_access_code",
    by.y = "roadmapid"
  )

#################################
# plot PROMISE T score box plot #
#################################

for (col in names(tscore_data)) {
  if (!grepl("t_", col) | col == "prtcpt_type") {
    next
  }
  img <- gen_tscore_plot(tscore_data, col)

  save_path <- paste0(data_path, "/figures/overall_", col, ".png")
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
    select(roadmapid, timepoint, prtcpt_type, !!(sym(col))) %>%
    group_by(roadmapid, prtcpt_type) %>%          # Group by the column of interest
    filter(n() == 3) %>%            # Keep only groups with exactly 4 occurrences
    ungroup()

  img <- gen_tscore_plot(tscore_data_filtered, col)

  save_path <- paste0(data_path, "/figures/overall_filtered_", col, ".png")
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
  save_path <- paste0(data_path, "/figures/baseline_intv_", promis_col_to_show, ".png")
  save_png(save_path, combined_plot)
  # break
}




# intv_col <- "cg_arm"
# col <- "t_fatig"
# event_col <- "grfs_agvhd_3_4"

# col_to_show <- sub(".*t_", "", col)

# baseline_intv_data <- read.csv(
#   paste0(data_path, "/dataset/Roadmap BMT Clinical Data_Outcome.csv") # target data
# ) %>%
# mutate(
#   cg_arm = as.factor(cg_arm)
# ) %>%
# merge(
#   tscore_data %>% filter(timepoint == 1),
#   by.x = "pt_rm_access_code",
#   by.y = "roadmapid"
# )
# baseline_intv_data[[col]] <- as.numeric(as.character(baseline_intv_data[[col]]))
# baseline_intv_data[[event_col]] <- as.factor(baseline_intv_data[[event_col]])
# baseline_intv_data[[intv_col]] <- as.factor(baseline_intv_data[[intv_col]])
# baseline_intv_data <- baseline_intv_data %>% filter(!!(sym(event_col)) != 'NA')
# print(col_to_show)

# img <- ggplot(baseline_intv_data, aes(x = !!(sym(intv_col)), y = !!(sym(col)))) +
#   geom_boxplot(notch = TRUE, lwd=0.5, col="blue") +
#   geom_jitter(alpha = 0.35) +
#   stat_summary(
#     fun.data = function(x) data.frame(y = max(x) + 2, label = paste("n =", length(x))),
#     geom = "text",
#     vjust = 0,
#   ) +
#   labs(
#     x = "Intervention Flag", y = PROMIS_name_dict[col_to_show]
#   ) +
#   theme_bw() +
#   facet_wrap(~ grfs_agvhd_3_4, labeller = "label_both")

# save_path <- paste0(data_path, "/figures/baseline_intv_", col, ".png")
# save_png(save_path, img)


# img <- ggplot(baseline_intv_data, aes(x = !!(sym(event_col)), y = !!(sym(col)))) +
#   geom_boxplot(notch = TRUE, lwd=0.5, col="blue") +
#   geom_jitter(alpha = 0.35, aes(color=!!(sym(intv_col)))) +
#   stat_summary(
#     fun.data = function(x) data.frame(y = max(x) + 2, label = paste("n =", length(x))),
#     geom = "text",
#     vjust = 0,
#   ) +
#   labs(
#     title = paste0("Roadmap BMT \n", PROMIS_name_dict[col_to_show], " T score"),
#     x = event_col, y = PROMIS_name_dict[col_to_show]
#   ) +
#   theme_bw()

# # save_path <- paste0(data_path, "/figures/baseline_intv_", promis_col, ".png")
# save_png(save_path, img)


# combined_plot <- (img | img) / (img | img) + 
#   plot_annotation(title = 'The surprising story about mtcars')
# save_png(save_path, combined_plot)
