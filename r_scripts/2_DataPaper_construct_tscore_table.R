library(readxl)
library(dplyr)
library(tidyr)
path <- "/home/nawatsw/UMM/BMT/dataset"
save_folder <- file.path(path, "cleaned/DataPaper")

Assmnt_dict <- list(
  "Patients" = list(
    "Baseline" = 1,
    "Day30" = 2,
    "Day120" = 3
  ),
  "Caregivers" = list(
    "Baseline" = 4,
    "Day30" = 5,
    "Day120" = 6
  )
)

timestamp_assmnt_dict <- list(
    "1" = "Baseline",
    "2" = "Day30",
    "3" = "Day120",
    "4" = "Baseline",
    "5" = "Day30",
    "6" = "Day120"
)

group_assmnt_dict <- list(
    "1" = "Patients",
    "2" = "Patients",
    "3" = "Patients",
    "4" = "Caregivers",
    "5" = "Caregivers",
    "6" = "Caregivers"
)

PROMIS_score_dict <- c(
  "companionship" = "comp",
  "anger" = "anger",
  "emotional support" = "emosup",
  "global health" = "glohlth",
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

PROMIS_subscore_dict <- c(
  "global mental health" = "mh",
  "global physical health" = "ph"
)

PIN_name <- "STUDY_PRTCPT_ID"

##################
#### function ####
##################

clean_string_columns <- function(df) {
  df[] <- lapply(df, function(column) {
    # Check if the column is character (string)
    if (is.character(column)) {
      # Apply trimws() and tolower() to the column
      return(trimws(column))
    } else {
      # Return the column unchanged if it's not character
      return(column)
    }
  })
  return(df)
}

merge_t_table <- function(
  t_df, PROMIS_score,
  PROMIS_score_d=PROMIS_score_dict,
  PROMIS_subscore_d=PROMIS_subscore_dict,
  PIN_n=PIN_name
) {
  combined_df <- data.frame()

  score_short <- PROMIS_score_d[PROMIS_score]
  # some PROMIS score have multiple sub-score
  inst_list <- unique(t_df$Inst)
  for (inst in inst_list) {
    if (length(inst_list) == 1) {
      names_prefix <- paste0(score_short)
    } else {
    inst_short <- tolower(sub(".*-\\s*", "", inst))
      subscore_short <- PROMIS_subscore_d[inst_short]
      names_prefix <- paste0(score_short, "_", subscore_short)
    }

    df_temp <- t_df %>%
      filter(Inst == inst) %>%
      mutate(
        Timestamp = timestamp_assmnt_dict[Assmnt],
        Group = group_assmnt_dict[Assmnt],
      ) %>%
      select(PIN, Timestamp, Group, TScore) %>%
      rename_with(~paste0("t_", names_prefix), TScore) %>%
      rename(!!(sym(PIN_n)) := PIN)

    df_temp <- as.data.frame(lapply(df_temp, unlist))

    if (nrow(combined_df) == 0) {
      combined_df <- df_temp # first iteration
    } else {
      combined_df <- merge(combined_df, df_temp, all = TRUE)
    }
    # print(nrow(combined_df))
  }
  return(combined_df)
}


#################################################
# create raw score table in PROMIS table format #
#################################################

# get all PROMIS metrices
data_dict <- read_excel(paste0(path, "/RM BMT - PROMIS Measure Codebook.xlsx")) %>%
  filter(
    is.na(Notes) | 
    Notes != "This question is a part of the PROMIS Pediatric Profile v2.0 - Profile 25 (Ages 8-17)"
  ) %>%
  mutate(`Variable name` = ifelse(grepl("Actual code", Notes), gsub(".*: ", "", Notes), `Variable name`))

PIN_col <- "roadmapid"
PROMIS_all <- data_dict$`PROMIS Measure Name`[!duplicated(data_dict$`PROMIS Measure Name`)]

for (PROMIS in PROMIS_all) {
  if (grepl("Pediatric", PROMIS)) {
    break
  }
  print(PROMIS)

  # get question codes of each PROMIS metric
  var_names <- data_dict %>%
    filter(data_dict$`PROMIS Measure Name` == PROMIS) %>%
    select(`Variable name`) %>%
    as.matrix() %>%
    tolower()
  var_count <- length(var_names)


  combined_data <- data.frame()
  for (prtcpt_type in c("Patients", "Caregivers")) {
    for (time_point in c("Baseline", "Day30", "Day120")) {
      print(c("   ", prtcpt_type, time_point))
      # get data for each PROMIS metric and each data type
      data_path <- paste0(
        save_folder, "/Roadmap_2_", prtcpt_type, "_", time_point, "_cleaned.csv"
      )

      data <- read.csv(data_path)
      
      available_cols <- var_names %in% colnames(data)
      
      data <- data %>%
        mutate(
          Assmnt = Assmnt_dict[[prtcpt_type]][[time_point]],
          !!(sym(PIN_col)) := trimws(!!(sym(PIN_col)))
        ) %>%
        select(all_of(c(PIN_col, "Assmnt", var_names))) %>%
        rename(PIN = !!(sym(PIN_col)))

      
      print(c("Unique id count", length(unique(data$PIN))))
      print(c("All rows count", nrow(data)))
      combined_data <- rbind(combined_data, data)
      
    }
  }

  PROMIS_to_save <- gsub(":", "", PROMIS)
  PROMIS_to_save <- gsub("[.]", "_", PROMIS_to_save)
  write.csv(
    combined_data,
    paste0(
      save_folder, "/PROMIS raw score/Roadmap_BMT_",
      PROMIS_to_save, " ", var_count, "a.csv"
    ),
    row.names = FALSE, na = "", quote = FALSE
  )
}

#################################

# Submit saved files to the url link to get t score:
# https://www.assessmentcenter.net/ac_scoringservice

#############################################################
# create raw and t score table from all PROMIS score tables #
#############################################################

tscore_folder_path <- paste0(save_folder, "/PROMIS t score")
tfile_list <- list.files(path = tscore_folder_path, full.names = TRUE)

combine_data <- data.frame()
for (PROMIS_score in names(PROMIS_score_dict)) {
  print(PROMIS_score)

  # choose file to read
  tfile_idx <- grep(PROMIS_score, tolower(tfile_list))
  tscore_data_path <- tfile_list[tfile_idx]

  if (length(tfile_idx) == 0) {
    print("   pass")
    next
  }

  # read files
  tscore_data <- read.csv(tscore_data_path) %>%
    {colnames(.) <- .[4, ]; .} %>%  # Set the column names to the 4th row
    slice(5:n()) %>%  # Keep rows from the 5th row onward
    clean_string_columns()

  # create score table
  temp_data <- merge_t_table(
    tscore_data,
    PROMIS_score
  )
  print(table(temp_data$Timestamp))
  print(table(temp_data$Group))

  if (nrow(combine_data) == 0) {
    combine_data <- temp_data # first iteration
  } else {
    combine_data <- merge(combine_data, temp_data, all=TRUE)
  }
}

View(combine_data)
write.csv(combine_data,
  paste0(save_folder, "/PROMIS_tscore_censored.csv"),
  row.names = FALSE, na = ""
)