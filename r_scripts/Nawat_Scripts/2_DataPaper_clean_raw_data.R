library(ggplot2)
library(Cairo)
library(dplyr)
library(tidyr)

# Define constant variables

path <- "/home/nawatsw/UMM/BMT/dataset"

time_list <- list("Baseline" = 1, "Day30" = 2, "Day120" = 3)
cols_range <- list(
  "Patients" = list(
    list(start = "Global01", end = "DRS_pt_10"),
    list(start = "PSOC200", end = "DRS_pt_10"),
    list(start = "SOC200", end = "DRS_pt_10")
  ),
  "Caregivers" = list(
    list(start = "SOC200", end = "DRS_cg_11"),
    list(start = "SOC200", end = "DRS_cg_11"),
    list(start = "TBICQ_CW11r", end = "DRS_cg_11")
  )
)

duplicated_cols <- list(
  c("Global01", "Global01.1"),
  c("Global02", "Global02.1"),
  c("Global03", "Global03.1"),
  c("Global04", "Global04.1"),
  c("Global05", "Global05.1"),
  c("Global09r", "Global09r.1"),
  c("Global06", "Global06.1")
)

reverse_code_list <- list(
  "Sleep116" = function(x) { return(ifelse(is.na(x), NA, 6 - x)) },
  "PS_4" = function(x) { return(ifelse(is.na(x), NA, 6 - x)) },
  "PS_5" = function(x) { return(ifelse(is.na(x), NA, 6 - x)) },
  "PS_7" = function(x) { return(ifelse(is.na(x), NA, 6 - x)) },
  "PS_8" = function(x) { return(ifelse(is.na(x), NA, 6 - x)) },

  "LOTR_3" = function(x) { return(ifelse(is.na(x), NA, 4 - x)) },
  "LOTR_7" = function(x) { return(ifelse(is.na(x), NA, 4 - x)) },
  "LOTR_9" = function(x) { return(ifelse(is.na(x), NA, 4 - x)) },

  "DRS_cg_3" = function(x) { return(ifelse(is.na(x), NA, 3 - x)) },
  "DRS_cg_4" = function(x) { return(ifelse(is.na(x), NA, 3 - x)) },
  "DRS_cg_5" = function(x) { return(ifelse(is.na(x), NA, 3 - x)) },
  "DRS_cg_8" = function(x) { return(ifelse(is.na(x), NA, 3 - x)) },
  "DRS_cg_11" = function(x) { return(ifelse(is.na(x), NA, 3 - x)) },

  "DRS_pt_3" = function(x) { return(ifelse(is.na(x), NA, 3 - x)) },
  "DRS_pt_4" = function(x) { return(ifelse(is.na(x), NA, 3 - x)) },
  "DRS_pt_5" = function(x) { return(ifelse(is.na(x), NA, 3 - x)) },
  "DRS_pt_8" = function(x) { return(ifelse(is.na(x), NA, 3 - x)) }
)


load(paste0(path, "/pseudoID_dict.RData"))

# function

clean_data <- function(prtcpt_type, time_point) {

  data_path <- paste0(path, "/raw/Roadmap_2_", prtcpt_type, "_", time_point, ".csv")

  # data <- read.csv(data_path, na.strings = "", check.names = FALSE)
  data <- read.csv(data_path, na.strings = "")

  # get the start and end columns for PROMIS-related questions
  start_col_name <- cols_range[[prtcpt_type]][[time_list[[time_point]]]]$start
  end_col_name <- cols_range[[prtcpt_type]][[time_list[[time_point]]]]$end

  start_col_idx <- which(colnames(data) == start_col_name)
  end_col_idx <- which(colnames(data) == end_col_name)

  print(data_path)
  print(c("prtcpt_type: ", prtcpt_type))
  print(c("time_point: ", time_point))
  print(c("start_col_name: ", start_col_name))
  print(c("end_col_name: ", end_col_name))
  # print(names(data))
  data <- data %>%
    filter(`Finished` == 1) %>%
    distinct() %>%
    mutate(
      `Duration..in.seconds.` = as.double(`Duration..in.seconds.`), 
      EndDate = as.POSIXct(EndDate, format = "%Y-%m-%d %H:%M:%S"),
      roadmapID = trimws(roadmapID)
    ) %>%
    filter(
      `Duration..in.seconds.` > 150,
      nchar(roadmapID) == 8
    ) %>%
    group_by(roadmapID) %>%
    filter(EndDate == max(EndDate)) %>%
    ungroup() %>%
    mutate(across(all_of(colnames(data)[start_col_idx:(end_col_idx)]), as.numeric)) %>%
    mutate(roadmapID = participant_dict[roadmapID]) %>%
    filter(!is.na(roadmapID)) %>%
    select(
      -c(
        StartDate, EndDate, IPAddress, RecordedDate, ResponseId, 
        RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
        LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage
      )
    )

  # print(names(data))
  print(c("Unique id count", length(unique(data$roadmapID))))
  print(c("All rows count", nrow(data)))

  # collapse duplicated columns
  for (dup_cols in duplicated_cols) {
    col1 <- dup_cols[1]
    col2 <- dup_cols[2]
    
    data <- data %>%
      mutate(!!sym(col1) := coalesce(!!sym(col1), !!sym(col2))) %>%
      select(-col2)
  }

  # calculate reverse codings
  for (rev_col in names(reverse_code_list)){
    if (rev_col %in% names(data)) {
      data <- data %>%
        mutate(!!sym(rev_col) := reverse_code_list[[rev_col]](!!sym(rev_col)))
    }
  }

  # lower all column names
  names(data) <- tolower(names(data))

  return (data)
}


#####################
# clean survey data #
#####################

# apply function for all survey datasets
for (prtcpt_type in c("Patients", "Caregivers")) {
  for (time_point in c("Baseline", "Day30", "Day120")) {
    
    data <- clean_data(prtcpt_type, time_point)
    View(data)
  
    save_path <- paste0(path, "/cleaned/Roadmap_2_", prtcpt_type, "_", time_point, "_cleaned.csv")
    write.csv(data, save_path)
    print(save_path)
  }
}



#####################
# clean target data #
#####################

# get transplant date to set zero date
trnsplnt_data <- read.csv(file.path(path, "raw/Roadmap 2.0 BMT_roadmapIDs.csv")) %>%
  mutate(pt_rm_access_code = participant_dict[pt_rm_access_code]) %>%
  select(pt_rm_access_code, pt_transplant_date) %>%
  mutate(pt_transplant_date = as.POSIXct(pt_transplant_date, format = "%m/%d/%y")) %>%
  drop_na()

target_data_file_list <- c(
  "Roadmap BMT Clinical Data_Infections.csv", 
  "Roadmap BMT Clinical Data_Readmissions.csv", 
  "Roadmap BMT Clinical Data_Outcome.csv"
)

for (file_name in target_data_file_list) {
  
  target_data <- read.csv(file.path(path, "raw", file_name)) %>%
    mutate(
      pt_rm_access_code = participant_dict[pt_rm_access_code]
    ) %>%
    merge(trnsplnt_data)

  for (date_col in names(target_data)) {
    if (grepl("date", date_col) & date_col != "pt_transplant_date") {
      print(date_col)

      # if (file_name == "Roadmap BMT Clinical Data_Outcome.csv") {
      #   target_data <- target_data %>%
      #     mutate(!!(sym(date_col)) := as.POSIXct(!!(sym(date_col)), format = "%m/%d/%Y"))
      # } else {
      #   target_data <- target_data %>%
      #     mutate(!!(sym(date_col)) := as.POSIXct(!!(sym(date_col)), format = "%m/%d/%y"))
      # }

      target_data <- target_data %>%
        mutate(!!(sym(date_col)) := as.POSIXct(!!(sym(date_col)), format = "%m/%d/%Y"))

      target_data <- target_data %>%
        mutate(
          !!(sym(date_col)) := as.integer(difftime(!!(sym(date_col)), pt_transplant_date, units = "days"))
        )
    }
  }

  target_data <- target_data %>% select(-pt_transplant_date, -record_id) %>% rename(STUDY_PRTCPT_ID = pt_rm_access_code)
  save_path <- paste0(path, "/cleaned/DataPaper/", tolower(sub("^[^_]*_", "", substr(file_name, 1, nchar(file_name)-4))), "_censored.csv")
  write.csv(target_data, save_path, row.names = FALSE, na = "")
  print(save_path)
  View(target_data)
}




