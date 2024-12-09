# 2_Read_Data.R

# Load required libraries
library(data.table)
library(dplyr)

# Define file paths
DATA_DIR <- "/home/adityabn/ChoiLab/Roadmap BMT/Data/"
CSV_DIR <- file.path(DATA_DIR, "CSVs")
ID_DIR <- file.path(DATA_DIR, "IDs")

#' Read CSV file with error handling
#'
#' @param file_path Path to the CSV file
#' @param file_name Name of the file (for error reporting)
#' @return Data table containing the CSV data
read_csv_safe <- function(file_path, file_name) {
  tryCatch(
    fread(file_path),
    error = function(e) {
      stop(paste("Error reading", file_name, ":", e$message))
    }
  )
}

# Read main CSVs
mood <- read_csv_safe(file.path(CSV_DIR, "bmt_MOOD_TIME_ENTRY.csv"), "Mood data")
hr <- read_csv_safe(file.path(CSV_DIR, "HR_Data_BMT.csv"), "Heart rate data")
sleep <- read_csv_safe(file.path(CSV_DIR, "Sleep_Data_BMT.csv"), "Sleep data")
steps <- read_csv_safe(file.path(CSV_DIR, "Step_Data_BMT.csv"), "Step data")

 # Read User-Device Map and Demographic & Clinical Information
device_ids <- read_csv_safe(file.path(ID_DIR, "DEVICE_IDS.csv"), "Device IDs")
roadmap_ids <- read_csv_safe(file.path(ID_DIR, "Roadmap 2.0 BMT_roadmapIDs_10.11.2023.csv"), "Roadmap IDs")
bmt_demo_data <- read_csv_safe(file.path(ID_DIR, "bmt_demo_data_new.csv"), "BMT demographic data")

# Extract Patient and Caregiver IDs
patient_ids <- roadmap_ids %>% 
  filter(cohort != "Peds") %>% 
  pull(pt_rm_access_code)

caregiver_ids <- roadmap_ids %>% 
  filter(cohort != "Peds") %>% 
  pull(cg_rm_access_code)

# Filter User-Device Map based on Patients and Caregivers
device_ids <- device_ids %>%
  filter(STUDY_PRTCPT_ID %in% c(patient_ids, caregiver_ids))

# Extract Patient and Caregiver Device IDs
patient_device_ids <- device_ids %>% 
  filter(STUDY_PRTCPT_ID %in% patient_ids) %>% 
  pull(PRTCPT_DVC_ID)

caregiver_device_ids <- device_ids %>% 
  filter(STUDY_PRTCPT_ID %in% caregiver_ids) %>% 
  pull(PRTCPT_DVC_ID)

# Print summary of loaded data
cat("Data loading summary:\n")
cat("Mood entries:", nrow(mood), "\n")
cat("Heart rate entries:", nrow(hr), "\n")
cat("Sleep entries:", nrow(sleep), "\n")
cat("Step entries:", nrow(steps), "\n")
cat("Unique patients:", length(patient_ids), "\n")
cat("Unique caregivers:", length(caregiver_ids), "\n")
cat("Unique devices:", nrow(device_ids), "\n")

