library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

DATA_DIR <- "/home/nawatsw/UMM/BMT/dataset"

# Read User-Device Map and Demographic & Clinical Information
device_ids <- read.csv(file.path(DATA_DIR, "raw/DEVICE_IDS.csv")) %>%
  mutate(
    PRTCPT_DVC_START_DT = as.POSIXct(PRTCPT_DVC_START_DT, format = "%d-%b-%y %I.%M.%OS %p", tz = "UTC"),
    PRTCPT_DVC_END_DT = as.POSIXct(PRTCPT_DVC_END_DT, format = "%d-%b-%y %I.%M.%OS %p", tz = "UTC")
  )

roadmap_ids <- read.csv(file.path(DATA_DIR, "raw/Roadmap 2.0 BMT_roadmapIDs.csv")) %>%
  mutate(
    pt_consent_date = as.POSIXct(pt_consent_date, format = "%m/%d/%y"),
    pt_day_120 = as.POSIXct(pt_day_120, format = "%m/%d/%y")
  )

bmt_demo_data <- read.csv(file.path(DATA_DIR, "raw/bmt_demo_data_new.csv")) %>%
  select(-time) %>%
  select(-1) %>%
  distinct() %>%
  rename(STUDY_PRTCPT_ID = roadmapID)

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
cat("Unique patients:", length(patient_ids), "\n")
cat("Unique caregivers:", length(caregiver_ids), "\n")
cat("Unique devices:", nrow(device_ids), "\n")



roadmap_ids_filtered <- roadmap_ids %>%
  filter(cohort != "Peds")

combined_ids <- bind_rows(
  roadmap_ids_filtered %>%
    left_join(device_ids, by = c("pt_rm_access_code" = "STUDY_PRTCPT_ID")) %>%
    mutate(STUDY_PRTCPT_ID = pt_rm_access_code),
  roadmap_ids_filtered %>%
    left_join(device_ids, by = c("cg_rm_access_code" = "STUDY_PRTCPT_ID")) %>%
    mutate(STUDY_PRTCPT_ID = cg_rm_access_code)
) %>%
  distinct()

# get psuedoID for patients and caregivers ID
participant_dict <- setNames(paste0("P", sprintf("%03d", seq_along(unique(combined_ids$STUDY_PRTCPT_ID)))), 
                              unique(combined_ids$STUDY_PRTCPT_ID))

device_dict <- setNames(paste0(participant_dict[combined_ids$STUDY_PRTCPT_ID], "_D", 
                                sprintf("%02d", sequence(rle(combined_ids$STUDY_PRTCPT_ID)$lengths))), 
                        combined_ids$PRTCPT_DVC_ID)

save(participant_dict, device_dict, file = file.path(DATA_DIR, "pseudoID_dict.RData"))

# combined_ids$pt_ID

# list(combined_ids = combined_ids, participant_dict = participant_dict, device_dict = device_dict)