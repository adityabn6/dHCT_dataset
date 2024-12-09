library(dplyr)

# Function to check uniqueness and report duplicates
check_sleep_uniqueness <- function(sleep_data, data_name) {
  # Count occurrences of each combination
  duplicate_check <- sleep_data %>%
    group_by(STUDY_PRTCPT_ID, DaysFromTransplant) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(count > 1)
  
  if (nrow(duplicate_check) == 0) {
    cat(paste0("All rows in ", data_name, " are unique across STUDY_PRTCPT_ID and DaysFromTransplant.\n"))
  } else {
    cat(paste0("Found duplicates in ", data_name, ":\n"))
    print(duplicate_check)
    
    # Display the duplicate rows
    duplicates <- sleep_data %>%
      inner_join(duplicate_check, by = c("STUDY_PRTCPT_ID", "DaysFromTransplant"))
    print(duplicates)
  }
}

# Check uniqueness for classic sleep data
check_sleep_uniqueness(longest_sleep_classic, "Classic Sleep Data")

# Check uniqueness for stages sleep data
check_sleep_uniqueness(longest_sleep_stages, "Stages Sleep Data")