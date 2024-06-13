# Load required libraries
library(dplyr)
library(here)

# Get maternal health trials and add trial_type column
maternal_health_trials <- read.csv(here::here("data", "clinical-trial-registry", "maternal-health-trials.csv")) %>%
  mutate(trial_type = "maternal health")

# Get tuberculosis trials and add trial_type column
tuberculosis_trials <- read.csv(here::here("data", "clinical-trial-registry", "tuberculosis-trials.csv")) %>%
  mutate(trial_type = "tuberculosis")

# Combine both trials into one dataframe
ct_trials <- bind_rows(maternal_health_trials, tuberculosis_trials)

# Select 10 random trials for pilot check
set.seed(123) 
publication_search_pilot <- 
  ct_trials |>
  sample_n(10)

# Save the dataset of 10 random trials
#write.csv(publication_search_pilot, here::here("data", "clinical-trial-registry", "publication_search_pilot.csv"), row.names = FALSE)

# Print the selected random trials (optional)
print(random_trials)
