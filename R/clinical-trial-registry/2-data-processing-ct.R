# PRODIGY: An exploratory analysis of transparency and patient engagement in Global Health Research

# This script prepares final clinical trial registry sample for analysis
# This code is adapted from Nordic trial reporting project (https://github.com/cathrineaxfors/nordic-trial-reporting/tree/main) 


# Load dependencies ---
library(dplyr)
library(lubridate)

# get clinical trial registry sample----------

# Get maternal health trials and add trial_type column
maternal_health_trials <- read.csv(here::here("data", "clinical-trial-registry", "17-6-2024-maternal-health-trials.csv")) %>%
  mutate(trial_type = "maternal health")

# Get tuberculosis trials and add trial_type column
tuberculosis_trials <- read.csv(here::here("data", "clinical-trial-registry", "17-6-2024-tuberculosis-trials.csv")) %>%
  mutate(trial_type = "tuberculosis")

# Combine both trials into one dataframe
ct_all <- bind_rows(maternal_health_trials, tuberculosis_trials)

# Save the dataset of random trials
#write.csv(ct_all, here::here("data", "clinical-trial-registry", "17-6-2024-publication_search_final.csv"), row.names = FALSE)

# Sanity checks

# Check for missing values in 'region' and 'country' columns
missing_region <- sum(is.na(ct_all$region))
missing_country <- sum(is.na(ct_all$country))


if (missing_region > 0) {
  cat("There are", missing_region, "missing values in the 'region' column.\n")
} else {
  cat("There are no missing values in the 'region' column.\n")
}

if (missing_country > 0) {
  cat("There are", missing_country, "missing values in the 'country' column.\n")
} else {
  cat("There are no missing values in the 'country' column.\n")
}


# Check for duplicate 'NCT ID' values
duplicate_nct_id <- 
  ct_all |>
  group_by(nct_id) |>
  filter(n() > 1) |>
  summarise(count = n())

if (nrow(duplicate_nct_id) > 0) {
  cat("There are duplicate NCT IDs:\n")
  print(duplicate_nct_id)
} else {
  cat("There are no duplicate NCT IDs.\n")
}


# Included: Interventional studies, Multi-centric local and Monocentric
# Study status: “Completed”, “Terminated”, “Suspended” or “Unknown”
table(ct_all$study_type)
table(ct_all$category)
table(ct_all$recruitment_status)


# add reconciled publication searches ------------------------

# Get reconciled publication searches
reconciliation_ps_final_raw <- 
  read.csv(here::here("data", "clinical-trial-registry", "publication_search", "reconciliation_check_final_part-1.csv"), check.names = FALSE) 

ct_all <- 
  reconciliation_ps_final_raw |>
  left_join(ct_all,by = "nct_id")|>
  select(nct_id, registration_date, summary_results_date, study_type, 
         phase, enrollment, recruitment_status, official_title, title, 
         start_date, completion_date, primary_completion_date, has_summary_results,
         allocation, masking, main_sponsor, is_multicentric, category, country, region, trial_type,
         `final match`, `final any paper`,`final publication found`, 
         `final PMID`, `DOI (format: e.g 10.1000/182)`, 
         `URL  (if not avaiable DOI or PMID )`, 
         `earliest publication date  (DD/MM/YYYY)`) |>
          rename(publication_date = `earliest publication date  (DD/MM/YYYY)`)

# covert publication date to ymd format
ct_all$publication_date <- ymd(dmy(ct_all$publication_date)) 

# add identifier if a trial has any publication result
ct_all <- mutate(ct_all, has_publication = case_when(
  `final any paper` == "article" ~ TRUE,   # If 'final any paper' is "article", set TRUE
  `final any paper` == "no paper" ~ FALSE, # If 'final any paper' is "no paper", set FALSE
))


# Calculate time-to variables ----


#days_cd_to_publication
ct_all <- mutate(ct_all, days_cd_to_publication = 
                   as.integer(ymd(publication_date)-ymd(completion_date)))
summary(ct_all$days_cd_to_publication)



#days_pcd_to_publication
ct_all <- mutate(ct_all, days_pcd_to_publication = 
                   as.integer(ymd(publication_date)-ymd(primary_completion_date)))
summary(ct_all$days_pcd_to_publication)


#days_cd_to_summary
ct_all <- mutate(ct_all, days_cd_to_summary = 
                   as.integer(ymd(summary_results_date)-ymd(completion_date)))
summary(ct_all$days_cd_to_summary)


#days_pcd_to_summary
ct_all <- mutate(ct_all, days_pcd_to_summary = 
                   as.integer(ymd(summary_results_date)-ymd(primary_completion_date)))
summary(ct_all$days_pcd_to_summary)


#days_reg_to_start
ct_all <- mutate(ct_all, days_reg_to_start = 
                   as.integer(ymd(start_date)-ymd(registration_date)))
summary(ct_all$days_reg_to_start)


#days_reg_to_cd
ct_all <- mutate(ct_all, days_reg_to_cd = 
                   as.integer(ymd(completion_date)-ymd(registration_date)))
summary(ct_all$days_reg_to_cd)


#days_reg_to_pcd
ct_all <- mutate(ct_all, days_reg_to_pcd = 
                   as.integer(ymd(primary_completion_date)-ymd(registration_date)))
summary(ct_all$days_reg_to_pcd)


#days_reg_to_publication
ct_all <- mutate(ct_all, days_reg_to_publication = 
                   as.integer(ymd(publication_date)-ymd(registration_date)))
summary(ct_all$days_reg_to_publication)


#completion_year
ct_all$completion_year <- as.character(year(as_date(ct_all$completion_date)))
#Character variable denoting year, 20 missing


#primary_completion_year
ct_all$primary_completion_year <- as.character(year(as_date(ct_all$primary_completion_date)))
#Character variable denoting year

# Save analysis dataset ---
colnames(ct_all)
#write.csv(ct_all, here::here("data", "clinical-trial-registry", "13-9-2024-analysis.csv"), row.names = FALSE)