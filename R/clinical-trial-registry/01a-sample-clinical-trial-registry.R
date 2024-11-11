# PRODIGY: An exploratory analysis of transparency and patient engagement in Global Health Research
# Description: This script fetches analyzes clinical trials related to maternal health conditions (postpartum depression, maternal sepsis, maternal anemia) and tuberculosis trials from ClinicalTrials.gov.

# Note
# We downloaded the raw data from ClinicalTrials.gov using relevant MeSH terms for the disease.
# Processed data from the aactr package is used in the script, and code for data processing is commented out.
# The user should use the aactr package, as shown below, to fetch relevant information for identified trials.

# Load required libraries
library(dplyr)
library(aactr) # to use this package, generate username and password at https://aact.ctti-clinicaltrials.org/users/sign_up
library(here)


# **Part 1:Maternal health trial----

# Step 1: Fetch clinical trials for postpartum depression --------------------------

# Load raw data 
ppd_trials_raw <- read.csv(here::here("data", "clinical-trial-registry","maternal-health-condition-trials" ,"13-4-2024-postpartum-depression-CT.csv"))

# Download and process AACT table 
#aactr::download_aact(
 # ids = ppd_trials_raw$NCT.Number,
#  dir = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "ppd") ,
#  user = "user", 
#)

#aactr::process_aact(dir_in = here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "ppd"), dir_out = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "ppd_processed"))


# Read processed data 
ppd_studies_raw <- readRDS(here::here("data", "clinical-trial-registry","maternal-health-condition-trials","ppd_processed", "ctgov-studies.rds"))
ppd_facility_raw <- readRDS(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "ppd_processed", "ctgov-facility-affiliations.rds"))

# Calculate summary statistics and categorize
ppd_facility_summary <- 
  ppd_facility_raw |>
  group_by(nct_id) |>
  summarise(
    facility_count = n(),
    country_count = n_distinct(country)
  ) |>
  mutate(
    category = case_when(
      facility_count == 1 ~ "Monocentric",
      facility_count > 1 & country_count == 1 ~ "Multicentric Local",
      facility_count > 1 & country_count > 1 ~ "Multicentric International",
      TRUE ~ NA_character_  # Handle any other cases as NA
    )
  )

# Merge the category column back into the original data and filter
ppd_facility <- 
  ppd_facility_raw |>
  left_join(ppd_facility_summary |>
              select(nct_id, category), by = "nct_id") |>
  filter(category %in% c("Monocentric", "Multicentric Local")) |>
  select(nct_id, category, country) |>
  distinct()



# Convert date columns to Date type
ppd_studies <- ppd_studies_raw |>
  mutate(
    start_date = as.Date(start_date),
    completion_date = as.Date(completion_date),
    primary_completion_date = as.Date(primary_completion_date)
  )

# Join facility address and apply filters
ppd_trials <- 
  ppd_studies |>
  left_join(ppd_facility, by = "nct_id") |>
  filter(
    !is.na(primary_completion_date),
    !is.na(start_date),
    start_date >= as.Date("2008-01-01"), 
    completion_date <= as.Date("2019-03-31"),
    category != "No location",
    recruitment_status != "Withdrawn",
    study_type != "Observational"
  )

# Create a new column 'region' based on the country classification
# Note: Misspellings of country and city names noted on manual inspection are also added to the list 
# (e.g., "Korea, Republic of")
high_income_countries <- c(
  "American Samoa", "Germany", "Oman", "Andorra", "Gibraltar", "Panama", "Antigua and Barbuda",
  "Greece", "Poland", "Aruba", "Greenland", "Portugal", "Australia", "Guam", "Puerto Rico",
  "Austria", "Hong Kong", "Qatar", "Bahamas, The", "Hungary", "Romania", "Bahrain",
  "Iceland", "San Marino", "Barbados", "Ireland", "Saudi Arabia", "Belgium", "Isle of Man",
  "Seychelles", "Bermuda", "Israel", "Singapore", "British Virgin Islands", "Italy", "Sint Maarten (Dutch part)",
  "Brunei Darussalam", "Japan", "Slovak Republic", "Canada", "Korea, Rep.","Korea, Republic of", "Slovenia", "Cayman Islands",
  "Kuwait", "Spain", "Channel Islands", "Latvia", "St. Kitts and Nevis", "Chile", "Liechtenstein",
  "St. Martin (French part)", "Croatia", "Lithuania", "Sweden", "Curaçao", "Luxembourg", "Switzerland",
  "Cyprus", "Macao SAR, China", "Taiwan", "Czech Republic", "Malta", "Trinidad and Tobago",
  "Denmark", "Monaco", "Turks and Caicos Islands", "Estonia", "Nauru", "United Arab Emirates",
  "Faroe Islands", "Netherlands", "United Kingdom", "Finland", "New Caledonia", "United States",
  "France", "New Zealand", "Uruguay", "French Polynesia", "Northern Mariana Islands",
  "Virgin Islands (U.S.)", "Guyana", "Norway"
)

ppd_trials <- 
  ppd_trials |>
  mutate(region = ifelse(country %in% high_income_countries, "HIC", "LMIC"))

table(ppd_trials$region)

# Step 2: Fetch clinical trials for maternal sepsis --------------------------

# Load raw data
maternal_sepsis_trials_raw <- read.csv(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "13-4-2024-maternal-sepsis-CT.csv")) 

# Only retain trials with female participant
maternal_sepsis_trials_raw <-
  maternal_sepsis_trials_raw |>
  filter(Gender == "Female")


# Download and process AACT table
#aactr::download_aact(
 #ids = maternal_sepsis_trials_raw$NCT.Number,
#dir = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "sepsis_raw") ,
#user = "user", overwrite = TRUE
#)
#aactr::process_aact(dir_in = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "sepsis_raw"), dir_out = here::here("data","clinical-trial-registry","maternal-health-condition-trials", "sepsis_processed"))

# Read processed data
maternal_sepsis_studies_raw <- readRDS(here::here("data", "clinical-trial-registry","maternal-health-condition-trials","sepsis_processed", "ctgov-studies.rds"))
maternal_sepsis_facility_raw <- readRDS(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "sepsis_processed", "ctgov-facility-affiliations.rds"))

# Calculate summary statistics and categorize
maternal_sepsis_facility_summary <- 
  maternal_sepsis_facility_raw |>
  group_by(nct_id) |>
  summarise(
    facility_count = n(),
    country_count = n_distinct(country)
  ) |>
  mutate(
    category = case_when(
      facility_count == 1 ~ "Monocentric",
      facility_count > 1 & country_count == 1 ~ "Multicentric Local",
      facility_count > 1 & country_count > 1 ~ "Multicentric International",
      TRUE ~ NA_character_  # Handle any other cases as NA
    )
  )

# Merge the category column back into the original data and filter
maternal_sepsis_facility <- 
  maternal_sepsis_facility_raw |>
  left_join(maternal_sepsis_facility_summary |>
              select(nct_id, category), by = "nct_id") |>
  filter(category %in% c("Monocentric", "Multicentric Local")) |>
  select(nct_id, category, country) |>
  distinct()



# Convert date columns to Date type
maternal_sepsis_studies <- maternal_sepsis_studies_raw |>
  mutate(
    start_date = as.Date(start_date),
    completion_date = as.Date(completion_date),
    primary_completion_date = as.Date(primary_completion_date)
  )

# Join facility address and apply filters
maternal_sepsis_trials <- 
  maternal_sepsis_studies |>
  left_join(maternal_sepsis_facility, by = "nct_id") |>
  filter(
    !is.na(primary_completion_date),
    !is.na(start_date),
    start_date >= as.Date("2008-01-01"), 
    completion_date <= as.Date("2019-03-31"),
    category != "No location",
    recruitment_status != "Withdrawn",
    study_type != "Observational"
  )

# Create a new column 'region' based on the country classification
maternal_sepsis_trials <- 
  maternal_sepsis_trials |>
  mutate(region = ifelse(country %in% high_income_countries, "HIC", "LMIC"))

table(maternal_sepsis_trials$region)

# Step 3: Fetch clinical trials for anemia-------------------

# Load raw data 
anemia_trials_raw <- read.csv(here::here("data", "clinical-trial-registry","maternal-health-condition-trials" ,"18-4-2024-maternal-anemia-CT.csv"))

# Download and process AACT table
#aactr::download_aact(
#  ids = anemia_trials_raw$NCT.Number,
#  dir = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "anemia_raw") ,
#  user = "user"
#)
#aactr::process_aact(dir_in = here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "anemia_raw"), dir_out = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "anemia_processed"))


# Read AACT table
anemia_studies_raw <- readRDS(here::here("data", "clinical-trial-registry","maternal-health-condition-trials","anemia_processed", "ctgov-studies.rds"))
anemia_facility_raw <- readRDS(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "anemia_processed", "ctgov-facility-affiliations.rds"))

# Calculate summary statistics and categorize
anemia_facility_summary <- 
  anemia_facility_raw |>
  group_by(nct_id) |>
  summarise(
    facility_count = n(),
    country_count = n_distinct(country)
  ) |>
  mutate(
    category = case_when(
      facility_count == 1 ~ "Monocentric",
      facility_count > 1 & country_count == 1 ~ "Multicentric Local",
      facility_count > 1 & country_count > 1 ~ "Multicentric International",
      TRUE ~ NA_character_  # Handle any other cases as NA
    )
  )

# Merge the category column back into the original data and filter
anemia_facility <- 
  anemia_facility_raw |>
  left_join(anemia_facility_summary |>
              select(nct_id, category), by = "nct_id") |>
  filter(category %in% c("Monocentric", "Multicentric Local")) |>
  select(nct_id, category, country) |>
  distinct()

# Convert date columns to Date type
anemia_studies_raw <- anemia_studies_raw |>
  mutate(
    start_date = as.Date(start_date),
    completion_date = as.Date(completion_date),
    primary_completion_date = as.Date(primary_completion_date)
  )

# Join the facility address and apply the prodigy clinical trial registry filter
anemia_trials <- 
  anemia_studies_raw |>
  left_join(anemia_facility, by = "nct_id") |>
  filter(
    !is.na(primary_completion_date),
    !is.na(start_date),
    start_date >= as.Date("2008-01-01"), 
    completion_date <= as.Date("2019-03-31"),
    category != "No location",
    recruitment_status != "Withdrawn",
    study_type != "Observational"
  )


# Create a new column 'region' based on the country classification
anemia_trials <- 
  anemia_trials |>
  mutate(region = ifelse(country %in% high_income_countries, "HIC", "LMIC"))

table(anemia_trials$region)

# Step 4: Add all trials for maternal health condition of  postpartum depression ,anemia and sepsis----------

# Add all trial condition to get maternal health trials
maternal_health_trials <- rbind(ppd_trials, maternal_sepsis_trials, anemia_trials)

# source function to select random trials
source(here::here("R", "random-trials-selector.R"))
maternal_health_trials <-  random_trials_selector(maternal_health_trials, 67, 33)

# save the maternal health condition dataset
#write.csv(maternal_health_trials,here::here("data", "clinical-trial-registry", "17-6-2024-maternal-health-trials.csv"))

### **Part 2: Tuberculosis clinical trial-----

# This script processes clinical trials with the condition "tuberculosis" from ClinicalTrials.gov

# Get data
tb_trials_raw <- read.csv(here::here("data", "clinical-trial-registry",  "tuberculosis-trials", "13-4-2024-tuberculosis-CT.csv"))

# Download AACT table
aactr::download_aact(
 ids =  tb_trials_raw$NCT.number,
dir = here::here("data","clinical-trial-registry", "tuberculosis-trials", "raw") ,
user = "user",
)

# Process AACT table
aactr::process_aact(dir_in = here::here("data","clinical-trial-registry", "tuberculosis-trials", "raw"), dir_out = here::here("data","clinical-trial-registry","tuberculosis-trials", "processed"))


# Step 1: Read processed tables----

# Read the table containing study information
tb_studies_raw <- readRDS(here::here("data", "clinical-trial-registry", "tuberculosis-trials", "processed", "ctgov-studies.rds"))

# Read and process the table containing facility affiliations
tb_facility_raw <- readRDS(here::here("data", "clinical-trial-registry", "tuberculosis-trials", "processed", "ctgov-facility-affiliations.rds"))

# Calculate summary statistics and categorize
tb_facility_summary <- 
  tb_facility_raw |>
  group_by(nct_id) |>
  summarise(
    facility_count = n(),
    country_count = n_distinct(country)
  ) %>%
  mutate(
    category = case_when(
      facility_count == 1 ~ "Monocentric",
      facility_count > 1 & country_count == 1 ~ "Multicentric Local",
      facility_count > 1 & country_count > 1 ~ "Multicentric International",
      TRUE ~ NA_character_  # Handle any other cases as NA
    )
  )

# Merge the category column back into the original data and filter
tb_facility <- 
  tb_facility_raw |>
  left_join(tb_facility_summary |>
              select(nct_id, category), by = "nct_id") |>
  filter(category %in% c("Monocentric", "Multicentric Local")) |>
  select(nct_id, category, country) |>
  distinct()


# Step 2: Convert date columns to Date type----
tb_studies_raw <- tb_studies_raw |>
  mutate(
    start_date = as.Date(start_date),
    completion_date = as.Date(completion_date),
    primary_completion_date = as.Date(primary_completion_date)
  ) 


# Step 3: Join the facility address and apply the prodigy clinical trial registry filter ---
tb_trials <- 
  tb_studies_raw |>
  left_join(tb_facility, by = "nct_id") |>
  mutate(
    category = ifelse(is.na(category), "No location", category)
  ) |>
  filter(
    !is.na(primary_completion_date),
    !is.na(start_date),
    start_date >= as.Date("2008-01-01"), 
    completion_date <= as.Date("2019-03-31"),
    category != "No location",
    recruitment_status != "Withdrawn",
    study_type != "Observational"
    
  )



# Step 4: Define list of high-income countries ----
# Note: Misspellings of country and city names noted on manual inspection are also added to the list 
# (e.g., "Korea, Republic of")
high_income_countries <- c(
  "American Samoa", "Germany", "Oman", "Andorra", "Gibraltar", "Panama", "Antigua and Barbuda",
  "Greece", "Poland", "Aruba", "Greenland", "Portugal", "Australia", "Guam", "Puerto Rico",
  "Austria", "Hong Kong", "Qatar", "Bahamas, The", "Hungary", "Romania", "Bahrain",
  "Iceland", "San Marino", "Barbados", "Ireland", "Saudi Arabia", "Belgium", "Isle of Man",
  "Seychelles", "Bermuda", "Israel", "Singapore", "British Virgin Islands", "Italy", "Sint Maarten (Dutch part)",
  "Brunei Darussalam", "Japan", "Slovak Republic", "Canada", "Korea, Rep.","Korea, Republic of", "Slovenia", "Cayman Islands",
  "Kuwait", "Spain", "Channel Islands", "Latvia", "St. Kitts and Nevis", "Chile", "Liechtenstein",
  "St. Martin (French part)", "Croatia", "Lithuania", "Sweden", "Curaçao", "Luxembourg", "Switzerland",
  "Cyprus", "Macao SAR, China", "Taiwan", "Czech Republic", "Malta", "Trinidad and Tobago",
  "Denmark", "Monaco", "Turks and Caicos Islands", "Estonia", "Nauru", "United Arab Emirates",
  "Faroe Islands", "Netherlands", "United Kingdom", "Finland", "New Caledonia", "United States",
  "France", "New Zealand", "Uruguay", "French Polynesia", "Northern Mariana Islands",
  "Virgin Islands (U.S.)", "Guyana", "Norway"
)

# Step 5: Create a new column 'region' based on the country classification ----
tb_trials <- 
  tb_trials |>
  mutate(region = ifelse(country %in% high_income_countries, "HIC", "LMIC"))

# Print the table of regions to verify the distribution
print(table(tb_trials$region))

# Step 6: Select random 100 trials (two-thirds from LIC region) ----
source(here::here("R", "random-trials-selector.R"))
tb_trials <- random_trials_selector(tb_trials, 67, 33)
#write.csv(tb_trials,here::here("data", "clinical-trial-registry", "17-6-2024-tuberculosis-trials.csv"))

# Count trials with summary results
tb_trials |> count(has_summary_results)

# Count trials by recruitment status
tb_trials |> count(recruitment_status)

