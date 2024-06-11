# Description: This script fetches and analyzes clinical trials related to maternal health conditions, including postpartum depression, maternal sepsis, and maternal anemia, from ClinicalTrials.gov.

# Load required libraries
library(dplyr)
library(aactr)
library(here)

# Step 1: Fetch clinical trials for postpartum depression --------------------------

# Load raw data
postpartum_depression_trials_raw <- read.csv(here::here("data", "clinical-trial-registry","maternal-health-condition-trials" ,"13-4-2024-postpartum-depression-CT.csv"))

# Download and process AACT table (comment out for now)
#aactr::download_aact(
# ids = ppd_trials_raw$NCT.Number,
#dir = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "ppd") ,
#user = "user", overwrite = TRUE_raw
#)

#aactr::process_aact(dir_in = here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "ppd"), dir_out = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "ppd_processed"))


# Read processed data
postpartum_depression_studies <- read_rds(here::here("data", "clinical-trial-registry","maternal-health-condition-trials","ppd_processed", "ctgov-studies.rds"))
postpartum_depression_facility <- read_rds(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "ppd_processed", "ctgov-facility-affiliations.rds"))

# Convert date columns to Date type
postpartum_depression_studies <- postpartum_depression_studies |>
  mutate(
    start_date = as.Date(start_date),
    completion_date = as.Date(completion_date),
    primary_completion_date = as.Date(primary_completion_date)
  )

# Join facility address and apply filters
postpartum_depression_trials <- 
  postpartum_depression_studies |>
  left_join(postpartum_depression_facility, by = "nct_id") |>
  filter(
    !is.na(primary_completion_date),
    !is.na(start_date),
    start_date >= as.Date("2008-01-01"), 
    primary_completion_date <= as.Date("2019-03-31"),
    is_multicentric == FALSE
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

postpartum_depression_trials <- 
  postpartum_depression_trials |>
  mutate(region = ifelse(country %in% high_income_countries, "HIC", "LMIC"))

# Step 2: Fetch clinical trials for maternal sepsis --------------------------

# Load raw data
maternal_sepsis_trials_raw <- read.csv(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "13-4-2024-maternal-sepsis-CT.csv")) 

# Only retain trials with female participant
maternal_sepsis_trials_raw <-
  maternal_sepsis_trials_raw |>
  filter(Gender == "Female")


# Download and process AACT table
#aactr::download_aact(
# ids = maternal_sepsis_trials_raw$NCT.Number,
#dir = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "sepsis_raw") ,
#user = "user", overwrite = TRUE
#)
#aactr::process_aact(dir_in = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "sepsis_raw"), dir_out = here::here("data","clinical-trial-registry","maternal-health-condition-trials", "sepsis_processed"))

# Read processed table
ms_studies_raw <- readRDS(here::here("data", "clinical-trial-registry","maternal-health-condition-trials","sepsis_processed", "ctgov-studies.rds"))
ms_facility <- readRDS(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "sepsis_processed", "ctgov-facility-affiliations.rds"))

# Read processed data
maternal_sepsis_studies_raw <- readRDS(here::here("data", "clinical-trial-registry","maternal-health-condition-trials","sepsis_processed", "ctgov-studies.rds"))
maternal_sepsis_facility <- readRDS(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "sepsis_processed", "ctgov-facility-affiliations.rds"))

# Convert date columns to Date type
maternal_sepsis_studies_raw <- maternal_sepsis_studies_raw |>
  mutate(
    start_date = as.Date(start_date),
    completion_date = as.Date(completion_date),
    primary_completion_date = as.Date(primary_completion_date)
  )

# Join facility address and apply filters
maternal_sepsis_trials <- 
  maternal_sepsis_studies_raw |>
  left_join(maternal_sepsis_facility, by = "nct_id") |>
  filter(
    !is.na(primary_completion_date),
    !is.na(start_date),
    start_date >= as.Date("2008-01-01"), 
    primary_completion_date <= as.Date("2019-03-31"),
    is_multicentric == FALSE
  )

# Create a new column 'region' based on the country classification
maternal_sepsis_trials <- 
  maternal_sepsis_trials |>
  mutate(region = ifelse(country %in% high_income_countries, "HIC", "LMIC"))

# Step 3: Fetch clinical trials for anemia-------------------

# Load raw data 
anemia_trials_raw <- read.csv(here::here("data", "clinical-trial-registry","maternal-health-condition-trials" ,"18-4-2024-maternal-anemia-CT.csv"))

# Download and process AACT table
#aactr::download_aact(
#  ids = anemia_trials_raw$NCT.Number,
# dir = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "anemia_raw") ,
#  user = "user", overwrite = TRUE
#)
#aactr::process_aact(dir_in = here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "anemia_raw"), dir_out = here::here("data","clinical-trial-registry", "maternal-health-condition-trials", "anemia_processed"))


# Read AACT table
anemia_studies_raw <- read_rds(here::here("data", "clinical-trial-registry","maternal-health-condition-trials","anemia_processed", "ctgov-studies.rds"))
anemia_facility <- read_rds(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "anemia_processed", "ctgov-facility-affiliations.rds"))


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
    primary_completion_date <= as.Date("2019-03-31"),
    is_multicentric == FALSE
  )


# Create a new column 'region' based on the country classification
anemia_trials <- 
  anemia_trials |>
  mutate(region = ifelse(country %in% high_income_countries, "HIC", "LMIC"))


# Step 4: Add all trials for maternal health condition of  postpartum depression ,anemia and sepsis----------

# Add all trial condition to get maternal health trials
maternal_health_trials <- rbind(postpartum_depression_trials, maternal_sepsis_trials, anemia_trials)

# source function to select random trials
source(here::here("R", "random_trials_selector.R"))
maternal_health_trials <-  random_trials_selector(maternal_health_trials, 67, 33)

# save the maternal health condition dataset
write.csv(maternal_health_trials,here::here("data", "clinical-trial-registry", "maternal-health-trials.csv"))

# Step 5: Analyze publications derived from clinical trials -------------------------

# Load references for postpartum depression trials
ctgov_references_1 <- readRDS(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "ppd_processed", "ctgov-references.rds"))

# Load references for maternal sepsis trials
ctgov_references_2 <- readRDS(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "sepsis_processed", "ctgov-references.rds"))

# Load references for maternal anemia trials
ctgov_references_3 <- readRDS(here::here("data", "clinical-trial-registry", "maternal-health-condition-trials", "anemia_processed", "ctgov-references.rds"))

# Combine all references
ctgov_references_mht <- rbind(ctgov_references_1, ctgov_references_2, ctgov_references_3)

# Load maternal health trials data
maternal_health_trials <- read.csv(here::here("data", "clinical-trial-registry","maternal-health-condition-trials","maternal-health-trials.csv"))

# Join trials with their derived references
references_derived_mh <- 
  maternal_health_trials |>
  left_join(ctgov_references_mht, by = "nct_id")

# Filter the dataset to retain only rows where reference is derived
unique_derived_rows <- 
  references_derived_mh |>
  filter(reference_derived == TRUE) |>
  distinct(nct_id, .keep_all = TRUE)

# Check how many of the derived publications are open access
mh_oa <- roadoi::oadoi_fetch(dois = unique_derived_rows$doi)

# Count the number of open access publications
table(mh_oa$is_oa)
