# This script processes clinical trials with the condition "tuberculosis" from ClinicalTrials.gov

# Load required libraries
library(dplyr)
library(aactr)
library(here)

# Get data
tb_trials_raw <- read.csv(here::here("data", "clinical-trial-registry",  "tuberculosis-trials", "13-4-2024-tuberculosis-CT.csv"))

# Download AACT table
#aactr::download_aact(
# ids =  tb_trials_raw$NCT.number,
#dir = here::here("data","clinical-trial-registry", "tuberculosis-trials", "raw") ,
#user = "user", overwrite = TRUE
#)

# Process AACT table
#aactr::process_aact(dir_in = here::here("data","clinical-trial-registry", "tuberculosis-trials", "raw"), dir_out = here::here("data","clinical-trial-registry","tuberculosis-trials", "processed"))


# Step 1: Read processed tables

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


# Step 2: Convert date columns to Date type
tb_studies_raw <- tb_studies_raw |>
  mutate(
    start_date = as.Date(start_date),
    completion_date = as.Date(completion_date),
    primary_completion_date = as.Date(primary_completion_date)
  ) 


# Step 3: Join the facility address and apply the prodigy clinical trial registry filter
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
    category != "No location"
  )



# Step 4: Define list of high-income countries
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

# Step 5: Create a new column 'region' based on the country classification
tb_trials <- 
  tb_trials |>
  mutate(region = ifelse(country %in% high_income_countries, "HIC", "LMIC"))

# Print the table of regions to verify the distribution
print(table(tb_trials$region))

# Step 6: Select random 100 trials (two-thirds from LIC region)
source(here::here("R", "random-trials-selector.R"))
tb_trials <- random_trials_selector(tb_trials, 67, 33)
write.csv(tb_trials,here::here("data", "clinical-trial-registry", "11-6-2024-tuberculosis-trials.csv"))

# Step 7: Determine how many trials have derived publications
ctgov_references <- readRDS(here::here("data", "clinical-trial-registry", "tuberculosis-trials", "processed", "ctgov-references.rds"))

references_derived_tb <- 
  tb_trials |>
  left_join(ctgov_references, by = "nct_id")

# Count the number of trials with derived references
references_derived_tb |>
  distinct(nct_id, .keep_all = TRUE) |>
  count(reference_derived == TRUE)

# Step 8: Filter the dataset to retain only rows where reference is derived
unique_derived_rows <- 
  references_derived_tb |>
  filter(reference_derived == TRUE) |>
  distinct(nct_id, .keep_all = TRUE)

# Step 9: Determine how many are open access publications
tb_oa <- roadoi::oadoi_fetch(dois = unique_derived_rows$doi)

# Count trials with summary results
tb_trials |> count(has_summary_results)

# Count trials by recruitment status
tb_trials |> count(recruitment_status)
