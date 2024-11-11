# PRODIGY: An exploratory analysis of transparency and stakeholder engagement in Global Health Research

# This script analyses final clinical trial registry sample for analysis
# This code is adapted from Nordic trial reporting project (https://github.com/cathrineaxfors/nordic-trial-reporting/tree/main). 

# Load packages ----

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(furniture)
library(DescTools)
library(binom)


# Get clinical trial registry final sample
analysis <- 
  read.csv(here::here("data","clinical-trial-registry", "13-9-2024-analysis.csv"), check.names  = FALSE)

# Re-code demographics variables ----

#Combine different phase descriptions
analysis <- mutate(analysis, phase_recoded = case_when(
  phase == "Early Phase 1" | phase == "1" | phase == "Phase 1" ~ "I",
  phase == "Phase 1/Phase 2" ~ "I-II",
  phase == "2" | phase == "Phase 2" ~ "II",
  phase == "Phase 2/Phase 3" ~ "II-III",
  phase == "3" | phase == "Phase 3" ~ "III",
  phase == "4" | phase == "Phase 4" ~ "IV",
  phase == "Not Applicable" ~ "Not Applicable"))

analysis <- mutate(analysis, sample_size = case_when(
  enrollment > 0 & enrollment <= 100 ~ "1-100",
  enrollment > 100 & enrollment <= 500 ~ "100-500",
  enrollment > 500 ~ ">500"
))


analysis <- mutate(analysis, masking_recoded = case_when(
  masking == "Single" ~ "Any masking",
  masking == "Double" ~ "Any masking",
  masking == "Triple" ~ "Any masking",
  masking == "Quadruple" ~ "Any masking",
  masking == "None (Open Label)" ~ "Open Label"
))

#Time to registration
analysis <- mutate_at(analysis, vars(start_date, registration_date, completion_date), ~as_date(.))
analysis <- mutate(analysis, time_to_registration = case_when(
  registration_date < start_date ~ "Prospective registration",
  registration_date < start_date + days(61) ~ "Within 60 days after trial start",
  registration_date > start_date + days(60) ~ ">60 days after trial start"
))

# Is prospective based on time_to_registration
analysis <- mutate(analysis, 
            is_prospective = time_to_registration == "Prospective registration"
)

demo_table_save <- 
  furniture::table1(analysis,recruitment_status,country,  phase_recoded,sample_size,masking_recoded,allocation, time_to_registration,na.rm = F)

### Create and save table (per registry) ----
demo_table_trialtype_save <- 
  furniture::table1(analysis,country, recruitment_status, phase_recoded, sample_size,masking_recoded,allocation, time_to_registration, na.rm = F, splitby = "trial_type")


# Timely publication 2 years

# add days_to_publ and has_publ_or_summary
analysis <- 
  analysis |>
  mutate(days_to_publ = pmin(days_cd_to_publication,   #get minimum of days to pub or to summary result
                             days_cd_to_summary, na.rm = TRUE)) |>
  mutate(has_publ_or_summary = (has_publication)== "TRUE" | (has_summary_results== "TRUE"))


# How many reported summary results?
table(analysis$has_summary_results)
mean(analysis$has_summary_results) * 100

# How many reported publication?
table(analysis$has_publication)
mean(analysis$has_publication) * 100

# How many trials were prospectively registered?
table(analysis$is_prospective)
mean(analysis$is_prospective) * 100

# How many reported any results within 2 years
reported_within_2_years <- 
  analysis |>
filter(days_to_publ < 365 * 2, has_publ_or_summary) |>
summarise(count_within_2_years = n())

# How many reported result within 4 years
reported_within_4_years <- 
  analysis |>
  filter(days_to_publ <= 365 * 4, has_publ_or_summary) |>
  summarise(count_within_4_years = n()) 


# What is median  reporting time
summary(analysis$days_to_publ)

# Save analysis dataset ---
colnames(analysis)
#write.csv(analysis, here::here("data", "clinical-trial-registry", "3-9-2024-analysis-02.csv"), row.names = FALSE)