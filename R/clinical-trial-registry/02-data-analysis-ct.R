# This script is designed to  analyse final publication searches - Part 2

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
  read.csv(here::here("data","clinical-trial-registry", "2-9-2024-analysis.csv"), check.names  = FALSE)


#Time to registration: this variable will be analyzed under secondary outcomes (not here)
analysis <- mutate_at(analysis, vars(start_date, registration_date, completion_date), ~as_date(.))
analysis <- mutate(analysis, time_to_registration = case_when(
  registration_date < start_date ~ "Prospective registration",
  registration_date < start_date + days(61) ~ "Within 60 days after trial start",
  registration_date > start_date + days(60) ~ ">60 days after trial start"
))


# Recode demographics variables ----

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

#Time to registration: this variable will be analyzed under secondary outcomes (not here)
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
  furniture::table1(analysis,recruitment_status, phase_recoded,sample_size,masking_recoded,allocation, time_to_registration,na.rm = F)

### Create and save table (per registry) ----
demo_table_trialtype_save <- 
  furniture::table1(analysis,recruitment_status, phase_recoded, sample_size,masking_recoded,allocation, time_to_registration, na.rm = F, splitby = "trial_type")


# Timely publication 2 years

# add days_to_publ and has_publ_or_summary
analysis <- 
  analysis |>
  mutate(days_to_publ = pmin(days_cd_to_publication,   #get minimum of days to pub or to summary result
                             days_cd_to_summary, na.rm = TRUE)) |>
  mutate(has_publ_or_summary = (has_publication)== "TRUE" | (has_summary_results== "TRUE"))


# How many reported any results during our follow-up (summary results and/or publication search)?
mean(analysis$has_publ_or_summary) * 100
prop_test <- prop.test(sum(analysis$has_publ_or_summary), nrow(analysis))
prop_test$conf.int

# How many reported any results within 1 year of completion?
non_na_days_1_year <- na.omit(analysis$days_to_publ)
num_trials_within_1_year <- sum(non_na_days_1_year <= 365)
percentage_1_year <- mean(non_na_days_1_year <= 365) * 100
n_1_year <- length(non_na_days_1_year)
ci_1_year <- binom.confint(sum(non_na_days_1_year <= 365), n_1_year, conf.level = 0.95, methods = "wilson")

# How many posted summary results in registry within 2 year of completion
non_na_days_2_years <- na.omit(analysis$days_to_publ)
num_trials_within_2_years <- sum(non_na_days_1_year <= 730)
percentage_2_years <- mean(non_na_days_2_years <= 730) * 100
n_2_years <- length(non_na_days_2_years)
ci_2_years <- binom.confint(sum(non_na_days_2_years <= 730), n_2_years, conf.level = 0.95, methods = "wilson")


# Print results
cat(
  "Number of trials with results within 1 year:", num_trials_within_1_year, "\n",
  "Percentage of trials with results within 1 year:", round(percentage_1_year, 2), "%\n",
  "95% CI for 1 year: [", round(ci_1_year$lower * 100, 2), "%, ", round(ci_1_year$upper * 100, 2), "%]\n",
  "Number of trials with results within 2 years:", num_trials_within_2_years, "\n",
  "Percentage of trials with results within 2 years:", round(percentage_2_years, 2), "%\n",
  "95% CI for 2 years: [", round(ci_2_years$lower * 100, 2), "%, ", round(ci_2_years$upper * 100, 2), "%]\n",
  sep = ""
)

# How many trials were prospectively registered?
table(analysis$is_prospective)
mean(analysis$is_prospective) * 100
prop_test <- prop.test(sum(analysis$is_prospective), nrow(analysis))
prop_test$conf.int

# Save analysis dataset ---
colnames(analysis)
write.csv(analysis, here::here("data", "clinical-trial-registry", "3-9-2024-analysis-02.csv"), row.names = FALSE)