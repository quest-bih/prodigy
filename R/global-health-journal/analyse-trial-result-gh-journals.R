# PRODIGY: An exploratory analysis of transparency and stakeholder engagement in Global Health Research
# This script assesses sample of trial result publications from selected global health journals (see supplementary file: https://osf.io/pxj52)

library(unpaywallR)
library(oddpub)
library(pdfRetrieve)
library(roadoi)
library(CRediT)
library(readxl)
library(here)
library(readr)
library(dplyr)


# **Part 1: Analyse open data, code and access---

# Get data
global_health_articles_raw <- 
  read_excel(
  here::here("data", "global-health-journal", "13-4-2024-global-health-article.xlsx"),
  sheet = "extraction sheet"
)

# Filter the data set to include only rows where 'study_type' is 'Clinical Trial'
global_health_articles <- 
  global_health_articles_raw |>
  filter(`Study type` == "Clinical trial publication" & 
           `Sub: Study type` %in% c("a. Primary clinical trial publication")) |>
  slice_head(n = 200)

table(global_health_articles$`Sub: Study type`)

# Extract DOIs from the dataset
dois <- global_health_articles$DOI

# Define the email and folder for saving PDFs
email <- "my-email"
save_folder <- "./data/global-health-journal/PDF_folder"

# Retrieve PDFs using pdf_retrieve
# Note: Ensure pdf_retrieve can handle a vector of DOIs
pdfRetrieve::pdf_retrieve(
  dois = dois,
  email = email,
  save_folder = save_folder
)

# Converts PDFs contained in one folder to txt-files and saves them into the output folder.
output_folder <- "C:/Users/hp/Documents/GitHub/prodigy/data/global-health-journal/Output_folder/"
oddpub::pdf_convert(here::here("data/global-health-journal/PDF_folder"), output_folder)

# Loads all text files from given folder.
PDF_text_sentences <- oddpub::pdf_load(output_folder)

# Get open data search statements (data, code)
oddpub_results <- oddpub::open_data_search(PDF_text_sentences)

oddpub_results$article <- 
  oddpub_results$article |>
  str_remove(fixed(".txt")) |>
  str_replace_all(fixed("+"), "/")
# write_csv(oddpub_results, here("data", "global-health-journal", "oddpub_results.csv"))
oddpub_results <- read_csv("data/global-health-journal/oddpub_results.csv") |> select(-`...1`)


# open access status with roadoi
open_access_result_roadoi <- roadoi::oadoi_fetch(dois = dois, email = "my-email")
#write_csv(open_access_result_roadoi, here("data", "global-health-journal", "open_access_results_roadoi.csv"))
open_access_result_roadoi <- read_csv("data/global-health-journal/open_access_result_roadoi.csv")
table(open_access_result_roadoi$is_oa)

# check to compare open access status with unpaywall
open_access_result_unpaywall <- unpaywallR::dois_OA_colors(dois = dois, email = "my-email")
#write_csv(open_access_result_unpaywall, here("data", "global-health-journal", "open_access_result_roadoi.csv"))
open_access_result_unpaywall <- read_csv("data/global-health-journal/open_access_result_unpaywall.csv")
table(open_access_result_unpaywall$OA_color)

open_access_result_roadoi |>
  count(is_oa, oa_status)


# Add  indicators to main data
# Step 1: Add oddpub results
global_health_articles <- 
  global_health_articles |>
  mutate(has_article = DOI %in% oddpub_results$article)

global_health_articles <- global_health_articles |>
  left_join(oddpub_results, by = c("DOI" = "article"))


# Step 2: Add open access results
global_health_articles <- 
  global_health_articles |>
  mutate(has_open_access_data = DOI %in% open_access_result_roadoi$doi)

# Perform the left join and select only the relevant columns
global_health_articles <- global_health_articles |>
  left_join(open_access_result_roadoi |> select(doi, is_oa, oa_status), by = c("DOI" = "doi")) |>
  select(-`TRN present`, -Comments, -...18)

oddpub_results |>
  count(open_data_category, is_open_data)


# **Count stakeholder statements from manual search---


PSE_screening_final <- 
  read_excel(here::here("data/global-health-journal/PSE-screening-final.xlsx"))

PSE_screening_final$`Final- PSE statement (Yes/No)` <- ifelse(PSE_screening_final$`Final- PSE statement (Yes/No)` == "Yes", TRUE, FALSE)
PSE_screening_final$`Negative statements`  <- ifelse(PSE_screening_final$`Negative statements` == "Yes", TRUE, FALSE)



pse_output <- 
  PSE_screening_final |>
  count(`Final- PSE statement (Yes/No)`, `Negative statements`)

mean(PSE_screening_final$`Final- PSE statement (Yes/No)`) * 100
prop_test_pse <- prop.test(sum(PSE_screening_final$`Final- PSE statement (Yes/No)`), nrow(PSE_screening_final))
prop_test_pse$conf.int


DT::datatable(pse_output)