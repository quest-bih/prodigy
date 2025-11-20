# PRODIGY: An exploratory analysis of transparency and stakeholder engagement in Global Health Research

# This scripts is extension of manual publication searches for included clinical trials (see:https://osf.io/hqdns/)

library(here)
library(dplyr)

# Define publication search (PS) list path for rater 1 
rater_1_ps_path <- 
  fs::path_home("insert path",
                "NAH-final-publication-search-17-6-2024.xlsx")

# Read funder list
rater_1_ps <- 
  readxl::read_excel(
  rater_1_ps_path,
  sheet = "17-06-2024-publication_search_f"
)

# Define publication search (PS) list path for rater 2
rater_2_ps_path <- 
  fs::path_home("insert path",
                "SSY-final-publication-search-17-6-2024.xlsx")

# Read funder list
rater_2_ps <- 
  readxl::read_excel(
  rater_2_ps_path,
  sheet = "17-06-2024-publication_search_f"
)


# Combine the two data frames by rows
reconciliation_check_raw <- dplyr::bind_rows(rater_1_ps, rater_2_ps)


# Read the CSV file and store it in a variable
reconciliation_check_raw <- read.csv(here::here("data", "clinical-trial-registry", "publication_search", "reconciliation_check_raw.csv"), check.names = FALSE)

# Select desired column
reconciliation_check <-
  reconciliation_check_raw |>
  select(nct_id,
          `rater 1`, 
         `any paper 1`, 
         `publication found  1`, 
         `PMID 1`, 
         `DOI 1(format: e.g 10.1000/182)`, 
         `URL 1 (if not avaiable DOI or PMID )`, 
         `earliest publication date 1 (DD/MM/YYYY)`,
         `rater 2`, 
         `any paper 2`, 
         `publication found 2`, 
         `PMID 2`, 
         `DOI 2(format: e.g 10.1000/182)`, 
         `URL 2 (if not avaiable DOI or PMID )`, 
         `earliest publication date 2 (DD/MM/YYYY)`) 

# Replace blank cells with NA in all columns
reconciliation_check <- reconciliation_check %>%
  mutate(across(everything(), ~ na_if(trimws(.), "")))



#for publication found by one and not fond by another person, check
#for publication found true, but pmid or doi different then check
#for publication found true, doi and pmid matching but date different check


# Recode and compare columns
reconciliation_check <- 
  reconciliation_check |>
  recode_and_compare("any paper 1", "any paper 2", "is_any_paper_match") |>
  recode_and_compare("publication found  1", "publication found 2", "is_publication_found_match") |>
  recode_and_compare("PMID 1", "PMID 2", "is_pmid_match") |>
  recode_and_compare("DOI 1(format: e.g 10.1000/182)", "DOI 2(format: e.g 10.1000/182)", "is_doi_match") |>
  recode_and_compare("URL 1 (if not avaiable DOI or PMID )", "URL 2 (if not avaiable DOI or PMID )", "is_url_match") 


# how many percent actually matched (found paper and same pmid)
#FALSE  TRUE 
#44   156 

# Add is_match column based on multiple match conditions
reconciliation_check <- 
  reconciliation_check |>
  mutate(
    is_match = if_else(
      is_any_paper_match & 
        is_pmid_match & 
        is_doi_match &
        is_url_match ,
      TRUE,
      FALSE
    )
  ) 
table(reconciliation_check$is_match)
#FALSE  TRUE 
#77   123 

#write.csv(reconciliation_check, here::here("data", "clinical-trial-registry", "publication_search", "reconciliation_check_final.csv"), row.names = FALSE)