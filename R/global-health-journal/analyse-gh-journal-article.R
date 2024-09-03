# This script retrieves and assess clinical trial publication

#remotes::install_github("ropensci-archive/rplos", force = TRUE)
#remotes::install_github("ropensci-archive/fulltext", force = TRUE)
#remotes::install_github("ropensci-archive/crminer", force = TRUE)
#remotes::install_github("ontogenerator/ContriBOT", force = TRUE)
remotes::install_github("quest-bih/oddpub", force = TRUE)
library(oddpub)
library(pdfRetrieve)
library(roadoi)
library(CRediT)
library(readxl)
library(here)
library(readr)

# Get data
global_health_articles_raw <- 
  read_excel(
  here::here("data", "global-health-journal", "13-4-2024-global-health-article.xlsx"),
  sheet = "extraction sheet"
)

# Filter the dataset to include only rows where 'study_type' is 'Clinical Trial'
global_health_articles <- 
  global_health_articles_raw |>
  filter(`Study type` == "Clinical trial publication" & 
           `Sub: Study type` %in% c("a. Primary clinical trial publication")) |>
  slice_head(n = 200)

table(global_health_articles$`Sub: Study type`)

# Extract DOIs from the dataset
#dois <- global_health_articles$DOI

# Define the email and folder for saving PDFs
#email <- "samruddhi2815@gmail.com"
#save_folder <- "./PDF_folder"

# Retrieve PDFs using pdf_retrieve
# Note: Ensure pdf_retrieve can handle a vector of DOIs
#pdfRetrieve::pdf_retrieve(
#  dois = dois,
 # email = email,
#  save_folder = save_folder
#)

# Converts PDFs contained in one folder to txt-files and saves them into the output folder.
#output_folder <- "C:/Users/hp/Documents/GitHub/prodigy/Output_folder/"
#oddpub::pdf_convert(here::here("PDF_folder"), output_folder)

# Loads all text files from given folder.
#PDF_text_sentences <- oddpub::pdf_load(output_folder)

# Get open data statement
#oddpub_results <- oddpub::open_data_search(PDF_text_sentences)

#oddpub_results$article <- 
#  oddpub_results$article |>
 # str_remove(fixed(".txt")) |>
#  str_replace_all(fixed("+"), "/")
#write_csv(oddpub_results, here("data", "global-health-journal", "oddpub_results.csv"))
oddpub_results <- read_csv("data/global-health-journal/oddpub_results.csv") |> select(-`...1`)

mean(oddpub_results$is_open_data) * 100
prop_test_data <- prop.test(sum(oddpub_results$is_open_data), nrow(oddpub_results))
prop_test_data$conf.int
furniture::table1(oddpub_results, open_data_category, na.rm = F)
mean(oddpub_results$is_open_code) * 100
prop_test_code <- prop.test(sum(oddpub_results$is_open_code), nrow(oddpub_results))
conf_int_formatted <- format(prop_test_code$conf.int, digits = 10)

# Is publication open access
#open_access_results <- roadoi::oadoi_fetch(dois = dois, email = "samuddhi.yerunkar@charite.de")
#write_csv(open_access_results, here("data", "global-health-journal", "open_access_results.csv"))
open_access_results <- read_csv("data/global-health-journal/open_access_results.csv")

mean(open_access_results$is_oa) * 100
prop_test_oa <- prop.test(sum(open_access_results$is_oa), nrow(open_access_results))
prop_test_oa$conf.int



# Add practice indicators to main data
# Step 1: Add oddpub results
global_health_articles <- 
  global_health_articles |>
  mutate(has_article = DOI %in% oddpub_results$article)

global_health_articles <- global_health_articles |>
  left_join(oddpub_results, by = c("DOI" = "article"))


# Step 2: Add open access results
global_health_articles <- 
  global_health_articles |>
  mutate(has_open_access_data = DOI %in% open_access_results$doi)

# Perform the left join and select only the relevant columns
global_health_articles <- global_health_articles |>
  left_join(open_access_results |> select(doi, is_oa, oa_status), by = c("DOI" = "doi")) |>
  select(-`TRN present`, -Comments, -...18)


