



# Load dplyr package
library(dplyr)

# Define a function to handle NA conversion and matching
recode_and_compare <- function(df, col1, col2, new_col_name) {
  df %>%
    mutate(
      # Convert blank cells to NA for both columns
      across(all_of(c(col1, col2)), ~ na_if(., "")),
      
      # Perform comparison and handle NA values
      !!new_col_name := case_when(
        # Both columns are NA or blank
        is.na(get(col1)) & is.na(get(col2)) ~ TRUE,
        
        # One column is NA and the other has a value
        is.na(get(col1)) | is.na(get(col2)) ~ FALSE,
        
        # Both columns have values and they match
        get(col1) == get(col2) ~ TRUE,
        
        # All other cases where values do not match
        TRUE ~ FALSE
      )
    )
}
