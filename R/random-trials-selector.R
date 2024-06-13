#' @description This function returns a random number of trials for the selected category
#' @param  dataset: A dataframe containing the set of trials from which to select.
#' @param  num_trials_LMIC: An integer specifying the number of "Low- and Middle-Income Country (LMIC)" based trials to randomly select
#' @param  num_trials_HIC: An integer specifying the number of "High-Income Country (HIC)" based trials to randomly select
#'
#' @return A dataframe containing the randomly selected trials from the dataset.
#' Note: Sets a seed value for reproducibility, change accordingly

random_trials_selector <- function(dataset, num_trials_LMIC, num_trials_HIC) {
  # Check if the total number of trials to select exceeds the number of trials in the dataset
  if ((num_trials_LMIC + num_trials_HIC) > nrow(dataset)) {
    stop("Total number of trials to select exceeds the number of trials in the dataset.")
  }
  
  # Set seed for reproducibility
  set.seed(123)  # Change the seed value if needed
  
  # Randomly sample LMIC trials from the dataset
  selected_LMIC <- dataset[dataset$region == "LMIC", ][sample(nrow(dataset[dataset$region == "LMIC",]), num_trials_LMIC), ]
  
  # Randomly sample HIC trials from the dataset
  selected_HIC <- dataset[dataset$region == "HIC", ][sample(nrow(dataset[dataset$region == "HIC",]), num_trials_HIC), ]
  
  # Combine selected LMIC and HIC trials
  selected_trials <- rbind(selected_LMIC, selected_HIC)
  
  return(selected_trials)
}
