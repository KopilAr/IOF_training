# % Cofficent of variations
calc_mean <- function(observed_values) {
    mean_value <- sum(observed_values) /length(observed_values)
    return(round(mean_value, digits = 2))
}


calc_sd <- function(observe_values) {
    sd_value <- sqrt(sum((observe_values - calc_mean(observe_values))^2, na.rm = TRUE) / (length(observe_values) - 1))
    return(round(sd_value, digits= 2))
}

check_data <- function(observed_values) {
    if (any(is.na(observed_values)) | any(observed_values < 0) | length(observed_values) < 2 | any(observed_values == 0)) {
        warning("The data contains missing values.")
    }
   
    else{
         return (round(((calc_sd(observed_values) / calc_mean(observed_values)) * 100),2))

    }
}   

calc_cv <- function(observed_values) {
check_data(observed_values)  # Call the guarding function first
    }

calc_cv(c(10, 12, 15, 20))