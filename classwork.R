# % Cofficent of variations : (sd /mean v) * 100
#first function : mean() - calculates the mean of a vector
# Calculating mean using the basic mathematical formula: Sum / Count
calc_mean <- function(observed_values) {
    mean_value <- sum(observed_values) /length(observed_values)
  return(round(mean_value, digits = 2))
}

#second function : sd() - calculates the standard deviation of a vector
calc_sd <- function(observed_values) {
    sd_value <- sqrt(sum((observed_values - calc_mean(observed_values))^2, na.rm = TRUE) / (length(observed_values) - 1))
    return(round(sd_value, digits= 2))
}

#third function : cv() - calculates the coefficient of variation of a vector
calc_cv <- function(observed_values) {
    cv <- (calc_sd(observed_values) / calc_mean(observed_values)) * 100
    return(round(cv, digits= 2))
}   
