rm(list=ls())
# Load necessary libraries
library(readxl)
library(dplyr)
library(readr)
library(lmtest)
library(sandwich)
library(car)
library(broom)
library(knitr)
library(kableExtra)
library(lubridate)
library(tidyr)
library(purrr)
library(tidyverse)
library(vars)
library(anytime)
library(bizdays)
library(timeDate)
library(ggplot2)
library(zoo)

select <- dplyr::select

# setup
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))


## FOMC Expectations -------------------------------------------------------

# Load the data
econ_data <- read_excel("Econ_Data.xlsx", sheet = "FOMC")
econ_data <- select(econ_data, -Time)
zq3m_data <- read.csv("ZQ3M.csv")
zq4m_data <- read.csv("ZQ4M.csv")

# Generate a list of US bank holidays
years <- unique(year(econ_data$`Release Date`))
holidays <- unique(do.call(c, lapply(years, function(y) {
  as.Date(holidayNYSE(year = y))
})))

# Create the calendar
create.calendar(name='US', weekdays=c('saturday', 'sunday'), holidays=holidays)


# Format 'Date' column in zq3m_data and zq4m_data
zq3m_data$Date <- anydate(zq3m_data$Date)
zq4m_data$Date <- anydate(zq4m_data$Date)
zq3m_data <- mutate(zq3m_data, year = year(Date), month = month(Date), day_3m = as.numeric(day(Date)))
zq4m_data <- mutate(zq4m_data, year = year(Date), month = month(Date), day_4m = as.numeric(day(Date)))

# Process Econ_Data
econ_data <- econ_data %>%
  mutate(`Release Date` = as.Date(`Release Date`),
         year = year(`Release Date`),
         month = month(`Release Date`),
         policy_day = as.numeric(day(`Release Date`)),
         adjusted_policy_day = ifelse(`Release Date` == as.Date('2020-03-15'), 
                                      as.numeric(day(adjust.next(`Release Date`, 'US'))),
                                      policy_day)) # accounts for the edge case surprise FOMC held on Sunday 3/15/20, 
                                                   # which was not a business day. SO we take futures price on next business day 

# Merge with 3-month futures data
econ_data <- left_join(econ_data, zq3m_data, by = c("year", "month", "adjusted_policy_day" = "day_3m"))


# Compute the prior day for joining with 3-month futures data pre-release
econ_data <- mutate(econ_data, 
                    prior_day = adjust.previous(`Release Date` - 1, 'US'))

# Ensure the prior_day is correctly formatted in zq4m_data and zq3m_data
zq4m_data <- zq4m_data %>%
  mutate(prior_day = Date)

zq3m_data <- zq3m_data %>%
  mutate(prior_day = Date)


# Merge with 3-month futures data for the prior day
econ_data <- left_join(econ_data, zq3m_data, by = "prior_day", suffix = c("", "_3m_prior"))

# Merge with 4-month futures data for the prior day
econ_data <- left_join(econ_data, zq4m_data, by = "prior_day", suffix = c("_3m", "_4m_prior"))

# define m and t
econ_data <- econ_data %>%
  mutate(m = as.numeric(days_in_month(`Release Date`)),
         t = policy_day-1)

# Calculate the futures_rate and futures_rate_prior
# we adjust the formula such that the scale does not add noise towards end of month (recall that futures are weighted avg)
# For a FOMC at the end of the month, for example, the change in the term premium of surprise is multiplied by 30, 
# significantly increasing the noise in our data.
# Gurkaynak (2005) suggests using the next month's contract when scale>4 (i.e. if FOMC last week of the month)
# Kuttner (2001) also suggests if FOMC is first day of month, we use 1 month forward contract, on the last day of prior month 
econ_data <- econ_data %>%
  mutate(futures_rate = 100 - Price_3m,
         futures_rate_prior = ifelse(t == 0 | (m / (m - t)) > 4, 100 - Price_4m_prior, 100 - Price_3m_prior))

# Calculate surprises (in basis points)
# No scaling involved when FOMC affects the expected rates in the entire subsequent month 
# e.g. When FOMC meetings is late in the month, there are no scheduled meetings in the subsequent month (Gurkaynak 2005)
# recall FOMC takes place every 6 weeks
econ_data <- econ_data %>%
  mutate(Surprise = ifelse(t == 0 | (m / (m - t)) > 4,
                           (futures_rate - futures_rate_prior),
                           ((futures_rate - futures_rate_prior) * (m / (m - t))))*100)  # Scale

summary(econ_data$Surprise)

# Calculate mean absolute surprise and mean adjusted surprise
Mean_Absolute_Surprise <- sum(abs(econ_data$Surprise)) / length(econ_data$Surprise)
econ_data$Mean_Adjusted_Surprise <- econ_data$Surprise / Mean_Absolute_Surprise

# Print the result
print(econ_data)


# Now let's plot the surprises over time
ggplot(econ_data, aes(x = `Release Date`, y = Surprise)) +
  geom_bar(stat = "identity") +
  labs(title = "FOMC Policy Surprises", x = "", y = "Surprise") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  theme(plot.title = element_text(hjust = 0.5))


ggsave("policy_surprise_plot.png", plot = last_plot(), width = 11, height = 8, units = "in")

# Re-plot without outliers (2008-1-22, 2001-09-17, 2001-04-18)
ggplot(econ_data, aes(x = `Release Date`, y = Surprise)) +
  geom_bar(data = subset(econ_data, !`Release Date` %in% as.Date(c('2008-01-22', '2001-09-17', '2001-04-18'))),
           stat = "identity") +
  labs(title = "FOMC Policy Surprises", x = "", y = "Surprise") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("policy_surprise_plot_significant.png", plot = last_plot(), width = 11, height = 8, units = "in")


## Import/Convert Other Economic Indicators -----------------------------------------------


## import rest of economic indicators
# Define the sheet names without the 'FOMC' tab
sheets_to_load <- c("GDP", "Consumer_Credit", "PPI", "Retail_Sales", "Factory_Orders", "Consumer_Confidence", 
                    "Business_Inventories", "Initial_Jobless_Claims", "Unemployment", "Trade_Balance", 
                    "Durable_Goods_Orders", "Industrial_Production", "NFP", "Leading_Index", "Housing_Starts", "CPI", "ISM_PMI")

# Function to apply the appropriate transformations
transform_data <- function(data, sheet_name) {
  # Check for non-numeric values and attempt to convert them
  if(!is.numeric(data)) {
    # Remove K, M, B, and commas if present, then convert to numeric
    cleaned_data <- gsub(",", "", gsub("K", "", gsub("M", "", gsub("B", "", data, fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
    # Attempt to convert to numeric and check for NAs
    numeric_data <- as.numeric(cleaned_data)
    if(any(is.na(numeric_data))) {
      warning(paste("NA introduced by coercion in sheet:", sheet_name))
      print(paste("Original data:", data[is.na(numeric_data)], "Cleaned data:", cleaned_data[is.na(numeric_data)]))
    }
    data <- numeric_data
  }
  
  # Now apply the multipliers where necessary
  if(sheet_name %in% c("Initial_Jobless_Claims", "NFP")) {
    return(data) # These are in thousands
  } else if(sheet_name %in% c("Consumer_Credit", "Trade_Balance")) {
    return(data) # These are in billions
  } else if(sheet_name == "Housing_Starts") {
    return(data) # These are in millions
  } else if(sheet_name %in% c("Leading_Index", "Unemployment", "Factory_Orders", "PPI", "CPI", "Retail_Sales", 
                              "Business_Inventories", "Industrial_Production", "Durable_Goods_Orders", "GDP")) {
    return(data*100) # Convert decimals to percentages
  } else if(sheet_name %in% c("Consumer_Confidence","ISM_PMI")) {
    return(data) # No change needed (index)
  }
}

# Read and transform each sheet
list_of_dataframes <- lapply(sheets_to_load, function(sheet_name) {
  # Read the data from the sheet
  data <- read_excel("Econ_Data.xlsx", sheet = sheet_name)
  
  # Remove the second column completely
  data <- data[-2]
  
  # Apply the transformation to the remaining columns
  data[-1] <- lapply(data[-1], transform_data, sheet_name = sheet_name)
  
  # Return the transformed data
  return(data)
})


# Combine all dataframes into a list named 'economic_data'
economic_data <- setNames(list_of_dataframes, sheets_to_load)

# Convert the 'Release Date' column to Date format for all sheets
economic_data <- lapply(economic_data, function(df) {
  df$`Release Date` <- as.Date(df$`Release Date`)
  return(df)
})

# Assign the converted data frames back to their names in the list
names(economic_data) <- sheets_to_load

# Output the list for further analysis
economic_data

## Check Bias/Outliers ----------------------------------------------------


## Check for bias/outliers
# Define test_for_bias to include F-test and Ljung-Box Q-statistic
test_for_bias <- function(data, announcement_name) {
  fit <- lm(Actual ~ Forecast, data = data)  # Fit the full model
  hc_cov <- vcovHC(fit, type = "HC1")  # Compute the heteroscedasticity-consistent covariance matrix
  coefficients <- coef(fit)
  
  # Manual Wald test
  null_hypothesis <- c(0, 1)  # Null hypothesis: a = 0, b = 1
  estimated_coeff <- coefficients  # Estimated coefficients from the model
  difference <- estimated_coeff - null_hypothesis  # Difference from null hypothesis
  wald_statistic <- t(difference) %*% solve(hc_cov) %*% difference  # Wald statistic
  df <- length(coefficients)  # Degrees of freedom
  wald_pvalue <- 1 - pchisq(wald_statistic, df)  # P-value
  
  lbtest <- Box.test(resid(fit), lag = 12, type = "Ljung-Box")  # Perform Ljung-Box test
  
  return(data.frame(
    Announcement = announcement_name,
    alpha = coefficients[1],
    beta = coefficients[2],
    alpha_se = sqrt(diag(hc_cov))[1],
    beta_se = sqrt(diag(hc_cov))[2],
    F_test_pvalue = wald_pvalue,
    Q_Stat_pvalue = lbtest$p.value
  ))
}

# Function to plot residuals
plot_residuals <- function(fit, announcement_name) {
  # Create a basic plot of residuals
  plot(resid(fit), main = paste("Residuals for", announcement_name), ylab = "Residuals", xlab = "Index", pch = 20)
  abline(h = 0, col = "red") # Add a horizontal line at 0
}

# Apply the test_for_bias to each data frame in the economic_data list and plot residuals
results <- lapply(names(economic_data), function(name) {
  model_results <- test_for_bias(economic_data[[name]], name)
  fit <- lm(Actual ~ Forecast, data = economic_data[[name]])
  plot_residuals(fit, name)
  return(model_results)
})

# Combine the results into a data frame
results_df <- do.call(rbind, results)
results_df <- as.data.frame(results_df)
results_df$Q_Stat_pvalue <- format.pval(results_df$Q_Stat_pvalue, digits = 4)
results_df$alpha <- format(results_df$alpha, scientific=FALSE)

# Output the results
print(results_df)

# Write to csv
write.csv(results_df, "initial_bias_test_df.csv", row.names = FALSE)

# Function to inspect data and fit model
inspect_and_fit <- function(data, announcement_name) {
  # Check for outliers and data summary
  summary_stats <- summary(data)
  
  # Plot Actual vs Forecast
  plot(data$Forecast, data$Actual, main = announcement_name,
       xlab = "Forecast", ylab = "Actual", pch = 19)
  abline(0, 1, col = "red") # Line where Actual equals Forecast
  
  # Fit the regression model
  fit <- lm(Actual ~ Forecast, data = data)
  
  # Calculate robust standard errors
  robust_se <- coeftest(fit, vcov = vcovHC(fit, type = "HC1"))
  
  # Return results
  list(
    summary = summary_stats,
    fit = tidy(fit),
    robust_se = robust_se
  )
}

# Apply the function to inspect and fit the model to problematic COVID announcements (three examples)
problematic_announcements <- c("Unemployment", "Initial_Jobless_Claims", "NFP")
problematic_results <- lapply(problematic_announcements, function(name) {
  inspect_and_fit(economic_data[[name]], name)
})

# Check the results
problematic_results

## Remove Outliers and Re-Do Tests -----------------------------------------

# Function to calculate the surprise and mean adjusted surprise
calculate_mean_adjusted_surprise <- function(df) {
  # Calculate Surprise
  df$Surprise <- df$Actual - df$Forecast
  
  # Calculate Mean Adjusted Surprise
  df$Mean_Adjusted_Surprise <- df$Surprise / (sum(abs(df$Surprise)) / length(df$Surprise))
  
  return(df)
}

economic_data <- lapply(economic_data, calculate_mean_adjusted_surprise)

# Remove outliers from indicators with F-test p value <0.05, large intercepts, betas, etc.
# List of sheet names with bad data where outlier removal is needed
bad_sheets <- c("Initial_Jobless_Claims", "NFP", "PPI", 
                "Unemployment", "CPI", "Retail_Sales","ISM_PMI", "Industrial_Production","Durable_Goods_Orders", "Leading_Index")

# Function to remove outliers using IQR for a single column
remove_outliers_column <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column <- ifelse(column < lower_bound | column > upper_bound, NA, column)
  return(column)
}

# Function to apply outlier removal across all specified columns in a dataframe
remove_outliers_dataframe <- function(df, columns) {
  df[columns] <- lapply(df[columns], remove_outliers_column)
  df <- df[complete.cases(df[columns]), ]
  return(df)
}
# Apply the outlier removal function only to the specified sheets
economic_data <- lapply(names(economic_data), function(name) {
  if (name %in% bad_sheets) {
    return(remove_outliers_dataframe(economic_data[[name]], "Surprise")) 
  } else {
    return(economic_data[[name]])
  }
})
names(economic_data) <- sheets_to_load

# Apply the test_for_bias to each data frame in the economic_data list and plot residuals
results <- lapply(names(economic_data), function(name) {
  model_results <- test_for_bias(economic_data[[name]], name)
  fit <- lm(Actual ~ Forecast, data = economic_data[[name]])
  plot_residuals(fit, name)
  return(model_results)
})

# Combine the results into a data frame
results_df <- do.call(rbind, results)
results_df <- as.data.frame(results_df)
results_df$Q_Stat_pvalue <- format.pval(results_df$Q_Stat_pvalue, digits = 4)
results_df$alpha <- format(results_df$alpha, scientific=FALSE)

# Output the results
print(results_df)

# Save to a CSV file:
write.csv(results_df, "bias_test_df.csv", row.names = FALSE)

## Add FOMC and output summary stats ----------------------------------------------

## Add FOMC dataframe
FOMC <- econ_data %>%
  select('Release Date', Surprise, Mean_Adjusted_Surprise)

economic_data <- c(economic_data, list(FOMC))
sheets_to_load <- c("GDP", "Consumer_Credit", "PPI", "Retail_Sales", "Factory_Orders", "Consumer_Confidence", 
                    "Business_Inventories", "Initial_Jobless_Claims", "Unemployment", "Trade_Balance", 
                    "Durable_Goods_Orders", "Industrial_Production", "NFP", "Leading_Index", "Housing_Starts", "CPI","ISM_PMI", "FOMC")
names(economic_data) <- sheets_to_load

# FOMC remove outliers
bad_sheets <- c("FOMC")

economic_data <- lapply(names(economic_data), function(name) {
  if (name %in% bad_sheets) {
    return(remove_outliers_dataframe(economic_data[[name]], "Surprise"))
  } else {
    return(economic_data[[name]])
  }
})
names(economic_data) <- sheets_to_load

## Announcement Summary Stats

# Initialize an empty data frame to store summary statistics for each announcement
summary_stats_df <- data.frame(
  Announcement = character(),
  Units = character(),
  Observations = integer(),
  Mean = numeric(),
  SD = numeric(),
  Min = numeric(),
  Max = numeric(),
  Mean_Abs_Surprise = numeric(),
  stringsAsFactors = FALSE
)

# Manually define the units for each announcement based on the provided information
units <- c("% change (annualized)", "change in $ billions", "% change", "% change", 
           "% change", "index value", "% change", "thousands", 
           "% level", "$ billions", "% change", "% change", 
           "change in thousands", "% change", "millions", "% change","index value", 
           "basis points")

# Calculate the summary statistics for each dataframe in the economic_data list
for (i in seq_along(economic_data)) {
  df <- economic_data[[i]]
  announcement <- names(economic_data)[i]
  
  # Ensure there's a Surprise column in the dataframe
  if ("Surprise" %in% names(df)) {
    obs <- nrow(df)
    mean <- mean(df$Surprise, na.rm = TRUE)
    sd <- sd(df$Surprise, na.rm = TRUE)
    min <- min(df$Surprise, na.rm = TRUE)
    max <- max(df$Surprise, na.rm = TRUE)
    mean_abs_surprise <- sum(abs(df$Surprise), na.rm = TRUE) / obs
    
    # Create a new row for the summary statistics dataframe
    new_row <- data.frame(
      Announcement = announcement,
      Units = units[i], 
      Observations = obs,
      Mean = mean,
      SD = sd,
      Min = min,
      Max = max,
      Mean_Abs_Surprise = mean_abs_surprise
    )
    
    # Bind the new row to the summary statistics dataframe
    summary_stats_df <- rbind(summary_stats_df, new_row)
  } else {
    warning(paste("No 'Surprise' column found in", announcement))
  }
}

# Print the resulting summary statistics dataframe
print(summary_stats_df)

# Save to a CSV file:
write.csv(summary_stats_df, "summary_stats_df.csv", row.names = FALSE)


## Import/Convert Bond Yields ---------------------------------------


# Load bond data from CSV files for different maturities
DGS3MO <- read_csv("DGS3MO.csv")
DGS1 <- read_csv("DGS1.csv")
DGS2 <- read_csv("DGS2.csv")
DGS5 <- read_csv("DGS5.csv")
DGS10 <- read_csv("DGS10.csv")
DGS30 <- read_csv("DGS30.csv")

# Convert the DATE column to DATE type and rates to numeric, handling missing values
convert_data <- function(df, column_name) {
  df %>%
    mutate(DATE = ymd(DATE)) %>%
    mutate_at(vars(column_name), list(~na_if(., "."))) %>%
    mutate_at(vars(column_name), list(~as.numeric(.))) %>%
    drop_na()
}

# Convert Data
DGS3MO <- convert_data(DGS3MO, "DGS3MO")
DGS1 <- convert_data(DGS1, "DGS1")
DGS2 <- convert_data(DGS2, "DGS2")
DGS5 <- convert_data(DGS5, "DGS5")
DGS10 <- convert_data(DGS10, "DGS10")
DGS30 <- convert_data(DGS30, "DGS30")

# Function to preprocess bond data and calculate daily changes in bps
preprocess_bond_data <- function(bond_data, maturity_name) {
  bond_data <- bond_data %>%
    mutate(DATE = as.Date(DATE)) %>%
    arrange(DATE) %>%
    mutate(!!sym(paste0("Change_", maturity_name)) := c(NA, diff(!!sym(maturity_name))*100))
  return(bond_data)
}

# Preprocess and calculate changes for each maturity's bond data
DGS3MO <- preprocess_bond_data(DGS3MO, "DGS3MO")
DGS1 <- preprocess_bond_data(DGS1, "DGS1")
DGS2 <- preprocess_bond_data(DGS2, "DGS2")
DGS5 <- preprocess_bond_data(DGS5, "DGS5")
DGS10 <- preprocess_bond_data(DGS10, "DGS10")
DGS30 <- preprocess_bond_data(DGS30, "DGS30")


# Combine all bond data into one dataframe by merging them on the DATE column
bond_data_combined <- reduce(list(DGS3MO, DGS1, DGS2, DGS5, DGS10, DGS30), full_join, by = "DATE")
bond_data_combined <- bond_data_combined %>% drop_na()


## Regress Indicators + Dummies on Bonds ---------------------------------------------


# Define a list to hold regression results
regression_results_list <- list()

# Define the names of the yield change columns 
yield_change_columns <- grep("^Change_", names(bond_data_combined), value = TRUE)

# Loop over each item in the economic data list
for (i in seq_along(economic_data)) {
  # Get the current economic data frame
  econ_df <- economic_data[[i]]
  econ_name <- names(economic_data)[i]
  
  # Only keep the rows with non-NA 'Mean_Adjusted_Surprise' values
  econ_df <- filter(econ_df, !is.na(Mean_Adjusted_Surprise))
  
  # Filter the economic data to include only rows where the release date matches the bond data dates
  econ_df <- econ_df %>% filter(`Release Date` %in% bond_data_combined$DATE)
  
  # If there are no matching dates, skip this iteration
  if (nrow(econ_df) == 0) {
    next
  }
  
  # Add day of the week as a factor
  econ_df$DayOfWeek <- factor(weekdays(econ_df$`Release Date`))
  
  # Merge the economic data with bond data on matching dates
  merged_data <- merge(econ_df, bond_data_combined, by.x = 'Release Date', by.y = 'DATE', all.x = TRUE)
  
  # Perform regressions using the merged data columns containing 
  # the change in each yield, day of week dummy, and mean_adjusted_surprise
  for (yield_change_column in yield_change_columns) {
    # Define the regression formula
    formula <- as.formula(paste0(yield_change_column, " ~ Mean_Adjusted_Surprise + DayOfWeek"))
    
    # Run the regression
    regression_result <- lm(formula, data = merged_data)
    
    # Get the summary of the model
    regression_summary <- summary(regression_result)
    
    # Calculate White's standard errors
    robust_se <- vcovHC(regression_result, type = "HC1")
    
    # Replace standard errors in the summary object
    regression_summary$coefficients[, "Std. Error"] <- sqrt(diag(robust_se))
    
    # Store the updated summary object
    regression_results_list[[paste(econ_name, yield_change_column, sep = "_")]] <- regression_summary
    
  }
}

lapply(regression_results_list, print)

## Output Indicator/Bond Regressions and Keep Significant Indicators ---------------------------------------


# Output the regression summaries
# Initialize a dataframe to store the results
regression_output <- data.frame(
  Indicator = character(),
  Estimate = numeric(),
  StdError = numeric(),
  AdjRSquared = numeric(),
  Star1 = character(),
  Star5 = character(),
  Star10 = character(),
  stringsAsFactors = FALSE
)

# Helper function to determine the significance stars
get_significance_stars <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.10) {
    return("*")
  } else {
    return("")
  }
}

# Loop through each regression result and extract the needed information
for (name in names(regression_results_list)) {
  
  # Get the regression summary
  regression_summary <- regression_results_list[[name]]
  
  # Extract the coefficient for Mean_Adjusted_Surprise
  coef_info <- coef(regression_summary)["Mean_Adjusted_Surprise", ]
  
  # Extract the estimate, std. error, p-value, and adj. r squared
  estimate <- coef_info["Estimate"]
  std_error <- coef_info["Std. Error"]
  p_value <- coef_info["Pr(>|t|)"]
  
  # Get significance stars
  stars <- get_significance_stars(p_value)
  # Extract the adjusted R-squared value
  adj_r_squared <- regression_summary$adj.r.squared
  
  # Create a new row for the output dataframe
  new_row <- data.frame(
    Indicator = name,
    Estimate = estimate,
    StdError = std_error,
    AdjRSquared = adj_r_squared,
    Significance = stars
  )
  
  # Bind the new row to the regression output dataframe
  regression_output <- rbind(regression_output, new_row)
}

# Print the formatted regression output
print(regression_output)

# Write the output to a CSV file
write.csv(regression_output, "regression_output.csv", row.names = FALSE)

# Extract the maturity part from the 'Indicator' column
regression_output$Maturity <- sub(".*_(DGS\\d+).*", "\\1", regression_output$Indicator)

# Now we map the maturity codes to more readable formats
maturity_map <- c("DGS3" = "3M", "DGS1" = "1Y", "DGS2" = "2Y", "DGS5" = "5Y", "DGS10" = "10Y", "DGS30" = "30Y")
regression_output$Maturity <- maturity_map[regression_output$Maturity]

# Now calculate the average Adjusted R Squared for each maturity
average_adj_r_squared <- aggregate(AdjRSquared ~ Maturity, data = regression_output, FUN = base::mean)

# Order the Maturity levels from lowest to highest:
average_adj_r_squared$Maturity <- factor(average_adj_r_squared$Maturity, levels = c("3M", "1Y", "2Y", "5Y", "10Y", "30Y"))
average_adj_r_squared <- average_adj_r_squared[order(average_adj_r_squared$Maturity), ]

# Print the result
print(average_adj_r_squared)

# Save to a CSV file:
write.csv(average_adj_r_squared, "average_adj_r_squared.csv", row.names = FALSE)

## drop indicators that are not significant
remove_indicators <- function(data_list, indicators_to_remove) {
  # Keep elements whose names are not in 'indicators_to_remove'
  filtered_data_list <- data_list[!names(data_list) %in% indicators_to_remove]
  # Remove NULL elements
  filtered_data_list <- filtered_data_list[sapply(filtered_data_list, function(x) !is.null(x))]
  return(filtered_data_list)
}

# Define the indicators to be removed
indicators_to_remove <- c("GDP", "Consumer_Credit", "PPI", "Unemployment", 
                          "Trade_Balance", "Durable_Goods_Orders", "Leading_Index")

# Apply the function to economic_data
economic_data_significant <- remove_indicators(economic_data, indicators_to_remove)

## Import/Convert Futures, Calculate Daily Change, and Plot ----------------------

## Add in futures and calculate daily change

# Add 3-month futures data
futures_data <- zq3m_data
futures_data$Date <- as.Date(futures_data$Date)

# Compute the prior day for joining with 4-month futures data
futures_data <- futures_data %>%
  mutate(prior_day = dplyr::lag(Date, n = 1, order_by = Date))

# Add price for 3m future on previous day 
futures_data <- futures_data %>%
  mutate(Price_3m_prior = dplyr::lag(Price, n = 1, order_by = Date))
# Merge with 4-month futures data for the prior day, then filter NA
futures_data <- left_join(futures_data, zq4m_data, by = "prior_day", suffix = c("_3m", "_4m_prior"))

futures_data <- filter(futures_data, !is.na(Price_4m_prior))

# Add a column that identifies the first day in the dataset for each month
futures_data <- futures_data %>%
  arrange(Date_3m) %>%
  group_by(year_3m, month_3m) %>%
  mutate(first_day_flag = row_number() == 1) %>%
  ungroup()

# Now use this flag to decide which price to use
futures_data <- futures_data %>%
  mutate(futures_rate = 100 - Price_3m,
         futures_rate_prior = ifelse(first_day_flag, 100 - Price_4m_prior, 100 - Price_3m_prior)) # given rebalancing on day 1

# Calculate daily changes in basis points
futures_data <- futures_data %>%
  mutate(Change = (futures_rate - futures_rate_prior)*100)  # convert to bps

summary(futures_data$Change)

## Regress Significant Indicators + Dummies on Futures -------------

# Define a list to hold regression results
regression_results_list_significant <- list()

# Loop over each significant economic indicator data frame
for (i in seq_along(economic_data_significant)) {
  econ_df <- economic_data_significant[[i]]
  econ_name <- names(economic_data_significant)[i]
  
  # Only keep the rows with non-NA 'Mean_Adjusted_Surprise' values
  econ_df <- filter(econ_df, !is.na(Mean_Adjusted_Surprise))
  
  # Filter the economic data to include only rows where the release date matches the futures data dates
  econ_df <- econ_df %>% filter(`Release Date` %in% futures_data$Date_3m)
  
  # If there are no matching dates, skip this iteration
  if (nrow(econ_df) == 0) {
    next
  }
  
  # Add day of the week as a factor
  econ_df$DayOfWeek <- factor(weekdays(econ_df$`Release Date`))
  
  # Merge the economic data with bond data on matching dates
  regression_data <- merge(econ_df, futures_data, by.x = 'Release Date', by.y = 'Date_3m', all.x = TRUE)
  
  # Define the regression formula
  formula <- as.formula(paste("Change ~ Mean_Adjusted_Surprise + DayOfWeek"))
  
  # Run the regression
  regression_result <- lm(formula, data = regression_data)
  
  # Get the summary of the model
  regression_summary <- summary(regression_result)
  
  # Calculate White's standard errors
  robust_se <- vcovHC(regression_result, type = "HC1")
  
  # Replace standard errors in the summary object
  regression_summary$coefficients[, "Std. Error"] <- sqrt(diag(robust_se))
  
  # Store the updated summary object
  regression_results_list_significant[[paste(econ_name, "Futures_Change", sep = "_")]] <- regression_summary
  
}

# Print the regression summaries
lapply(regression_results_list_significant, function(x) {
  cat("Regression results for:", names(x), "\n")
  print(x)
})


## Regress Significant Indicators + Futures + Dummies on Bonds -------------

# Prepare a dataframe that merges futures_data with bond_data_combined by date
data_for_regression <- left_join(futures_data, bond_data_combined, by = c("Date_3m"="DATE" ))

# Loop over each item in the filtered economic data list
for (i in seq_along(economic_data_significant)) {
  econ_df <- economic_data_significant[[i]]
  econ_name <- names(economic_data_significant)[i]
  
  # Filter the economic data to include only rows with non-NA 'Mean_Adjusted_Surprise' values
  econ_df <- filter(econ_df, !is.na(Mean_Adjusted_Surprise))
  
  # Filter the economic data to include only rows where the release date matches the bond data dates
  econ_df <- filter(econ_df, `Release Date` %in% data_for_regression$Date_3m)
  
  # Skip this iteration if there are no matching dates
  if (nrow(econ_df) == 0) {
    next
  }
  
  # Add day of the week as a factor
  econ_df$DayOfWeek <- factor(weekdays(econ_df$`Release Date`))
  
  # Merge the economic data with the bond and futures data on matching dates
  merged_data <- merge(econ_df, data_for_regression, by.x = 'Release Date', by.y = 'Date_3m', all.x = TRUE)
  
  # Perform regressions using the merged data columns containing the change in each yield, day of week dummy, 
  # mean_adjusted_surprise, and daily change in Federal funds futures
  for (yield_change_column in yield_change_columns) {
    # Define the regression formula
    formula <- as.formula(paste0(yield_change_column, " ~ Mean_Adjusted_Surprise + DayOfWeek + Change"))
    
    # Run the regression
    regression_result <- lm(formula, data = merged_data)
    
    # Get the summary of the model
    regression_summary <- summary(regression_result)
    
    # Calculate White's standard errors
    robust_se <- vcovHC(regression_result, type = "HC1")
    
    # Replace standard errors in the summary object
    regression_summary$coefficients[, "Std. Error"] <- sqrt(diag(robust_se))
    
    # Store the updated summary object
    regression_results_list_significant[[paste(econ_name, "Futures", yield_change_column, sep = "_")]] <- regression_summary
    
  }
}

## Output Regression of Significant Indicators + Futures + Dummies  --------


# Initialize a dataframe to store the results
regression_output_significant <- data.frame(
  Indicator = character(),
  Estimate = numeric(),
  StdError = numeric(),
  AdjRSquared = numeric(),
  Star1 = character(),
  Star5 = character(),
  Star10 = character(),
  stringsAsFactors = FALSE
)

# Loop through each regression result and extract the needed information
for (name in names(regression_results_list_significant)) {
  
  # Get the regression summary
  regression_summary <- regression_results_list_significant[[name]]
  
  # Extract the coefficient for Mean_Adjusted_Surprise
  coef_info <- coef(regression_summary)["Mean_Adjusted_Surprise", ]
  
  # Extract the estimate, std. error, p-value, and adj. r squared
  estimate <- coef_info["Estimate"]
  std_error <- coef_info["Std. Error"]
  p_value <- coef_info["Pr(>|t|)"]
  
  # Get significance stars
  stars <- get_significance_stars(p_value)
  
  # Extract the adjusted R-squared value
  adj_r_squared <- regression_summary$adj.r.squared
  
  # Create a new row for the output dataframe
  new_row <- data.frame(
    Indicator = name,
    Estimate = estimate,
    StdError = std_error,
    AdjRSquared = adj_r_squared,
    Significance = stars
  )
  
  # Bind the new row to the regression output dataframe
  regression_output_significant <- rbind(regression_output_significant, new_row)
}

# Print the formatted regression output
print(regression_output_significant)

# Write the output to a CSV file
write.csv(regression_output_significant, "regression_output_significant.csv", row.names = FALSE)

# Extract the maturity part from the 'Indicator' column
regression_output_significant$Maturity <- ifelse(grepl("DGS", regression_output_significant$Indicator), 
                                                 sub(".*_(DGS\\d+).*", "\\1", regression_output_significant$Indicator),
                                                 "Futures")

# Now we map the maturity codes to more readable formats
maturity_map <- c("DGS3" = "3M", "DGS1" = "1Y", "DGS2" = "2Y", "DGS5" = "5Y", 
                  "DGS10" = "10Y", "DGS30" = "30Y", "Futures" = "Futures")
regression_output_significant$Maturity <- maturity_map[regression_output_significant$Maturity]

# Now calculate the average Adjusted R Squared for each maturity
average_adj_r_squared_significant <- aggregate(AdjRSquared ~ Maturity, data = regression_output_significant, FUN = base::mean)

# Order the Maturity levels from lowest to highest:
average_adj_r_squared_significant$Maturity <- factor(average_adj_r_squared_significant$Maturity, 
                                                     levels = c("Futures", "3M", "1Y", "2Y", "5Y", "10Y", "30Y"))
average_adj_r_squared_significant <- average_adj_r_squared_significant[order(average_adj_r_squared_significant$Maturity), ]

# Print the result
print(average_adj_r_squared_significant)

# Save to a CSV file:
write.csv(average_adj_r_squared_significant, "average_adj_r_squared_significant.csv", row.names = FALSE)


## Visual plots to compare Impact of Controlling for Expectations --------

# Function to extract estimates for plotting
extract_estimates <- function(regression_df, indicator_prefix) {
  # Subset the data frame to only the rows for the specified indicator
  indicator_df <- subset(regression_df, grepl(indicator_prefix, regression_df$Indicator))
  # Extract the estimates and reshape for plotting
  estimates <- data.frame(
    Maturity = c("3MO", "1Y", "2Y", "5Y", "10Y", "30Y"),
    Estimate = as.numeric(indicator_df$Estimate)
  )
  return(estimates)
}

# Get the list of unique economic indicators from the 'Indicator' column, excluding the maturity and 'Futures' parts
unique_indicators <- unique(gsub("_Futures_Change.*", "", regression_output_significant$Indicator))
unique_indicators
# Loop over each economic indicator
for(indicator in unique_indicators) {
  
  # Extract the estimates for the current indicator with and without futures
  estimates_with_futures <- extract_estimates(regression_output_significant, paste(indicator, "_Futures_Change_DGS", sep = ""))
  estimates_without_futures <- extract_estimates(regression_output, paste(indicator, "_Change_", sep = ""))
  
  # Combine the estimates into one data frame for plotting
  estimates <- data.frame(
    Maturity = rep(c("3M", "1Y", "2Y", "5Y", "10Y", "30Y"), 2),
    Estimate = c(estimates_with_futures$Estimate, estimates_without_futures$Estimate),
    Type = rep(c("With Futures", "Without Futures"), each = 6)
  )
  
  # Factor the Maturity levels in the order from lowest to highest
  estimates$Maturity <- factor(estimates$Maturity, levels = c("3M", "1Y", "2Y", "5Y", "10Y", "30Y"))
  
  # Replace underscore with a space for the title
  formatted_indicator <- gsub("_", " ", indicator)
  
  # Plot the estimates
  p <- ggplot(estimates, aes(x = Maturity, y = Estimate, group = Type, colour = Type)) +
    geom_line() +
    geom_point() +
    labs(title = paste(formatted_indicator),
         y = "Estimate") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_colour_manual(values = c("With Futures" = "blue", "Without Futures" = "red"))
  
  # Display the plot
  print(p)
}

