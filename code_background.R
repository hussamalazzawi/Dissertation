rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(anytime)
library(zoo)

## background charts
# setup
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

# Read the data files
dgs1_df <- read_csv("DGS1.csv")
dgs5_df <- read_csv("DGS5.csv")
dgs10_df <- read_csv("DGS10.csv")
dfedtar_df <- read_csv("DFEDTAR.csv")

# Ensure that the 'DATE' column is in the Date format
dgs1_df$DATE <- as.Date(dgs1_df$DATE)
dgs5_df$DATE <- as.Date(dgs5_df$DATE)
dgs10_df$DATE <- as.Date(dgs10_df$DATE)
dfedtar_df$DATE <- anydate(dfedtar_df$DATE)

# Ensure that the data column is in numeric format
dgs1_df$DGS1 <- as.numeric(dgs1_df$DGS1)
dgs5_df$DGS5 <- as.numeric(dgs5_df$DGS5)
dgs10_df$DGS10 <- as.numeric(dgs10_df$DGS10)


# Plot 1985-2008 ----------------------------------------------------------


# Filter data to include only dates from 1/1/1985 to 12/15/2008
start_date <- ymd("1985-01-01")
end_date <- ymd("2008-12-15")

dgs1_df2 <- dgs1_df %>% filter(DATE >= start_date & DATE <= end_date)
dgs5_df2 <- dgs5_df %>% filter(DATE >= start_date & DATE <= end_date)
dgs10_df2 <- dgs10_df %>% filter(DATE >= start_date & DATE <= end_date)
dfedtar_df2 <- dfedtar_df %>% filter(DATE >= start_date & DATE <= end_date)

# Define the dates for vertical lines
vline_dates <- c("1987-01-05", "1989-03-21", "1993-10-15", "1998-07-29","1999-04-29", "2000-05-18", "2004-03-25", "2007-07-06")

# Plotting
ggplot() +
  geom_line(data = dgs1_df2, aes(x = DATE, y = DGS1, color = "1-Year Treasury Constant Maturity Rate")) +
  geom_line(data = dgs5_df2, aes(x = DATE, y = DGS5, color = "5-Year Treasury Constant Maturity Rate")) +
  geom_line(data = dgs10_df2, aes(x = DATE, y = DGS10, color = "10-Year Treasury Constant Maturity Rate")) +
  geom_line(data = dfedtar_df2, aes(x = DATE, y = DFEDTAR, color = "Federal Funds Target Rate")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", limits = c(start_date, end_date)) +
  scale_color_manual(values = c("1-Year Treasury Constant Maturity Rate" = "blue", 
                                "5-Year Treasury Constant Maturity Rate" = "purple",
                                "10-Year Treasury Constant Maturity Rate" = "red", 
                                "Federal Funds Target Rate" = "green"),
                     name = "Rate Type") +
  labs(title = "1-Year, 5-Year, and 10-Year Bond Yields and Federal Funds Target Rate (1/1/1985 to 12/15/2008)",
       x = "Time",
       y = "Percentage") +
  theme_minimal() +
  geom_vline(xintercept = as.Date(vline_dates), linetype="dashed", color = "black")  # Add vertical lines

ggsave("plot1985.png", plot = last_plot(), width = 11, height = 8, units = "in")

# Plot 2008-Present -------------------------------------------------------


# Read additional data files
dfedtaru_df <- read_csv("DFEDTARU.csv")
dfedtarl_df <- read_csv("DFEDTARL.csv")

# Ensure that the 'DATE' column is in the Date format
dfedtaru_df$DATE <- as.Date(dfedtaru_df$DATE)
dfedtarl_df$DATE <- as.Date(dfedtarl_df$DATE)

# Filter data to include only dates from 12/16/2008 to 1/25/2024
start_date <- ymd("2008-12-16")
end_date <- ymd("2024-01-25")

dgs1_df3 <- dgs1_df %>% filter(DATE >= start_date & DATE <= end_date)
dgs5_df3 <- dgs5_df %>% filter(DATE >= start_date & DATE <= end_date)
dgs10_df3 <- dgs10_df %>% filter(DATE >= start_date & DATE <= end_date)
dfedtaru_df3 <- dfedtaru_df %>% filter(DATE >= start_date & DATE <= end_date)
dfedtarl_df3 <- dfedtarl_df %>% filter(DATE >= start_date & DATE <= end_date)

# Define the dates for vertical lines
vline_dates2 <- c("2015-10-14", "2018-11-08", "2021-09-21", "2023-10-17")

# Plotting
ggplot() +
  geom_line(data = dgs1_df3, aes(x = DATE, y = DGS1, color = "1-Year Treasury Constant Maturity Rate")) +
  geom_line(data = dgs5_df3, aes(x = DATE, y = DGS5, color = "5-Year Treasury Constant Maturity Rate")) +
  geom_line(data = dgs10_df3, aes(x = DATE, y = DGS10, color = "10-Year Treasury Constant Maturity Rate")) +
  geom_line(data = dfedtaru_df3, aes(x = DATE, y = DFEDTARU, color = "Federal Funds Target Rate Upper")) +
  geom_line(data = dfedtarl_df3, aes(x = DATE, y = DFEDTARL, color = "Federal Funds Target Rate Lower")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("1-Year Treasury Constant Maturity Rate" = "blue", 
                                "5-Year Treasury Constant Maturity Rate" = "purple", 
                                "10-Year Treasury Constant Maturity Rate" = "red", 
                                "Federal Funds Target Rate Upper" = "green", 
                                "Federal Funds Target Rate Lower" = "orange"),
                     name = "Rate Type") +
  labs(title = "Bond Yields, Federal Funds Target Rate Upper & Lower Bounds (12/16/2008 to 1/25/2024)",
       x = "Time",
       y = "Percentage") +
  theme_minimal() +
  geom_vline(xintercept = as.Date(vline_dates2), linetype="dashed", color = "black")  # Add vertical lines

ggsave("plot2008.png", plot = last_plot(), width = 11, height = 8, units = "in")


## Show convergence of spot month futures to average effective Fed funds  --------

# Add spot-month futures data
futures_data <- read.csv("ZQ0M.csv")
futures_data$Date <- anydate(futures_data$Date)

# Calculate rate
futures_data <- futures_data %>%
  mutate(futures_rate = 100 - Price)

futures_data <- mutate(futures_data, day = as.numeric(day(Date)))

# Create column with futures days-to-expiration
futures_data <- futures_data %>%
  mutate(dte = day-as.numeric(days_in_month(`Date`)))

# Import and convert federal funds data
dff_data <- read_csv("DFF.csv")
dff_data$DATE <- anydate(dff_data$DATE)
dff_data <- dff_data %>% filter(DATE >= "1982-09-27")


# Calculate the cumulative average Fed funds rate for each day of each month
dff_cumulative_avg <- dff_data %>%
  group_by(year = year(DATE), month = month(DATE)) %>%
  arrange(DATE) %>%
  mutate(cumulative_avg = cummean(DFF)) %>%
  ungroup()

# Join the futures data with the DFF cumulative averages
futures_data <- futures_data %>%
  left_join(dff_cumulative_avg, by = c("Date" = "DATE"))

# Calculate the difference between the spot month futures rate and the cumulative average Fed funds rate
futures_data <- futures_data %>%
  mutate(rate_diff = (futures_rate - cumulative_avg) * 100)

# Plot the result
ggplot(futures_data, aes(x = dte, y = rate_diff)) +
  geom_point() +
  labs(title = "Spot month futures rate - cumulative average Fed funds rate",
       x = "Days till expiration",
       y = "Basis points") +
  theme_minimal()

# Save the plot
ggsave("futures_rate_diff_plot.png", width = 11, height = 8, units = "in")

## Show relation between Fed target and effective rate  --------

ggplot() +
  geom_line(data = dff_data, aes(x = DATE, y = DFF, color = "Fed Funds Rate")) +
  geom_line(data = dfedtar_df, aes(x = DATE, y = DFEDTAR, color = "Federal Funds Target Rate")) +
  geom_line(data = dfedtaru_df3, aes(x = DATE, y = DFEDTARU, color = "Federal Funds Target Rate Upper")) +
  geom_line(data = dfedtarl_df3, aes(x = DATE, y = DFEDTARL, color = "Federal Funds Target Rate Lower")) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_color_manual(values = c("Fed Funds Rate" = "black", 
                                "Federal Funds Target Rate" = "green", 
                                "Federal Funds Target Rate Upper" = "blue", 
                                "Federal Funds Target Rate Lower" = "orange"),
                     name = "Rate Type") +
  labs(title = "Federal Funds Rate and Target Rate",
       x = "Time",
       y = "Rate") +
  theme_minimal()

ggsave("Target_rate.png", plot = last_plot(), width = 11, height = 8, units = "in")
