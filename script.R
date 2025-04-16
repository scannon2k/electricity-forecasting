# required packages
library(tidyverse)
library(janitor)
library(lubridate)
library(fpp3)
library(kableExtra)
library(sf)
library(cowplot)
library(leaflet)

# optional packages
#library(tidylog)

# lat/long field 
options(scipen = 999, digits=8)

# read in the Gainesville electric consumption data for years 2012 - 2022
gru_data <- readr::read_csv('GRU_Customer_Electric_Consumption_2012-2022_20250327.csv') %>%
  janitor::clean_names() %>%
  filter(year %in% 2012:2022)

# do some data cleaning
elec_cons_data <- gru_data %>%
  # remove ALACHUA and NEWBERRY records 
  filter(service_city == "GAINESVILLE") %>%
  # remove the single record missing address
  filter(!is.na(service_address)) %>%
  # remove records with kwh consumption > 5000000
  # this filter removes a single record - the top kwh consumption value in the dataset,
  # which was visually inspected as bad data
  filter(kwh_consumption < 5000000) %>%
  # get date variable in ISO date format and create a new year/month variable
  mutate(date2 = as.Date(date, format = "%m/%d/%Y"),
         month2 = zoo::as.yearmon(date2, "%Y %m")) %>%
  # drop state, service_city, and old date vars
  select(!c('state','service_city','year','month','date')) %>%
  rename(date = date2,
         month = month2) %>%
  # reorder columns and sort by month ascending
  select(month, date, service_address, kwh_consumption, latitude, longitude, geo) %>%
  arrange(month)

# look at the count of missing values for each field
elec_cons_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "var",
               values_to = "value") %>%
  mutate(misspct = value / nrow(elec_cons_data)) %>%
  mutate(across(value, scales::label_comma())) %>%
  mutate(across(misspct, scales::label_percent())) %>%
  kbl(col.names = c("Variable", "Missing Count", "Missing Percent"), align = "ccc",
      table.attr = 'data-quarto-disable-processing="true"') %>% 
  kable_styling(bootstrap_options = c("striped", "hover",
                                      "condensed", "responsive"), full_width = F)

# look at the count of unique values for each field
elec_cons_data %>%
  summarise(across(everything(), ~ n_distinct(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "var",
               values_to = "value") %>%
  mutate(across(value, scales::label_comma())) %>%
  kbl(col.names = c("Variable", "Unique Values"), align = "cc",
      table.attr = 'data-quarto-disable-processing="true"') %>% 
  kable_styling(bootstrap_options = c("striped", "hover",
                                      "condensed", "responsive"), full_width = F)

# most frequently occuring value in geo field
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-82.325024, lat=29.651958, popup = "Main Street and University Avenue")

duplicates <- elec_cons_data %>%
   group_by(date, service_address) %>%
   filter(n() > 1) %>%
   ungroup()

# if there are multiple entries in a particular date, sum them
# other options - keep the largest, keep the smallest, etc.
# this method we keep all information, and we also dont know what is correct vs incorrect
elec_data_dedup <- elec_cons_data %>%
  group_by(date, service_address) %>%
  summarize(rollup_kwh = sum(kwh_consumption)) %>%
  mutate(month = zoo::as.yearmon(date, "%Y %m"),
         month_name = month(month, label = TRUE, abbr = FALSE),
         year = year(month)) %>%
  arrange(month)

# boot barn on archer rd is part of the duplicates
elec_cons_data %>%
  filter(service_address=='3638 SW ARCHER RD') %>%
  select(month, date, service_address, kwh_consumption) %>%
  head(6)

# take the sum of each month to get total kwh consumption for archer rd boot barn
elec_data_dedup %>%
  filter(service_address=='3638 SW ARCHER RD') %>%
  select(month, date, service_address, rollup_kwh) %>%
  head(3)

# Summary statistics for ungrouped data
# cat("Summary statistics for rollup_kwh variable:\n")
# cat("\nMean of rollup_kwh:", mean(elec_data_dedup$rollup_kwh), "\n")
# cat("Median of rollup_kwh:", median(elec_data_dedup$rollup_kwh), "\n")
# cat("Minimum value of rollup_kwh:", min(elec_data_dedup$rollup_kwh), "\n")
# cat("Maximum value of rollup_kwh:", max(elec_data_dedup$rollup_kwh), "\n")
# cat("Standard deviation of rollup_kwh:", sd(elec_data_dedup$rollup_kwh), "\n")
# cat("Quantiles of rollup_kwh:")
# quantile(elec_data_dedup$rollup_kwh)

# look at the summary stats by month
elec_data_dedup %>%
  group_by(month_name) %>%
  summarise(
    mean_kwh_by_month = mean(rollup_kwh),
    med_kwh_by_month = median(rollup_kwh),
    sd_kwh_by_month = sd(rollup_kwh)) %>%
  mutate(across(mean_kwh_by_month:sd_kwh_by_month, scales::label_comma())) %>%
  kbl(col.names = c("Month", "Mean","Median","Standard Deviation"), align = "cccc",
      table.attr = 'data-quarto-disable-processing="true"') %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)

# shape of the curve changes in the summer months May - Sept
elec_data_dedup %>%
  ggplot(aes(x = rollup_kwh)) +
  geom_histogram(binwidth = 100, fill = "goldenrod2", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(0,5000),
                     breaks = seq(0,5000,by=1000),
                     labels = c('','1k','2k','3k','4k','5k')) +
  scale_y_continuous(labels = scales::unit_format(unit = "k", scale = 1e-3)) +
  labs(title = "Distribution of Gainesville Property-Level kWh Consumption by Month",
       x = "kWh Consumption",
       y = "Count",
       caption = "This chart has been limited to observations with less than 5000 kWh consumption values.") +
  facet_wrap(~ month_name) +
  theme_minimal()

# aggregate the data by month and convert to gWh
elec_data_agg <- elec_data_dedup %>%
  group_by(month) %>%
  summarize(mthly_kwh_sum = sum(rollup_kwh)) %>%
  mutate(mthly_gwh_sum = mthly_kwh_sum / 1000000,
         newyrmon = tsibble::yearmonth(month)) %>%
  select(!month) %>%
  as_tsibble(index = newyrmon) %>%
  rename(month = newyrmon) %>%
  select(month, mthly_kwh_sum, mthly_gwh_sum) %>%
  arrange(month)

elec_data_agg %>% 
  autoplot(mthly_gwh_sum, size=.75,color="#04052E") +
  geom_point() +
  labs(title="Gainesville Monthly Electricity Consumption (gWh)",
       x="Month",
       y='gWh') +
  theme_minimal()

# elec_data_agg %>%
#   gg_subseries(mthly_gwh_sum) +
#   labs(title = "",
#        x="",
#        y='') +
#   theme_minimal() +
#   theme(axis.text.x=element_text(angle=-90))

elec_data_agg %>%
  gg_season(mthly_gwh_sum) +
  labs(title = "Seasonal Pattern in Gainesville Electricity Consumption",
       x="Month",
       y='gWh') +
  theme_minimal()

elec_data_agg %>%
  ACF(mthly_gwh_sum, lag_max = 24) %>%
  autoplot() + 
  labs(title="Autocorrelation Function for Electricity Consumption") +
  theme_minimal()

dcmp <- elec_data_agg %>%
  model(stl = STL(mthly_gwh_sum ~
                    trend(window = 9) +
                    season(window = "periodic"),
                  robust = TRUE))

components(dcmp) %>%
  autoplot() +
  labs(title = 'STL Decomposition for Monthly gWh Consumption',
       x = "Month") +
  theme_minimal()

components(dcmp) %>%
  as_tsibble() %>%
  autoplot(mthly_gwh_sum, color="darkgrey") +
  geom_line(aes(y=trend), size=.75,color = "#ff006d") +
  labs(title = "Trend in Electricity Consumption",
       x="Month",
       y='gWh') +
  theme_minimal()

# check for a regular unit root
elec_data_agg %>%
  features(mthly_gwh_sum, unitroot_ndiffs)

# check for a seasonal unit root
elec_data_agg %>%
  features(mthly_gwh_sum, unitroot_nsdiffs)

# split into train test (9 years train, 2 years test)
train <- elec_data_agg %>% filter_index(~ "2020-12")
test <- elec_data_agg %>% filter_index("2021-01" ~ .)

# fit snaive and sarima models
fit_models <- train %>%
  model(SNaive = SNAIVE(mthly_gwh_sum),
        SARIMA = ARIMA(mthly_gwh_sum))

# view details of the two models
report(fit_models[1])
report(fit_models[2])

# forecast for the test period
elec_fc <- forecast(fit_models, new_data = test, level = 95)

# plot the forecasts with confidence intervals
elec_fc %>%
  autoplot(train, size=.75, level = c(95, 80)) +
  labs(title="Forecasts: Seasonal Naïve and SARIMA Models",
       x="Month",
       y='gWh') +
  theme_minimal()

# in-sample accuracy
accuracy(fit_models)

# out-of-sample accuracy
accuracy(elec_fc, test)
