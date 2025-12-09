## ############################################################################
##
## LONDON SCHOOL OF HYGIENE AND TROPICAL MEDICINE (LSHTM)
## preproc: Processing Vietnam forecasts and observed data into dataframe for performance evaluation, generating random and seasonal baselines for comparison
##
## ############################################################################
##

# setwd("C:/Users/AmyCampbell/Documents/DMOSS/PerformanceEvaluationPaper/Analysis")

library(dplyr)
library(lubridate)
library(tidyr)


#############################################
#1. Pre-process forecasts
#############################################

#Input observed dengue cases from latest data update Otcober 2022 (currently in Vietnam_provinces folder)
observed <- read.csv("Data/Vietnam_province/20221001/observed/observed_dengue_cases_02_19_20221025.csv")
observed <- na.omit(observed)  
observed$tsdatetime <- as.Date(observed$tsdatetime, format = "%Y-%m-%d")
observed$YearMonth <- floor_date(observed$tsdatetime, "month")



#Make observed cases a population weighted incidence
population <- read.csv("Data/Vietnam_province/20221001/observed/annual/observed_population_02_23_20221025.csv")
population$pop <- population$tsvalue
population$tsdatetime <- as.Date(population$tsdatetime, format = "%Y-%m-%d")
population$year <- format(population$tsdatetime, "%Y")
observed$year <- format(observed$tsdatetime, "%Y")
observed <- merge(observed, population[, c("areaid", "year", "pop")], by = c("areaid", "year"))
#Make  new columns: observed_incidence for incidence per 100,000 population
observed$observed <- (observed$tsvalue / observed$pop) * 100000
write.csv(observed, "Scripts_Draft2/Outputs/Vietnam_observed_incidence.csv")


#Clip to forecast period
observed <- observed %>%
  filter(tsdatetime >= as.Date("2019-07-01") & tsdatetime <= as.Date("2022-09-30"))

#Input D-MOSS forecasted cases
forecastfolder_path <- "Data/Vietnam_forecasts/"
forecast_csvs <- list.files(
  path = forecastfolder_path,  # Current folder
  pattern = "^predicted_dengue_incidence_2.*\\.csv$",  
  full.names = TRUE,  # Return full file paths
  recursive = TRUE    # Search recursively through subdirectories
)
all_forecast <- list()
for (file in forecast_csvs) {
  df <- read.csv(file)
  df <- na.omit(df)  
  df$tsdatetime <- as.Date(df$tsdatetime, format = "%Y-%m-%d")
  df$YearMonth <- floor_date(df$tsdatetime, "month")
  df <- df[, !grepl("^tsstd_", colnames(df))]
  df <- df[df$parametername == "dengue_incidence", ]
  #Adding a column to indicate lead time of forecast
  start_month <- min(df$YearMonth)
  df <- df %>%
    mutate(LeadTime = as.integer(interval(start_month, YearMonth) / months(1)) + 1)
  #Reshape to make long- one entry for every ensemble
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("tsvalue_ensemble_"),
      names_to = "ensemble",
      values_to = "forecasted") %>%
    mutate(ensemble = as.integer(sub("tsvalue_ensemble_", "", ensemble)))
  all_forecast[[file]] <- df_long
}
forecasted <- bind_rows(all_forecast)
#Clip to forecast period
forecasted <- forecasted %>%
  filter(tsdatetime >= as.Date("2019-07-01") & tsdatetime <= as.Date("2022-09-30"))



# Merge with observed data (many-to-many as observed rows need to be duplicated for each ensemble forecast)
final_df <- left_join(observed, forecasted, by = c("areaid","tsdatetime", "YearMonth"), relationship="one-to-many")
final_df <- na.omit(final_df) 

#Add region column using look-up table
regions <- read.csv("Data/Shapefiles/Vietnam_province_shapefiles/Vietnam_province_lookuptable.csv")
final_df <- merge(final_df, regions[, c("areaid", "region")], by = "areaid")

# Final long dataframe format for scoringutils and later analysis
long_df <- final_df %>%
  select(
    areaid,
    region,
    tsdatetime,
    YearMonth,
    observed,
    forecasted,
    LeadTime,
    ensemble
  )


#Write
write.csv(long_df, "Scripts_Draft2/Outputs/Vietnam_DMOSS_forecasts.csv")



#############################################
#2a. Create baseline models for comparison: Random baseline
#############################################

set.seed(2108)  # For reproducibility

# Simulating a random predictor by sampling from observed values per province
forecast_random <- observed %>%
  group_by(areaid) %>%
  mutate(
    forecasted_random = sample(observed, size = n(), replace = TRUE),
  ) %>%
  ungroup()

#Add random lead times
forecast_random <- forecast_random %>%
  arrange(areaid, tsdatetime) %>%
  group_by(areaid) %>%
  mutate(
    LeadTime_1 = lead(forecasted_random),
    LeadTime_2 = lead(forecasted_random, n = 2),
    LeadTime_3 = lead(forecasted_random, n = 3),
    LeadTime_4 = lead(forecasted_random, n = 4),
    LeadTime_5 = lead(forecasted_random, n = 5),
    LeadTime_6 = lead(forecasted_random, n = 6)
  ) %>%
  select(-forecasted_random)%>%
  ungroup()

forecast_random_long <- forecast_random %>%
  pivot_longer(cols = starts_with("LeadTime"),
               names_to = "LeadTime",
               names_prefix = "LeadTime_",
               values_to = "forecasted_random") %>%
  mutate(LeadTime = as.integer(LeadTime))%>%
  select(
    areaid,
    tsdatetime,
    YearMonth,
    observed,
    forecasted_random,
    LeadTime
  )


#############################################
#2b. Create baseline models for comparison: Seasonal baseline
#############################################

observed$tsdatetime <- as.Date(observed$tsdatetime, format = "%Y-%m-%d")
observed$Month <- month(observed$tsdatetime)
observed$Year <- year(observed$tsdatetime)

monthly_rolling_averages <- observed %>%
  arrange(areaid, Year, Month) %>%  # Ensure proper ordering
  group_by(areaid, Month) %>%       # Group by province and month
  mutate(
    seasonal_expanding_avg = cummean(ifelse(is.na(observed), 0, observed))  # Expanding mean for the same month in prior years, count NAs as 0s
  ) %>%
  ungroup()


forecast_seasonal <- monthly_rolling_averages %>%
  arrange(areaid, tsdatetime) %>%
  group_by(areaid) %>%
  mutate(
    LeadTime_1 = lead(seasonal_expanding_avg),
    LeadTime_2 = lead(seasonal_expanding_avg, n = 2),
    LeadTime_3 = lead(seasonal_expanding_avg, n = 3),
    LeadTime_4 = lead(seasonal_expanding_avg, n = 4),
    LeadTime_5 = lead(seasonal_expanding_avg, n = 5),
    LeadTime_6 = lead(seasonal_expanding_avg, n = 6)
  ) %>%
  select(-seasonal_expanding_avg)%>%
  ungroup()

forecast_seasonal_long <- forecast_seasonal %>%
  pivot_longer(cols = starts_with("LeadTime"),
               names_to = "LeadTime",
               names_prefix = "LeadTime_",
               values_to = "forecasted_seasonal") %>%
  mutate(LeadTime = as.integer(LeadTime))%>%
  select(
    areaid,
    tsdatetime,
    YearMonth,
    observed,
    forecasted_seasonal,
    LeadTime
  )
  




#Combine baseline models 
null_models <- left_join(forecast_random_long, forecast_seasonal_long, by = c("areaid","tsdatetime", "YearMonth", "LeadTime")) %>%
  select(
    areaid,
    tsdatetime,
    YearMonth,
    LeadTime,
    observed = observed.x,
    forecasted_seasonal,
    forecasted_random)%>%
  mutate(lead_time = as.integer(LeadTime))

null_models <- merge(null_models, regions[, c("areaid", "region")], by = "areaid")


#############################################
#3. Create combined dataframe with D-MOSS forecasts (mean of seasonal climate forecast ensembles), observed, random predictor, 
#############################################

#Take mean across seasonal climate forecast ensembles
forecast_DMOSSmean <- long_df %>% 
  group_by(areaid, tsdatetime, region, YearMonth, LeadTime) %>%  
  summarise(
    forecasted_DMOSSmean = mean(forecasted),  # Mean across all seasonal climate forcast ensembles.
    .groups = "drop"
    ) %>%  
  select(-region)
  
#    .groups = "drop"

forecasts_baseline <- left_join(null_models, forecast_DMOSSmean, by = c("areaid","tsdatetime", "YearMonth", "LeadTime")) %>%
  select(
    areaid,
    region,
    tsdatetime,
    YearMonth,
    LeadTime,
    observed,
    forecasted_DMOSSmean,
    forecasted_seasonal,
    forecasted_random)
  
#Write
write.csv(forecasts_baseline, "Scripts_Draft2/Outputs/Vietnam_DMOSS_forecasts_withnullmodels.csv")



