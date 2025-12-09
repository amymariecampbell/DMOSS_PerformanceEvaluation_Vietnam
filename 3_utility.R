## ############################################################################
##
## LONDON SCHOOL OF HYGIENE AND TROPICAL MEDICINE (LSHTM)
## utility: Runs utility scenarios performance assessment
## ############################################################################
##

# setwd("C:/Users/AmyCampbell/Documents/DMOSS/PerformanceEvaluationPaper/Analysis")

# Using two files from observed: dengue cases, precipitation
# Using two files from forecasts:
# epidemic_thresholds : to assess whether the threshold was exceeded based on observed cases
# probability_exceeding_thresholds : to assess forecast accuracy 

library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(pROC)
library(ggplot2)
library(patchwork)
library(zoo)
library(caret)




###############################################################################################################
# 1. Extract threshold for every month and province id in 2020:
###############################################################################################################
thresholdfolder_path <- "Data/Vietnam_forecasts/"
threshold_csvs <- list.files(
  path = thresholdfolder_path,  # Current folder
  pattern = "^epidemic_thresholds_2.*\\.csv$",  
  full.names = TRUE, 
  recursive = TRUE
)

all_thresholds <- list()

# Loop through each file
for (file in threshold_csvs) {
  # Extract the date from the filename as a date feature 
  date <- str_extract(basename(file), "\\d{4}-\\d{2}-\\d{2}")
  
  # Read the file
  df <- read.csv(file)
  
  # Add date column
  df$date <- as.Date(date)
  
  # Select only the relevant columns
  df_selected <- df %>%
    select(date,
           areaid,
           month,
           epi.1d,
           epi.2d,
           epi.q75,
           epi.q95)
  
  # Append to list
  all_thresholds[[length(all_thresholds) + 1]] <- df_selected
}

# Combine all data frames
epidemicthresholds <- bind_rows(all_thresholds) %>%
  arrange(date, areaid, month)

#Clip to forecast period
epidemicthresholds <- epidemicthresholds %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2020-12-31"))

###############################################################################################################
# 2. Extract cases and create binary variable for if the observed values exceeded a threshold
###############################################################################################################
observed <- read.csv("Data/Vietnam_province/20221001/observed/observed_dengue_cases_02_19_20221025.csv")
observed <- na.omit(observed)  
observed$tsdatetime <- as.Date(observed$tsdatetime, format = "%Y-%m-%d")
observed$month <- month(observed$tsdatetime)
observed <- observed %>%
  filter(tsdatetime >= as.Date("2020-01-01") & tsdatetime <= as.Date("2020-12-31"))%>%
  arrange(areaid, month)%>%
  select(areaid, month, observed = tsvalue)



epidemicthresholds_observed <- epidemicthresholds %>%
  left_join(observed, by = c("areaid", "month"))


exceed_check <- function(observed, threshold) {
  if (is.na(observed) | is.na(threshold)) {return(NA)}
  if (observed > threshold) {return(1)}
  else if (observed == threshold && observed != 0) {return(1)}
  else {return(0)}
}


epidemicthresholds_exceeded <- epidemicthresholds_observed %>%
  rowwise() %>%
  mutate(
    epi.1d = exceed_check(observed, epi.1d),
    epi.2d = exceed_check(observed, epi.2d),
    epi.q75 = exceed_check(observed, epi.q75),
    epi.q95 = exceed_check(observed, epi.q95), 
    tsdatetime = date
  ) %>%
  ungroup()


###############################################################################################################
# 3. Extract outbreak threshold exceedance probabilities
###############################################################################################################
probabilityfolder_path <- "Data/Vietnam_forecasts/"
probability_csvs <- list.files(
  path = probabilityfolder_path,  # Current folder
  pattern = "^probability_exceeding_thresholds_2.*\\.csv$",  
  full.names = TRUE, 
  recursive = TRUE
)

all_probabilities <- list()

# Loop through each file
for (file in probability_csvs) {
  df <- read.csv(file)
  df$tsdatetime <- as.Date(df$tsdatetime, format = "%Y-%m-%d")
  df$month <- month(df$tsdatetime)
  df$YearMonth <- floor_date(df$tsdatetime, "month")
  start_month <- min(df$YearMonth)
  df <- df %>%
    mutate(LeadTime = as.integer(interval(start_month, YearMonth) / months(1)) + 1)
  df_selected <- df %>%
    select(tsdatetime,
           areaid,
           month,
           LeadTime,
           prob1sd,
           ll1sd,
           uu1sd,
           prob2sd,
           probq75,
           probq95)
  # Append to list
  all_probabilities[[length(all_probabilities) + 1]] <- df_selected
}

# Combine all data frames
probabilityexceedances <- bind_rows(all_probabilities)

#Clip to forecast period
probabilityexceedances <- probabilityexceedances %>%
  filter(tsdatetime >= as.Date("2020-01-01") & tsdatetime <= as.Date("2020-12-31"))


#Combine with epidemicthresholds_exceeded


thresholdexceedance_analysisdf <- probabilityexceedances %>%
  left_join(epidemicthresholds_exceeded, by = c("areaid", "tsdatetime", "month"))



###############################################################################################################
# 4. Subset for utility scenario 
###############################################################################################################


# Scenario 1
# Forecast horizon June to October
# For each province, work out how many months usually exceed the threshold in this period. 
# Give each province a binary score whether 2020 had more months than this (1, 0). 
#	Sum forecasted probabilities
observed <- read.csv("Data/Vietnam_province/20221001/observed/observed_dengue_cases_02_19_20221025.csv")
observed <- na.omit(observed)
observed$tsdatetime <- as.Date(observed$tsdatetime, format = "%Y-%m-%d")
observed$month <- month(observed$tsdatetime)
observed$year <- year(observed$tsdatetime)
observed <- observed %>%
  filter(month %in% 6:10) %>%
  select(areaid, year, month, observed = tsvalue)
epidemicthresholds_all <- epidemicthresholds %>%
  filter(month %in% 6:10) %>%
  left_join(observed, by = c("areaid", "month"))
exceed_check <- function(observed, threshold) {
  if (is.na(observed) | is.na(threshold)) return(NA)
  if (observed > threshold || (observed == threshold && observed != 0)) return(1)
  return(0)
}
epidemicthresholds_all <- epidemicthresholds_all %>%
  rowwise() %>%
  mutate(
    epi.1d = exceed_check(observed, epi.1d),
    epi.2d = exceed_check(observed, epi.2d),
    epi.q75 = exceed_check(observed, epi.q75),
    epi.q95 = exceed_check(observed, epi.q95)
  ) %>%
  ungroup()
average_exceedances <- epidemicthresholds_all %>%
  filter(year < 2020) %>%
  group_by(areaid) %>%
  summarise(
    avg_epi.1d = mean(epi.1d, na.rm = TRUE) * 5,
    avg_epi.2d = mean(epi.2d, na.rm = TRUE) * 5,
    avg_epi.q75 = mean(epi.q75, na.rm = TRUE) * 5,
    avg_epi.q95 = mean(epi.q95, na.rm = TRUE) * 5
  )
epidemicthresholds_exceeded <- epidemicthresholds_all %>%
  filter(year == 2020) %>%
  group_by(areaid) %>%
  summarise(
    count_epi.1d = sum(epi.1d, na.rm = TRUE),
    count_epi.2d = sum(epi.2d, na.rm = TRUE),
    count_epi.q75 = sum(epi.q75, na.rm = TRUE),
    count_epi.q95 = sum(epi.q95, na.rm = TRUE)
  )
epidemicthresholds_exceeded <- epidemicthresholds_exceeded %>%
  left_join(average_exceedances, by = "areaid") %>%
  mutate(
    epi.1d_more = ifelse(count_epi.1d > avg_epi.1d, 1, 0),
    epi.2d_more = ifelse(count_epi.2d > avg_epi.2d, 1, 0),
    epi.q75_more = ifelse(count_epi.q75 > avg_epi.q75, 1, 0),
    epi.q95_more = ifelse(count_epi.q95 > avg_epi.q95, 1, 0)
  )



scenario1 <- probabilityexceedances %>%
  filter(month %in% 6:10) %>%
  group_by(areaid, month, LeadTime) %>%
  summarise(
    avg_prob1sd = mean(prob1sd, na.rm = TRUE),
    avg_prob2sd = mean(prob2sd, na.rm = TRUE),
    avg_probq75 = mean(probq75, na.rm = TRUE),
    avg_probq95 = mean(probq95, na.rm = TRUE),
    .groups = "drop"
  )
scenario1_forecasts_april <- scenario1 %>%
  filter(month %in% 6:10 & LeadTime == (month - 4)) %>%
  group_by(areaid) %>%
  summarise(
    prob1sd = mean(avg_prob1sd, na.rm = TRUE),
    prob2sd = mean(avg_prob2sd, na.rm = TRUE),
    probq75 = mean(avg_probq75, na.rm = TRUE),
    probq95 = mean(avg_probq95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(epidemicthresholds_exceeded, by = "areaid")

scenario1_forecasts_may <- scenario1 %>%
  filter(month %in% 6:10 & LeadTime == (month - 5)) %>%
  group_by(areaid) %>%
  summarise(
    prob1sd = mean(avg_prob1sd, na.rm = TRUE),
    prob2sd = mean(avg_prob2sd, na.rm = TRUE),
    probq75 = mean(avg_probq75, na.rm = TRUE),
    probq95 = mean(avg_probq95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(epidemicthresholds_exceeded, by = "areaid")




# Scenario 2
# Forecast horizon June to October
# Does the average of this period exceed the outbreak threshold averages across this period?
# Where March-May precipitation exceeded a 5 year average 

#Select provinces with above-average temperature/rainfall at start of 'season' (March- May)
precip <- read.csv("Data/Vietnam_province/20221001/observed/observed_precipitation_amount_per_day_04_19_20221025.csv")
precip$tsdatetime <- as.Date(precip$tsdatetime, format = "%Y-%m-%d")
precip$year <- format(precip$tsdatetime, "%Y")
precip_mean <- precip %>%
  filter(month(tsdatetime) %in% 3:5) %>%  
  group_by(areaid, year) %>%
  summarise(mean3months = mean(tsvalue, na.rm = TRUE), .groups = 'drop')
avgsum_df <- precip_mean %>%
  group_by(areaid) %>%
  mutate(
    avg3months = mean(mean3months, na.rm = TRUE))   %>%
  ungroup() %>%
  filter(year == 2020)

scenario2_filter <- avgsum_df %>%
  filter(mean3months > avg3months)

scenario2_forecasts_april <- thresholdexceedance_analysisdf %>%
  semi_join(scenario2_filter, by = c("areaid"))%>%  
  filter(month(tsdatetime) %in% 6:10 & LeadTime == (month - 4))

scenario2_forecasts_may <- thresholdexceedance_analysisdf %>%
  semi_join(scenario2_filter, by = c("areaid"))%>%  
  filter(month(tsdatetime) %in% 6:10 & LeadTime == (month - 5))


# Scenario 3
# Forecast horizon June to October
# Where March-May saw consecutive increases in cases 

#Select provinces with a consecutive increase in cases from April to May 
observed <- read.csv("Data/Vietnam_province/20221001/observed/observed_dengue_cases_02_19_20221025.csv")
observed <- na.omit(observed)  
observed$tsdatetime <- as.Date(observed$tsdatetime, format = "%Y-%m-%d")
observed$month <- month(observed$tsdatetime)
observed <- observed %>%
  filter(tsdatetime >= as.Date("2020-01-01") & tsdatetime <= as.Date("2020-12-31"))%>%
  arrange(areaid, month)%>%
  select(areaid, month, observed = tsvalue)
scenario3_filter <- observed %>%
  filter(month %in% 3:5) %>%  # Keep only March, April, and May
  group_by(areaid) %>%
  summarise(
    march_value = observed[month == 3],  # Get March value
    april_value = observed[month == 4],  # Get April value
    may_value = observed[month == 5],  # Get May value
    .groups = "drop"
  ) %>%
  filter(april_value > march_value & may_value > april_value)

scenario3_forecasts_april <- thresholdexceedance_analysisdf %>%
  semi_join(scenario3_filter, by = c("areaid"))%>%  
  filter(month(tsdatetime) %in% 6:10 & LeadTime == (month - 4))

scenario3_forecasts_may <- thresholdexceedance_analysisdf %>%
  semi_join(scenario3_filter, by = c("areaid"))%>%  
  filter(month(tsdatetime) %in% 6:10 & LeadTime == (month - 5))





# Scenario 4
# Filter provinces with 3 consecutive months exceeding threshold between June and October (outbreak events)
# Observed: Binary metric of which month  (across 6:10) had the highest exceedance of the threshold
#	Forecasted: Probability of outbreak exceedance (expectation model should be more confident of exceedance when the outbreak is at the peak) 
# Question being asked: Did the forecast predict a high probability for the peak month within that outbreak period?
thresholds_jun_oct <- epidemicthresholds_observed %>%
  filter(month %in% 6:10, month(date) == month) %>%
  group_by(areaid, month) %>%
  arrange(areaid, date) %>%
  mutate(
    exceed_epi.1d_flag = if_else(observed > epi.1d, 1, 0),
    exceed_epi.2d_flag = if_else(observed > epi.2d , 1, 0),
    exceed_epi.q75_flag = if_else(observed > epi.q75 , 1, 0),
    exceed_epi.q95_flag = if_else(observed > epi.q95 , 1, 0)
  ) %>%
  mutate(
    consec_exceed_epi.1d_flag = rollapplyr(exceed_epi.1d_flag, width = 3, FUN = function(x) all(x == 1), fill = NA, partial = TRUE),
    consec_exceed_epi.2d_flag = rollapplyr(exceed_epi.2d_flag, width = 3, FUN = function(x) all(x == 1), fill = NA, partial = TRUE),
    consec_exceed_epi.q75_flag = rollapplyr(exceed_epi.q75_flag, width = 3, FUN = function(x) all(x == 1), fill = NA, partial = TRUE),
    consec_exceed_epi.q95_flag = rollapplyr(exceed_epi.q95_flag, width = 3, FUN = function(x) all(x == 1), fill = NA, partial = TRUE)
  ) %>%
  select(areaid, month, consec_exceed_epi.1d_flag, consec_exceed_epi.2d_flag, consec_exceed_epi.q75_flag, consec_exceed_epi.q95_flag)  %>%
  ungroup()
observed_peaks <- epidemicthresholds_observed %>%
  merge(thresholds_jun_oct, by = c("areaid", "month")) %>%
  filter(month %in% 6:10, month(date) == month) %>%
  group_by(areaid,) %>%
  mutate(
    epi.1d_peak = observed - epi.1d,
    epi.2d_peak = observed - epi.2d,
    epi.q75_peak = observed - epi.q75,
    epi.q95_peak = observed - epi.q95,
  ) %>%
  mutate(
    epi.1d_peak = if_else(epi.1d_peak == max(epi.1d_peak, na.rm = TRUE), 1, 0),
    epi.2d_peak = if_else(epi.2d_peak == max(epi.2d_peak, na.rm = TRUE), 1, 0),
    epi.q75_peak = if_else(epi.q75_peak == max(epi.q75_peak, na.rm = TRUE), 1, 0),
    epi.q95_peak = if_else(epi.q95_peak == max(epi.q95_peak, na.rm = TRUE), 1, 0)
  )%>%
  select(areaid, month, tsdatetime = date, epi.1d_peak, epi.2d_peak, epi.q75_peak, epi.q95_peak)

scenario4_preproc <- observed_peaks %>%
  left_join(probabilityexceedances, by = c("areaid", "tsdatetime", "month"))
scenario4_preproc <- scenario4_preproc %>%
  left_join(thresholds_jun_oct, by = c("areaid", "month"))

scenario4_forecasts_april <- scenario4_preproc %>%
  filter(month(tsdatetime) %in% 6:10 & LeadTime == (month - 4))

scenario4_forecasts_may <- scenario4_preproc %>%
  filter(month(tsdatetime) %in% 6:10 & LeadTime == (month - 5))


###############################################################################################################
# 5. Export utility scenarios
###############################################################################################################

write.csv(scenario1_forecasts_april, "Scripts_Draft2/Outputs/Scenario1_Aprilforecast.csv")
write.csv(scenario1_forecasts_may, "Scripts_Draft2/Outputs/Scenario1_Mayforecast.csv")
write.csv(scenario2_forecasts_april, "Scripts_Draft2/Outputs/Scenario2_Aprilforecast.csv")
write.csv(scenario2_forecasts_may, "Scripts_Draft2/Outputs/Scenario2_Mayforecast.csv")
write.csv(scenario3_forecasts_april, "Scripts_Draft2/Outputs/Scenario3_Aprilforecast.csv")
write.csv(scenario3_forecasts_may, "Scripts_Draft2/Outputs/Scenario3_Mayforecast.csv")
write.csv(scenario4_forecasts_april, "Scripts_Draft2/Outputs/Scenario4_Aprilforecast.csv")
write.csv(scenario4_forecasts_may, "Scripts_Draft2/Outputs/Scenario4_Mayforecast.csv")
