## ############################################################################
##
## LONDON SCHOOL OF HYGIENE AND TROPICAL MEDICINE (LSHTM)
## metrics: Generates metrics for D-MOSS performance evaluation across 3 outcomes (raw incidence, trajectory, outbreak dynamics), and facilitates comparison to null models
##
## ############################################################################
##

# setwd("C:/Users/AmyCampbell/Documents/DMOSS/PerformanceEvaluationPaper/Analysis")

library(scoringutils)
library(dtw)
library(lubridate)
library(dplyr)



###############################
# Metric 1: Mean squared error (scoringutils) across lead times, months and provinces
###############################

data <- read.csv("Scripts_Draft2/Outputs/Vietnam_DMOSS_forecasts_withnullmodels.csv")
data$tsdatetime <- as.Date(data$tsdatetime, format = "%Y-%m-%d")
data$month <- month(data$tsdatetime)

forecast_DMOSS <- data |>
  as_forecast_point(
    forecast_unit = c("areaid", "tsdatetime", "LeadTime", "month"),
    observed= "observed",
    predicted= "forecasted_DMOSSmean"
  )
scores <- forecast_DMOSS |> 
  score()
scores <- scores %>% 
  mutate_if(is.numeric,round, digits = 3)
summary_DMOSS_leadtimes <- summarise_scores(scores, by = c("LeadTime")) %>% select (LeadTime, se_DMOSS = se_point)
summary_DMOSS_month_leadtimes <- summarise_scores(scores, by = c("month", "LeadTime")) %>% select (LeadTime, month, se_DMOSS = se_point)
summary_DMOSS_province_leadtimes <- summarise_scores(scores, by = c("areaid", "LeadTime"))%>% select (LeadTime, areaid, se_DMOSS = se_point)



forecast_random <- data |>
  as_forecast_point(
    forecast_unit = c("areaid", "tsdatetime", "LeadTime", "month"),
    observed= "observed",
    predicted= "forecasted_random"
  )
scores <- forecast_random |> 
  score()
scores <- scores %>% 
  mutate_if(is.numeric,round, digits = 3)
summary_random_leadtimes <- summarise_scores(scores, by = c("LeadTime")) %>% select (LeadTime, se_random = se_point)
summary_random_month_leadtimes <- summarise_scores(scores, by = c("month", "LeadTime"))%>% select (LeadTime, month, se_random = se_point)
summary_random_province_leadtimes <- summarise_scores(scores, by = c("areaid", "LeadTime"))%>% select (LeadTime, areaid, se_random = se_point)



forecast_seasonal <- data |>
  as_forecast_point(
    forecast_unit = c("areaid", "tsdatetime", "LeadTime", "month"),
    observed= "observed",
    predicted= "forecasted_seasonal"
  )
scores <- forecast_seasonal |> 
  score()
scores <- scores %>% 
  mutate_if(is.numeric,round, digits = 3)
summary_seasonal_leadtimes <- summarise_scores(scores, by = c( "LeadTime")) %>% select (LeadTime, se_seasonal = se_point)
summary_seasonal_month_leadtimes <- summarise_scores(scores, by = c("month", "LeadTime")) %>% select (LeadTime, month, se_seasonal = se_point)
summary_seasonal_province_leadtimes <- summarise_scores(scores, by = c("areaid", "LeadTime"))%>% select (LeadTime, areaid, se_seasonal = se_point)

#Merge
leadtimes_metric1 <-merge(summary_DMOSS_leadtimes, summary_random_leadtimes, by=c("LeadTime")) %>% merge (summary_seasonal_leadtimes, by=c("LeadTime"))
temporal_metric1 <-merge(summary_DMOSS_month_leadtimes, summary_random_month_leadtimes, by=c("month", "LeadTime")) %>% merge (summary_seasonal_month_leadtimes, by=c("month", "LeadTime"))
spatial_metric1 <-merge(summary_DMOSS_province_leadtimes, summary_random_province_leadtimes, by=c("areaid", "LeadTime")) %>% merge (summary_seasonal_province_leadtimes, by=c("areaid", "LeadTime"))



#Square root to get RMSE in same units
leadtimes_metric1_rmse <- leadtimes_metric1 %>%
  mutate(
    rmse_DMOSS = sqrt(se_DMOSS), 
    rmse_random  = sqrt(se_random ),
    rmse_seasonal  = sqrt(se_seasonal )
  ) %>% select (-se_DMOSS, -se_random, -se_seasonal)
temporal_metric1_rmse <- temporal_metric1 %>%
  mutate(
    rmse_DMOSS = sqrt(se_DMOSS), 
    rmse_random  = sqrt(se_random ),
    rmse_seasonal  = sqrt(se_seasonal )
  )%>% select (-se_DMOSS, -se_random, -se_seasonal)
spatial_metric1_rmse <- spatial_metric1 %>%
  mutate(
    rmse_DMOSS = sqrt(se_DMOSS), 
    rmse_random  = sqrt(se_random ),
    rmse_seasonal  = sqrt(se_seasonal )
  )%>% select (-se_DMOSS, -se_random, -se_seasonal)




###############################
# Metric 2: Incidence annual peak across lead times, months and provinces
###############################

data$year <- format(data$tsdatetime, "%Y")

# Identifying observed and forecasted peaks (timing of peak and max cases per year per areaid per ensemble)
peaks <- data %>%
  group_by(areaid, year, LeadTime) %>% #1 peak per province per year, averaged across ensembles
  #group_by(areaid, year, LeadTime, ensemble) %>% #1 peak per province per year for each ensemble
  summarise(
    observed_peak_time = tsdatetime[which.max(observed)],  # Peak time for observed data
    observed_peak_cases = max(observed),  # Peak cases for observed data
    forecasted_DMOSS_peak_time = tsdatetime[which.max(forecasted_DMOSSmean)], 
    forecasted_DMOSS_peak_cases = max(forecasted_DMOSSmean),  
    forecasted_random_peak_time = tsdatetime[which.max(forecasted_random)], 
    forecasted_random_peak_cases = max(forecasted_random),  
    forecasted_seasonal_peak_time = tsdatetime[which.max(forecasted_seasonal)],  
    forecasted_seasonal_peak_cases = max(forecasted_seasonal),  
  ) %>%
  mutate(
    temporal_distance_DMOSS = abs(forecasted_DMOSS_peak_time - observed_peak_time),  # Calculate temporal distance
    peak_difference_DMOSS = abs(forecasted_DMOSS_peak_cases - observed_peak_cases),  # Calculate case difference
    temporal_distance_random = abs(forecasted_random_peak_time - observed_peak_time), 
    peak_difference_random = abs(forecasted_random_peak_cases - observed_peak_cases),  
    temporal_distance_seasonal = abs(forecasted_seasonal_peak_time - observed_peak_time),  
    peak_difference_seasonal = abs(forecasted_seasonal_peak_cases - observed_peak_cases)  
  )  %>%
  mutate(temporal_distance_DMOSS = ceiling(signif(as.numeric(temporal_distance_DMOSS) / 30,3)),
         temporal_distance_random = ceiling(signif(as.numeric(temporal_distance_random) / 30, 3)),
         temporal_distance_seasonal = ceiling(signif(as.numeric(temporal_distance_seasonal) / 30, 3)))

leadtimes_metric2 <- peaks %>%
  group_by(LeadTime) %>%
  summarise(
    peakdiff_DMOSS = mean(temporal_distance_DMOSS, na.rm = TRUE),
    peakdiff_random = mean(temporal_distance_random, na.rm = TRUE),
    peakdiff_seasonal = mean(temporal_distance_seasonal, na.rm = TRUE),
    .groups = "drop"
  )

spatial_metric2 <- peaks %>%
  group_by(areaid, LeadTime) %>%
  summarise(
    peakdiff_DMOSS = mean(temporal_distance_DMOSS, na.rm = TRUE),
    peakdiff_random = mean(temporal_distance_random, na.rm = TRUE),
    peakdiff_seasonal = mean(temporal_distance_seasonal, na.rm = TRUE),
    .groups = "drop"
  )

peaks$month <- month(peaks$observed_peak_time)
temporal_metric2 <- peaks %>%
  group_by(month, LeadTime) %>%
  summarise(
    peakdiff_DMOSS = mean(temporal_distance_DMOSS, na.rm = TRUE),
    peakdiff_random = mean(temporal_distance_random, na.rm = TRUE),
    peakdiff_seasonal = mean(temporal_distance_seasonal, na.rm = TRUE),
    .groups = "drop"
  )

#Save dataframe for Figure 1
monthlypeaks <- peaks %>%
  group_by(areaid, year) %>%
  # Get only one peak per areaid/year: the one with the highest observed_peak_cases
  slice_max(order_by = observed_peak_cases, n = 1, with_ties = FALSE) %>%
  mutate(observed_peak_month = floor_date(observed_peak_time, "month")) %>%
  count(areaid, observed_peak_month, name = "num_observed_peaks") %>%
  mutate(month = month(observed_peak_month, label = TRUE, abbr = TRUE)) %>%
  group_by(areaid, month) %>%
  summarise(total_observed_peaks = sum(num_observed_peaks, na.rm = TRUE), .groups = "drop")
write.csv(monthlypeaks, "Scripts_Draft2/Outputs/monthlypeaks.csv", row.names = FALSE)

#Extract peaks for 2022 for supplemetary figure exploring June 2022 peak accuracy
peaks_2022 <- peaks %>%
  filter(year == 2022, LeadTime  == 1)  %>%
  group_by(areaid) %>%
  mutate(observed_peak_month = month(observed_peak_time),
         observed_June_peak = ifelse(observed_peak_month == 6, 1, 0),
         peak_2022 = month(forecasted_DMOSS_peak_time))  %>%
  select(areaid, observed_June_peak, peak_2022)
write.csv(peaks_2022, "Scripts_Draft2/Outputs/2022peaks_leadtime1.csv", row.names = FALSE)





###############################
# Metric 3: Trajectory (dynamic time warping) across lead times and provinces
###############################


# Ensemble mean of D-MOSS seasonal climate forecasts
trajectories <- data %>%
  group_by(areaid, tsdatetime, LeadTime) %>%
  summarise(mean_forecast = mean(forecasted_DMOSSmean), observed = mean(observed), .groups = "drop") %>%
  na.omit()  

dtw_results <- trajectories %>%
  group_by(areaid, LeadTime) %>%
  summarize(distance = {
    dtw_result <- dtw(observed, mean_forecast, keep = TRUE)
    dtw_result$distance
  })

#Summarise across areas to see which lead times best match trajectories 
dtw_results_leadtimes_DMOSS <- dtw_results %>%
  group_by(LeadTime) %>%
  summarise(trajectorydistance_DMOSS = mean(distance))

#Summarise across lead times to see where trajectories are best being matched
dtw_results_provinces_DMOSS <- dtw_results %>%
  group_by(areaid, LeadTime) %>%
  summarise(trajectorydistance_DMOSS = mean(distance))


#Random
trajectories <- data %>%
  group_by(areaid, tsdatetime, LeadTime) %>%
  summarise(mean_forecast = mean(forecasted_random), observed = mean(observed), .groups = "drop") %>%
  na.omit()  

dtw_results <- trajectories %>%
  group_by(areaid, LeadTime) %>%
  summarize(distance = {
    dtw_result <- dtw(observed, mean_forecast, keep = TRUE)
    dtw_result$distance
  })

#Summarise across areas to see which lead times best match trajectories 
dtw_results_leadtimes_random <- dtw_results %>%
  group_by(LeadTime) %>%
  summarise(trajectorydistance_random = mean(distance))

#Summarise across lead times to see where trajectories are best being matched
dtw_results_provinces_random <- dtw_results %>%
  group_by(areaid, LeadTime) %>%
  summarise(trajectorydistance_random = mean(distance))


#Seasonal
trajectories <- data %>%
  group_by(areaid, tsdatetime, LeadTime) %>%
  summarise(mean_forecast = mean(forecasted_seasonal), observed = mean(observed), .groups = "drop") %>%
  na.omit()  

dtw_results <- trajectories %>%
  group_by(areaid, LeadTime) %>%
  summarize(distance = {
    dtw_result <- dtw(observed, mean_forecast, keep = TRUE)
    dtw_result$distance
  })

#Summarise across areas to see which lead times best match trajectories 
dtw_results_leadtimes_seasonal <- dtw_results %>%
  group_by(LeadTime) %>%
  summarise(trajectorydistance_seasonal = mean(distance))

#Summarise across lead times to see where trajectories are best being matched
dtw_results_provinces_seasonal <- dtw_results %>%
  group_by(areaid, LeadTime) %>%
  summarise(trajectorydistance_seasonal = mean(distance))




#Merge
leadtimes_metric3 <-merge(dtw_results_leadtimes_DMOSS, dtw_results_leadtimes_random, by=c("LeadTime")) %>% merge (dtw_results_leadtimes_seasonal, by=c("LeadTime"))
spatial_metric3 <-merge(dtw_results_provinces_DMOSS, dtw_results_provinces_random, by=c("areaid", "LeadTime")) %>% merge (dtw_results_provinces_seasonal, by=c("areaid", "LeadTime"))






###############################
# Aggregate results across dimensions 
###############################


leadtimes <-merge(leadtimes_metric1_rmse, leadtimes_metric2, by=c("LeadTime")) %>% merge (leadtimes_metric3, by=c("LeadTime"))
spatial <- merge(spatial_metric1_rmse, spatial_metric2, by=c("areaid", "LeadTime")) %>% merge (spatial_metric3, by=c("areaid", "LeadTime"))
temporal <- left_join(temporal_metric1_rmse, temporal_metric2, by=c("month", "LeadTime"))




###############################
# Calculate value-added columns, difference between D-MOSS and seasonal forecast 
###############################


leadtimes_valueaddedseasonal <- leadtimes %>%
  mutate(
    rmse_diff= rmse_DMOSS - rmse_seasonal, 
    peakdiff_diff = peakdiff_DMOSS - peakdiff_seasonal, 
    trajectorydistance_diff = trajectorydistance_DMOSS - trajectorydistance_seasonal
  )


spatial_valueaddedseasonal <- spatial %>%
  mutate(
    rmse_diff= rmse_DMOSS - rmse_seasonal, 
    peakdiff_diff = peakdiff_DMOSS - peakdiff_seasonal, 
    trajectorydistance_diff = trajectorydistance_DMOSS - trajectorydistance_seasonal
  )

temporal_valueaddedseasonal <- temporal %>%
  mutate(
    rmse_diff= rmse_DMOSS - rmse_seasonal, 
    peakdiff_diff = peakdiff_DMOSS - peakdiff_seasonal
  )




##################
#Save to file 

write.csv(leadtimes_valueaddedseasonal, "Scripts_Draft2/Outputs/metrics_leadtimes.csv")
write.csv(spatial_valueaddedseasonal, "Scripts_Draft2/Outputs/metrics_spatial.csv")
write.csv(temporal_valueaddedseasonal, "Scripts_Draft2/Outputs/metrics_temporal.csv")

