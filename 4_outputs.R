## ############################################################################
##
## LONDON SCHOOL OF HYGIENE AND TROPICAL MEDICINE (LSHTM)
## outputs: Generating figures from analysis
##
## ############################################################################
##

# setwd("C:/Users/AmyCampbell/Documents/DMOSS/PerformanceEvaluationPaper/Analysis")
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(sf)
library(ggrepel)
library(tibble)
library(lubridate)
library(pROC)
library(caret)
library(patchwork)

#Set accessible colour schemes and themes

#Diverging two colour for value added graphs
diverging_palette= brewer.pal(n = 3, name = "RdBu")

theme_accessible <- theme_minimal(base_family = "sans", base_size = 12) +
  theme(
    axis.title = element_text(face = "bold", family = "sans", size = 12),
    axis.text  = element_text(face = "bold", family = "sans", size = 12),
    legend.title = element_text(face = "bold", family = "sans", size = 12),
    legend.text  = element_text(family = "sans", size = 12),
    strip.text   = element_text(face = "bold", family = "sans", size = 12),
    plot.title   = element_text(face = "bold", family = "sans", size = 12), 
    plot.tag = element_text(face = "bold", family = "sans", size = 12)
  )


y_labels <- c(
  "rmse" = "Root Mean Squared Error",
  "peakdiff" = "Peak Difference",
  "trajectorydistance" = "Trajectory Distance"
)

#Read in results 
leadtime_metrics <- read.csv("Scripts_Draft2/Outputs/metrics_leadtimes.csv")
spatial_metrics <- read.csv( "Scripts_Draft2/Outputs/metrics_spatial.csv")
temporal_metrics <- read.csv( "Scripts_Draft2/Outputs/metrics_temporal.csv")

#Read in extra files
incidence <- read.csv("Scripts_Draft2/Outputs/Vietnam_observed_incidence.csv")
provinces <- st_read('Data/Shapefiles/Vietnam_province_shapefiles/provinces.shp', stringsAsFactors = F, quiet = TRUE)
provinces$areaid <- provinces$provinceid
monthlypeaks <- read.csv("Scripts_Draft2/Outputs/monthlypeaks.csv")

#######################################################
#Figure 1: 
#######################################################

#Figure1a: Incidence
incidence$tsdatetime <- as.Date(incidence$tsdatetime, format = "%Y-%m-%d")
incidence$YearMonth <- floor_date(incidence$tsdatetime, "month")
incidence <- provinces %>%  left_join(incidence, by = "areaid")
incidence_forecastperiod <- incidence %>%
  filter(tsdatetime >= as.Date("2019-07-01") & tsdatetime <= as.Date("2022-09-30"))

figure1a <- ggplot(incidence_forecastperiod, aes(x = factor(YearMonth), y = factor(areaid), fill = observed)) +
  geom_tile() +
  scale_fill_gradient(
    low = "lightgrey", high = "darkgreen", trans = "sqrt",
    name = "Dengue \nIncidence\nper 100,000"
  ) +
  labs(x = "Time", y = "Province", tag="A") +
  scale_y_discrete(limits=rev, 
                   labels = setNames(incidence$provincena, incidence$areaid)
  ) +
  theme_accessible +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#Figure 1b: Peaks
monthlypeaks <- provinces %>%  left_join(monthlypeaks, by = "areaid")
monthlypeaks <- monthlypeaks %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  complete(areaid, month, fill = list(total_observed_peaks = 0))

figure1b <- ggplot(monthlypeaks, aes(x = month, y = factor(areaid), fill = factor(total_observed_peaks))) +
  geom_tile(color = "gray90", size = 0.3) +
  scale_fill_manual(values = c("0" = "white", "1"= "#d5fbd1", "2"= "#00C700", "3"= "darkgreen"),
    name = "Number of\nIncidence \nPeaks per \nMonth"
  ) +
  labs(x = "Month", y = "Province", tag="B") +
  scale_y_discrete(limits=rev, 
                   labels = setNames(incidence$provincena, incidence$areaid)
  ) +
  theme_accessible


#Figure 1c: Trajectories
incidence <- read.csv("Scripts_Draft2/Outputs/Vietnam_observed_incidence.csv")
incidence$tsdatetime <- as.Date(incidence$tsdatetime, format = "%Y-%m-%d")
incidence$year <- format(incidence$tsdatetime, "%Y")

incidence <- incidence %>%
  mutate(month = month(tsdatetime),  # Extract month for x-axis
         month_name = month(tsdatetime, label = TRUE, abbr = TRUE))  # Month labels

# Calculate the average for each year and areaid (for grey lines)
incidence_avg_per_area <- incidence %>%
  group_by(areaid, year, month) %>%
  summarise(average_observed = mean(observed, na.rm = TRUE), .groups = "drop")

# Calculate the yearly average across all areaids (for dark grey lines)
incidence_avg_per_year <- incidence %>%
  group_by(year, month) %>%
  summarise(average_observed = mean(observed, na.rm = TRUE), .groups = "drop")

# Calculate the overall average (for dark blue line)
overall_avg <- incidence %>%
  group_by(month) %>%
  summarise(average_observed = mean(observed, na.rm = TRUE), .groups = "drop")

# Plot
figure1c <-ggplot() +
  # Grey lines for each areaid (light grey and transparent)
  geom_line(data = incidence_avg_per_area, aes(x = factor(month), y = average_observed, 
                                               group = interaction(areaid, year)), 
            color = "lightgrey", alpha = 0.5) +
  
  # Dark grey line for the average of each year across all areaids
  geom_line(data = incidence_avg_per_year, aes(x = factor(month), y = average_observed, 
                                               group = year), 
            color = "darkgrey", size = 1.2) +
  
  # Dark blue line for the overall average across all areaids and years
  geom_line(data = overall_avg, aes(x = factor(month), y = average_observed), 
            color = "darkgreen", size = 1.5, group = 1) +  # Adjust group to handle overall line
  
  # Text labels for the annual averages (show year)
  geom_text(data = incidence_avg_per_year %>% filter(month == "9"), 
            aes(x = month, y = average_observed, label = as.character(year)),
            color = "darkgrey", vjust = -0.5) +
  
  # Text label for the overall average
  geom_text(data = overall_avg %>% filter(month == "9"), 
            aes(x = month, y = average_observed, label = "Overall Average"),
            color = "darkgreen", vjust = -0.5) +
  
  # Square root transformation on the y-axis
  scale_y_sqrt() +
  
  # Customize plot
  labs(x = "Month", y = "Monthly Average Dengue Incidence per 100,000", tag="C") +
  theme_accessible 


Figure1<- grid.arrange(
  figure1a, figure1b, figure1c,  # Add your plots
  layout_matrix = rbind(c(1, 2),  # First row: p1 and p2 side by side
                        c(3, 3)),  # Second row: p3 spans the whole width
  heights = c(1.5, 1)
)
Figure1
#ggsave("Scripts_Draft2/Outputs/Figures/Figure1.png", plot = Figure1, width = 1600, height = 2000, units = "px", dpi=300, limitsize = FALSE)



#######################################################
#Figure 2: Lead Times 
#######################################################

model_melt <- leadtime_metrics %>%
  select(LeadTime, ends_with("_DMOSS")) %>%
  pivot_longer(-LeadTime, names_to = "Metric", values_to = "value") %>%
  mutate(Metric = gsub("_DMOSS", "", Metric),
         variable = as.factor(LeadTime)) %>%
  group_by(Metric) %>%
  mutate(scaled_value = (value - min(value)) / (max(value) - min(value))) %>%
  ungroup()

diff_melt <- leadtime_metrics %>%
  select(LeadTime, ends_with("_diff")) %>%
  pivot_longer(-LeadTime, names_to = "Metric", values_to = "value") %>%
  mutate(Metric = gsub("_diff", "", Metric),
         variable = as.factor(LeadTime)) %>%
  group_by(Metric) %>%
  mutate(scaled_value = value / max(abs(value))) %>%
  ungroup()

heatmap_DMOSS <- ggplot(model_melt, aes(x = variable, y = Metric, fill = scaled_value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white", size = 4) +  # Adjust text size as needed
  scale_fill_gradient(low = "darkgreen", high = "lightgrey", name = "Scaled D-MOSS \nForecast Accuracy \nMetric Value") +
  labs(x = "Lead Time", y = NULL, tag = "A") +
  scale_y_discrete(labels = y_labels) +
  theme_accessible

heatmap_diff <- ggplot(diff_melt, aes(x = variable, y = Metric, fill = scaled_value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Adjust text size
  scale_fill_gradient2(
    low = diverging_palette[3], mid = diverging_palette[2], high = diverging_palette[1],
    midpoint = 0, limits = c(-1, 1), name = "Scaled Difference\nbetween D-MOSS \nForecast and \nSeasonal Baseline"
  ) +
  labs(x = "Lead Time", y = NULL, tag = "B") +
  scale_y_discrete(labels = y_labels) +
  theme_accessible

grid.arrange(heatmap_DMOSS, heatmap_diff, ncol = 1)



#######################################################
#Figure 3: Temporal
#######################################################

#Figure3a: Line graph with rugplot of month vs metric 
incidence_monthly <- incidence %>%
  mutate(month = month(tsdatetime)) %>%
  group_by(month) %>%
  summarize(observed = mean(tsvalue, na.rm = TRUE))
temporal_monthly <- temporal_metrics %>%
  group_by(month) %>%
  summarize(
    rmse = mean(rmse_DMOSS),
    rmse_diff= mean(rmse_diff),
    peakdiff = mean(peakdiff_DMOSS,na.rm = TRUE),
    peakdiff_diff = mean(peakdiff_diff),
  )
plot_data <- left_join(temporal_monthly, incidence_monthly, by = "month")

scale_factor_rmse <- max(plot_data$rmse, na.rm = TRUE) / max(plot_data$observed, na.rm = TRUE)
scale_factor_peakdiff <- max(plot_data$peakdiff, na.rm = TRUE) / max(plot_data$observed, na.rm = TRUE)
scale_factor_rmse_diff <- max(plot_data$rmse_diff, na.rm = TRUE) / max(plot_data$observed, na.rm = TRUE)
scale_factor_peakdiff_diff <- max(plot_data$peakdiff_diff, na.rm = TRUE) / max(plot_data$observed, na.rm = TRUE)
plot_data <- plot_data %>%
  mutate(observed_scaled_rmse = observed * scale_factor_rmse,
         observed_scaled_peakdiff = observed * scale_factor_peakdiff,
         observed_scaled_rmse_diff = observed * scale_factor_rmse_diff,
         observed_scaled_peakdiff_diff = observed * scale_factor_peakdiff_diff)

figure3a_rmse <- ggplot(plot_data, aes(x = month)) +
  geom_col(aes(y = observed_scaled_rmse), fill = "gray80", alpha = 0.4) +
  geom_point(aes(y = rmse), color = "darkgreen", size = 3) + 
  scale_x_continuous(breaks = 1:12) +  # Ensure months are labeled as 1-12 
  labs(x = "Month", y = "Monthly Mean \nRoot Mean Squared Error", tag = "A") +
  theme_accessible

figure3a_peakdiff <- ggplot(plot_data, aes(x = month)) +
  geom_col(aes(y = observed_scaled_peakdiff), fill = "gray80", alpha = 0.4) +
  geom_point(aes(y = peakdiff), color = "darkgreen", size = 3) +  
  scale_x_continuous(breaks = 1:12) +  # Ensure months are labeled as 1-12
  labs(x = "Month", y = "Monthly Mean \nPeak Timing Difference") +
  theme_accessible


#Figure3b: Heatmap of metric x month x lead time

figure3b_rmse <- ggplot(temporal_metrics, aes(x = as.factor(LeadTime), y = as.factor(month), fill = rmse_DMOSS)) +
  geom_tile() +
  scale_fill_gradient(low = "darkgreen", high = "lightgrey", name = "D-MOSS Root\nMean Squared \nError Value") +
  labs(x = "Lead Time", y = "Month", tag = "B") +
  scale_y_discrete(labels = y_labels, limits=rev) +
  theme_accessible

figure3b_peakdiff <- ggplot(temporal_metrics, aes(x = as.factor(LeadTime), y = as.factor(month), fill = peakdiff_DMOSS)) +
  geom_tile() +
  scale_fill_gradient(low = "darkgreen", high = "lightgrey", name = "D-MOSS Peak\nDifference \nValue") +
  labs(x = "Lead Time", y = "Month") +
  scale_y_discrete(labels = y_labels, limits=rev) +
  theme_accessible



# Figure 3c:
figure3c_rmse <- ggplot(temporal_monthly, aes(x = month)) +
  #geom_col(aes(y = plot_data$observed_scaled_rmse_diff), fill = "gray80", alpha = 0.4) +
  geom_point(aes(y = rmse_diff), color = "darkgreen", size = 3) +  
  geom_line(y=0, color= "black", linetype = "dashed") +
  scale_x_continuous(breaks = 1:12) +  # Ensure months are labeled as 1-12
  labs(x = "Month", y = "Monthly Mean Difference \nRoot Mean Squared Error", tag = "C") +
  theme_accessible

figure3c_peakdiff <- ggplot(temporal_monthly, aes(x = month)) +
  #geom_col(aes(y = plot_data$observed_scaled_peakdiff_diff), fill = "gray80", alpha = 0.4) +
  geom_point(aes(y = peakdiff_diff), color = "darkgreen", size = 3) +  
  geom_line(y=0, color= "black", linetype = "dashed") +
  scale_x_continuous(breaks = 1:12) +  # Ensure months are labeled as 1-12
  labs(x = "Month", y = "Monthly Mean Difference\nPeak Timing Difference") +
  theme_accessible


#Figure 3d
figure3d_rmse <- ggplot(temporal_metrics, aes(x = as.factor(LeadTime), y = as.factor(month), fill = rmse_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = diverging_palette[3], mid = diverging_palette[2], high = diverging_palette[1],
    midpoint = 0, name = "RMSE Difference\nbetween D-MOSS \nForecast and \nSeasonal Baseline"
  ) +
  labs(x = "Lead Time", y = "Month", tag = "D") +
  scale_y_discrete(labels = y_labels, limits=rev) +
  theme_accessible

figure3d_peakdiff <- ggplot(temporal_metrics, aes(x = as.factor(LeadTime), y = as.factor(month), fill = peakdiff_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = diverging_palette[3], mid = diverging_palette[2], high = diverging_palette[1],
    midpoint = 0, name = "Peak Difference\nbetween D-MOSS \nForecast and \nSeasonal Baseline"
  ) +
  labs(x = "Lead Time", y = "Month") +
  scale_y_discrete(labels = y_labels, limits=rev) +
  theme_accessible



grid.arrange(figure3a_rmse, figure3a_peakdiff, figure3b_rmse, figure3b_peakdiff, figure3c_rmse, figure3c_peakdiff, figure3d_rmse, figure3d_peakdiff,   ncol = 2,  heights = c(1, 1.5, 1, 1.5))



#######################################################
#Figure 4: Spatial 
#######################################################
spatial_metrics <- provinces %>%  left_join(spatial_metrics, by = "areaid")

# Define selected provinces to display with an arrow
spatial_error_se_selected_provinces <- c("Ha Noi", "TP. Ho Chi Minh", "Dong Nai", "Binh Duong", "Quang Binh", "Quang Tri", "Da Nang", "Quang Nam", "Ba Ria - Vung Tau", "Dak Lak")  
spatial_error_peak_selected_provinces <- c("Ha Noi", "TP. Ho Chi Minh", "Dong Nai", "Binh Duong", "Cao Bang", "Dien Bien", "Hung Yen", "Quang Tri", "Can Tho", "Vinh Long", "Ha Giang", "Son La")  
spatial_error_trajectory_selected_provinces <- c("Ha Noi", "TP. Ho Chi Minh", "Dong Nai", "Binh Duong", "Quang Binh", "Da Nang", "Ba Ria - Vung Tau", "Long An", "Dong Thap", "AN Giang", "Dak Lak", "Quang Nam")  
spatial_value_se_selected_provinces <- c("Ha Noi", "TP. Ho Chi Minh", "Dong Nai", "Binh Duong", "Da Nang", "Dak Lak", "Ba Ria - Vung Tau")  
spatial_value_peak_selected_provinces <- c("Ha Noi", "TP. Ho Chi Minh", "Dong Nai", "Binh Duong", "Ha Giang", "Cao Bang", "Dien Bien", "Son La", "Can Tho")  
spatial_value_trajectory_selected_provinces <- c("Ha Noi", "TP. Ho Chi Minh", "Dong Nai", "Binh Duong", "Long An", "Dong Thap", "AN Giang", "Dak Lak", "Quang Nam")  
province_labels <- setNames(ifelse(spatial_metrics$provincena %in% spatial_error_se_selected_provinces, paste0(spatial_metrics$provincena, " \u2192"), ""), spatial_metrics$provincena)

#Figure 4a: Province metrics
province_labels <- setNames(ifelse(spatial_metrics$provincena %in% spatial_error_se_selected_provinces, 
                                   paste0(spatial_metrics$provincena, " \u2192"), ""), spatial_metrics$provincena)
figure4a_rmse <- ggplot(spatial_metrics, aes(x = as.factor(LeadTime), y = as.factor(provincena), fill = rmse_DMOSS)) +
  geom_tile() +
  scale_fill_gradient(low = "darkgreen", high = "lightgrey", name = "D-MOSS \nRoot\nMean \nSquared \nError") +
  labs(x = "Lead Time", y = "Province", tag = "A") +
  scale_y_discrete(limits = rev(unique(spatial_metrics$provincena)), labels = province_labels) +
  theme_accessible


province_labels <- setNames(ifelse(spatial_metrics$provincena %in% spatial_error_peak_selected_provinces, 
                                   paste0(spatial_metrics$provincena, " \u2192"), ""), spatial_metrics$provincena)
figure4a_peakdiff <- ggplot(spatial_metrics, aes(x = as.factor(LeadTime), y = as.factor(provincena), fill = peakdiff_DMOSS)) +
  geom_tile() +
  scale_fill_gradient(low = "darkgreen", high = "lightgrey", name = "D-MOSS \nPeak\nDifference \nValue") +
  labs(x = "Lead Time", y = "Province") +
  scale_y_discrete(limits = rev(unique(spatial_metrics$provincena)), labels = province_labels) +
  theme_accessible


province_labels <- setNames(ifelse(spatial_metrics$provincena %in% spatial_error_trajectory_selected_provinces, 
                                   paste0(spatial_metrics$provincena, " \u2192"), ""), spatial_metrics$provincena)
figure4a_trajectory <- ggplot(spatial_metrics, aes(x = as.factor(LeadTime), y = as.factor(provincena), fill = trajectorydistance_DMOSS)) +
  geom_tile() +
  scale_fill_gradient(low = "darkgreen", high = "lightgrey", name = "D-MOSS \nTrajectory\nDistance \nValue") +
  labs(x = "Lead Time", y = "Province") +
  scale_y_discrete(limits = rev(unique(spatial_metrics$provincena)), labels = province_labels) +
  theme_accessible



#Figure 4b: Province value 

province_labels <- setNames(ifelse(spatial_metrics$provincena %in% spatial_value_se_selected_provinces, 
                                   paste0(spatial_metrics$provincena, " \u2192"), ""), spatial_metrics$provincena)
figure4b_rmse <- ggplot(spatial_metrics, aes(x = as.factor(LeadTime), y = as.factor(provincena), fill = rmse_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = diverging_palette[3], mid = diverging_palette[2], high = diverging_palette[1],
    midpoint = 0, name = "RMSE\nDifference\nbetween\nD-MOSS \nForecast and\nSeasonal\nBaseline"
  ) +
  labs(x = "Lead Time", y = "Province", tag = "B") +
  scale_y_discrete(limits = rev(unique(spatial_metrics$provincena)), labels = province_labels) +
  theme_accessible

province_labels <- setNames(ifelse(spatial_metrics$provincena %in% spatial_value_peak_selected_provinces, 
                                   paste0(spatial_metrics$provincena, " \u2192"), ""), spatial_metrics$provincena)
figure4b_peakdiff <- ggplot(spatial_metrics, aes(x = as.factor(LeadTime), y = as.factor(provincena), fill = peakdiff_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = diverging_palette[3], mid = diverging_palette[2], high = diverging_palette[1],
    midpoint = 0, name = "Peak\nDifference\nbetween \nD-MOSS \nForecast and\nSeasonal \nBaseline"
  ) +
  labs(x = "Lead Time", y = "Province") +
  scale_y_discrete(limits = rev(unique(spatial_metrics$provincena)), labels = province_labels) +
  theme_accessible

province_labels <- setNames(ifelse(spatial_metrics$provincena %in% spatial_value_trajectory_selected_provinces, 
                                   paste0(spatial_metrics$provincena, " \u2192"), ""), spatial_metrics$provincena)
figure4b_trajectory <- ggplot(spatial_metrics, aes(x = as.factor(LeadTime), y = as.factor(provincena), fill = trajectorydistance_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = diverging_palette[3], mid = diverging_palette[2], high = diverging_palette[1],
    midpoint = 0, name = "Trajectory\nDistance\nDifference\nbetween\nD-MOSS\nForecast and\nSeasonal\nBaseline"
  ) +
  labs(x = "Lead Time", y = "Province") +
  scale_y_discrete(limits = rev(unique(spatial_metrics$provincena)), labels = province_labels) +
  theme_accessible



#Figure 4c: Province map
provinces_tobelabelled <- c("Ha Noi", "TP. Ho Chi Minh", "Dong Nai", "Binh Duong", "Quang Binh", "Quang Tri", "Da Nang", "Quang Nam", "Ba Ria - Vung Tau", "Dak Lak", "Cao Bang", "Dien Bien", "Hung Yen", "Can Tho", "Vinh Long", "Ha Giang", "Son La", "Long An", "Dong Thap", "AN Giang")  
province_map <- spatial_metrics %>%
  select(provincena, geometry) %>%
  mutate(highlight = ifelse(provincena %in% provinces_tobelabelled, "Highlighted", "Other"))
province_map <- province_map %>%
  distinct(provincena, .keep_all = TRUE) %>%
  mutate(centroid = st_centroid(geometry),
         Longitude = st_coordinates(centroid)[,1],  # Extract X coordinates
         Latitude = st_coordinates(centroid)[,2])  # Extract Y coordinates

figure4c <- ggplot() +
  geom_sf(data = province_map, aes(fill = highlight), color = "black", size = 0.5) +  # Black outlines
  scale_fill_manual(values = c("Highlighted" = "lightgrey", "Other" = "white")) +  # Set fill colors
  geom_label_repel(
    data = province_map %>% filter(provincena %in% provinces_tobelabelled, provincena != "Da Nang"), 
    aes(x = Longitude, y = Latitude, label = provincena),  # Use Longitude and Latitude for positions
    size = 4, 
    color = "black",  # Text color
    fill = "white",   # Background color for the label (white for contrast)
    force = 1,  # Adjust the strength of label repulsion (lower force to prioritize leader lines)
    force_pull= 2,
    segment.color = "black",  # Draw leader lines to avoid overlap
    box.padding = 2,  # Padding around the text box to give space for labels
    max.overlaps = Inf,  # Avoid label overlap with shapes and other labels
    direction = "both",  # Allow labels to move in both directions from their centroids
    min.segment.length = 0,  # Ensure leader lines connect directly to the centroids
    nudge_x = -1,  # Adjust position horizontally to push labels to the left
    nudge_y = -1,  # Adjust position horizontally to push labels to the left
    segment.size = 0.8,  # Adjust the thickness of the leader line
  ) +
  
  # Manual adjustment for Da Nang (if necessary)
  geom_label_repel(
    data = province_map %>% filter(provincena == "Da Nang"), 
    aes(x = Longitude - 1, y = Latitude , label = provincena),  # Use the actual coordinates
    size = 4, 
    color = "black",  # Text color
    fill = "white",   # Background color for the label (white for contrast)
    segment.color = "black",  # Draw leader lines to avoid overlap
    box.padding = 2,  # Padding around the text box to give space for labels
    max.overlaps = Inf,  # Avoid label overlap with shapes and other labels
    direction = "both",  # Allow labels to move in both directions from their centroids
    min.segment.length = 0,  # Ensure leader lines connect directly to the centroids
    nudge_x = 2,  # Adjust position horizontally to push labels to the left
    nudge_y = 0,  # Adjust position horizontally to push labels to the left
    segment.size = 0.8,  # Adjust the thickness of the leader line
  ) +
  coord_sf(xlim = c(90, 125)) +
  labs(tag="C") +
  theme_accessible +
  theme(legend.position = "none")  


# Define the layout matrix using rbind
layout_matrix <- rbind(c(1, 2, 3), 
                       c(4, 5, 6), 
                       c(7, 7, 7)) 
# Create the grid with the provinces_map spanning the third row
grid.arrange(
  figure4a_rmse, figure4a_peakdiff, figure4a_trajectory, 
  figure4b_rmse, figure4b_peakdiff, figure4b_trajectory,
  figure4c,  # This is the map plot
  layout_matrix = layout_matrix,  # Custom layout matrix to span 3 columns for the map
  heights = c(1, 1, 1),
  widths = c(1, 1, 1)    # Equal height for all rows or adjust as needed
)







#######################################################
#Figure 5: Operational utility scenario accuracy assessment  
#######################################################

#Input scenario assessments
scenario1_forecasts_april <- read.csv("Scripts_Draft2/Outputs/Scenario1_Aprilforecast.csv")
scenario2_forecasts_april <- read.csv( "Scripts_Draft2/Outputs/Scenario2_Aprilforecast.csv")
scenario3_forecasts_april  <-read.csv( "Scripts_Draft2/Outputs/Scenario3_Aprilforecast.csv")
scenario4_forecasts_april <- read.csv( "Scripts_Draft2/Outputs/Scenario4_Aprilforecast.csv")


custom_accessible_colors <- c(
  "Scenario 1" = "#E69F00",  
  "Scenario 2" = "#009E73",  
  "Scenario 3" = "#56B4E9", 
  "Scenario 4" = "#CC79A7"   
)

#Makes ONE plot for each outbreak threshold

#New plot idea; single plot comparing accuracy vs probability (or ROC curve); with confusion matrices for set probability thresholds next to it 

#Set name so all the same
scenario1_forecasts_april$epi.2d <- scenario1_forecasts_april$epi.2d_more
scenario4_forecasts_april$epi.2d <- scenario4_forecasts_april$epi.2d_peak

#Set outbreak threshold
prob_col <- "prob2sd"                        # options: "prob1sd", "prob2sd", "probq75", "probq95" OR for scenario 1 "sum_prob1sd", "sum_prob2sd", "sum_probq75", "sum_prob95"
actual_col <- "epi.2d"                       # options: "epi.1d", "epi.2d", "epi.q75", "epi.q95" OR for scenario 1 "epi.1d_more", "epi.2d_more", "epi.q75_more", "epi.q95_more" OR for scenario 4 "epi.1d_peak", "epi.2d_peak", "epi.q75_peak", "epi.q95_peak"

#Flag for scenario 4 to only include provinces that exceeded that particular threshold for 3 months
scenario4_forecasts_april <- scenario4_forecasts_april %>%
  group_by(areaid) %>%
  filter(any(consec_exceed_epi.2d_flag == TRUE, na.rm = TRUE)) %>%
  ungroup()
########################
# ROC curves 
########################

subset_dfs <- list(
  "Scenario 1" = scenario1_forecasts_april,
  "Scenario 2" = scenario2_forecasts_april,
  "Scenario 3" = scenario3_forecasts_april,
  "Scenario 4" = scenario4_forecasts_april
)

# Store all ROC data
roc_data_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  
  roc_obj <- roc(df[[actual_col]], df[[prob_col]])
  auc_val <- auc(roc_obj)
  
  # Compute Brier Score
  brier_score <- mean((df[[prob_col]] - df[[actual_col]])^2)
  
  data.frame(
    specificity = rev(roc_obj$specificities),
    sensitivity = rev(roc_obj$sensitivities),
    group = name,
    auc = round(auc_val, 3),
    brier = round(brier_score, 3)
    
  )
})

# Combine all ROC data into one dataframe
all_roc_df <- bind_rows(roc_data_list)

# Extract AUCs and Brier for plot
metrics_df <- all_roc_df %>%
  group_by(group) %>%
  summarise(
    auc = unique(auc),
    brier = unique(brier)
  ) %>%
  mutate(
    auc_label = paste0(group, " (AUC=", auc, ")"),
    brier_label = paste0(group, " (Brier=", brier, ")")
  )



# Plot
plot_a <- ggplot(all_roc_df, aes(x = 1 - specificity, y = sensitivity, color = group)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  scale_color_manual(values = custom_accessible_colors) +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate",
    tag= "A",
    color = "Group"
  ) +
  theme_accessible



########################
# Accuracy
########################
thresholds <- seq(0, 1, by = 0.01)

acc_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  
  # For each threshold, compute accuracy
  acc_df <- lapply(thresholds, function(thresh) {
    preds <- ifelse(df[[prob_col]] >= thresh, 1, 0)
    actuals <- df[[actual_col]]
    accuracy <- mean(preds == actuals)
    
    data.frame(
      threshold = thresh,
      accuracy = accuracy,
      Group = name
    )
  }) %>% bind_rows()
  
  acc_df
})

acc_data <- bind_rows(acc_list)


max_acc_data <- acc_data %>%
  group_by(Group) %>%
  filter(accuracy == max(accuracy)) %>%
  slice(1) %>%       # take the first max occurrence only
  ungroup() %>%
  mutate(Label = paste0(Group, " accuracy max: ", round(accuracy, 3), " at exceedance threshold of ", round(threshold, 2)))

plot_b <- ggplot(acc_data, aes(x = threshold, y = accuracy, color = Group)) +
  geom_line(size = 1.2) +
  geom_segment(data = max_acc_data,
               aes(x = threshold, xend = threshold, y = 0.62, yend = 1, color = Group),
               linetype = "dashed", size = 0.8, show.legend = FALSE) +
  geom_text(data = max_acc_data, aes(x = threshold, y = 0, label = Label, color = Group),
            angle = 270,
            vjust = 0.2,
            hjust = 1,
            size = 3.5,
            show.legend = FALSE,
            position = position_nudge(x = c(-0.01, 0.01, 0, 0))) +
  scale_color_manual(values = custom_accessible_colors) +
  labs(x = "Probability Threshold", y = "Accuracy", color = "Scenario", tag= "A" ) +
  theme_minimal(base_size = 14) +
  theme_accessible


########################
#CONF MATRICES
########################


thresholds <- c(0.25, 0.5, 0.75)
group_names <- names(subset_dfs)

confmat_plot_data <- list()

for (group_name in group_names) {
  df <- subset_dfs[[group_name]]
  
  for (thresh in thresholds) {
    temp_df <- df %>%
      mutate(
        Predicted = ifelse(.data[[prob_col]] >= thresh, 1, 0),
        Actual = .data[[actual_col]]
      )
    
    # Use caret's confusionMatrix
    cm <- confusionMatrix(
      as.factor(temp_df$Predicted),
      as.factor(temp_df$Actual),
      positive = "1"
    )
    
    # Reformat to long
    cm_df <- as.data.frame(cm$table)
    colnames(cm_df) <- c("Predicted", "Actual", "Count")
    
    cm_df <- cm_df %>%
      mutate(
        Classification = case_when(
          Predicted == Actual & Predicted == 1 ~ "True Positive (TP)",
          Predicted == Actual & Predicted == 0 ~ "True Negative (TN)",
          Predicted != Actual & Predicted == 1 ~ "False Positive (FP)",
          Predicted != Actual & Predicted == 0 ~ "False Negative (FN)"
        ),
        FillColor = ifelse(Classification %in% c("True Positive (TP)", "True Negative (TN)"),
                           "Correct", "Incorrect"),
        Group = group_name,
        Threshold = paste0("Exceedance Probability \u2265 ", thresh)
      ) %>%
      mutate(Proportion = Count / sum(Count)) %>%
      mutate(
        Percent_Label = paste0(round(Proportion * 100, 1), "%"),
        Full_Label = case_when(
          Classification == "False Positive (FP)" ~ paste("False Alarms:", "\n", Percent_Label,  "(n = ", Count, ")"),
          Classification == "False Negative (FN)" ~ paste("Missed Actions:", "\n", Percent_Label,  "(n = ", Count, ")"),
          Classification == "True Positive (TP)" ~ paste("Correct Action:",  "\n", Percent_Label, "(n = ", Count, ")"),
          Classification == "True Negative (TN)" ~ paste("Correct No Action:", "\n", Percent_Label, "(n = ", Count, ")")
        )
      )
    
    confmat_plot_data[[length(confmat_plot_data) + 1]] <- cm_df
  }
}

# Combine all
all_cm_df <- bind_rows(confmat_plot_data)

# Color scheme (you can customize)
correct_colour <- "#40B0A6"  # blue
incorrect_colour <- "#E1BE6A"  # red

color_scale <- scale_fill_manual(
  values = c("Correct" = correct_colour, "Incorrect" = incorrect_colour),
  name = "Prediction Accuracy"
)

# Plot with facets
plot_c <- ggplot(all_cm_df, aes(x = Predicted, y = Actual)) +
  geom_tile(color = "black", fill = NA, linewidth = 0.8) +
  geom_point(aes(size = Proportion, fill = FillColor), shape = 21, color = "black") +
  geom_text(aes(label = Full_Label), size = 3, lineheight = 0.8) +
  color_scale +
  scale_size(range = c(10, 40), guide = "none") +
  facet_grid(Group ~ Threshold) +
  labs(
    x = "Predicted Exceedance",
    y = "Observed Exceedance", 
    tag= "B"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  ) + 
  guides(
    fill = guide_legend(
      override.aes = list(size = 6),  # size of points in legend
      keywidth = unit(1.5, "cm"),     # width of legend keys
      keyheight = unit(1.5, "cm")     # height of legend keys
    )) +
  theme_accessible



########################
# Bar plots showing prediction outcomes
########################
classification_colors <- c(
  "Hits" = "#2B7A78",                # Dark teal (TP)
  "Correct Rejections" = "#ADE1DC",  # Light teal (TN)
  "False Alarms" = "#F3E1A3",        # Light gold (FP)
  "Missed" = "#E1BE6A"               # Dark gold (FN)
)

# Relabel classification levels and reorder
all_cm_df <- all_cm_df %>%
  mutate(
    Outcome = case_when(
      Classification == "True Positive (TP)" ~ "Hits",
      Classification == "True Negative (TN)" ~ "Correct Rejections",
      Classification == "False Positive (FP)" ~ "False Alarms",
      Classification == "False Negative (FN)" ~ "Missed"
    ),
    Outcome = factor(Outcome, levels = c("Missed", "False Alarms", "Correct Rejections", "Hits")),
    Label = ifelse(Count == 0, NA, paste0("n = ", Count))
  )

# Plot
plot_d <- ggplot(all_cm_df, aes(x = Threshold, y = Proportion, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5, color = "black", fontface="bold") +
  facet_wrap(~ Group, ncol = 1) +
  scale_fill_manual(values = classification_colors, name = "Prediction Outcome") +
  labs(
    x = "Exceedance Probability Threshold",
    y = "Proportion of Predictions",
    tag = "B"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  theme_accessible



# Create subtitle text: Combine AUC and Brier into single labels
metrics_df$combined_label <- paste0(  metrics_df$group, " AUC=", metrics_df$auc, ", Brier=", metrics_df$brier)
subtitle_text <- paste(metrics_df$combined_label, collapse = " | ")


#Option1s
option1 <- (plot_a / plot_c) +
  plot_layout(heights = c(1, 1.5)) + 
  plot_annotation(subtitle = subtitle_text, theme= theme_accessible)

#Option2s
option2 <- (plot_b / plot_d) +
  plot_layout(heights = c(1, 1.8)) + 
  plot_annotation(subtitle = subtitle_text, theme= theme_accessible)

#Final
finalplot <-(plot_a / plot_d) +
  plot_layout(heights = c(1, 1.8)) + 
  plot_annotation(subtitle = subtitle_text, theme= theme_accessible)


finalplot



#######################################################
#Supplementary Figures
#######################################################


#S1 Figure: Made externally (schematic)

#S2 Figure: June 2022 peak analysis

peaks_2022 <- read.csv("Scripts_Draft2/Outputs/2022peaks_leadtime1.csv")
peaks_map <- provinces %>%  left_join(peaks_2022, by = "areaid")

map1 <- ggplot(peaks_map) +
  geom_sf(aes(fill = factor(observed_June_peak))) +
  scale_fill_manual(values = c("0" = "white", "1" = "darkgreen"), name = "Observed June Peak") +
  labs(tag="A") +
  theme_accessible

peaks_map <- peaks_map %>%
  mutate(
    forecasted_peak_month = ifelse(observed_June_peak == 1, month(peak_2022), NA)
  )

month_colours <- c("6" = "darkgreen",  "7" = "gray90", "8" = "gray60", "9" = "gray30")

map2 <- ggplot(peaks_map) +
  geom_sf(aes(fill = factor(forecasted_peak_month))) +
  scale_fill_manual(values = month_colours, name = "Forecasted Peak Month", na.value = "white") +
  labs(tag="B") +
  theme_accessible

map1 + map2

#S3 Figure: Choropleth maps of spatiotemporal patterns of D-MOSS performance 

figureS3a_rmse <- ggplot(data = spatial_metrics) +
  geom_sf(aes(fill = rmse_DMOSS)) +
  scale_fill_gradient(
    low = "darkgreen", 
    high = "lightgrey", 
    name = "Root Mean \nSquared Error"
  ) +
  guides(fill = guide_colorbar(title.theme = element_text(size = 8))) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 6), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), legend.text = element_text(size = 8))+
  coord_sf(expand = TRUE) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(tag="A") +
  theme_accessible

figureS3a_peak <- ggplot(data = spatial_metrics) +
  geom_sf(aes(fill = peakdiff_DMOSS)) +
  scale_fill_gradient(
    low = "darkgreen", 
    high = "lightgrey", 
    name = "Mean Peak\nTemporal Distance"
  ) +
  guides(fill = guide_colorbar(title.theme = element_text(size = 8))) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 6), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), legend.text = element_text(size = 8)) +
  coord_sf(expand = TRUE) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )+
  theme_accessible


figureS3a_trajectory <- ggplot(data = spatial_metrics) +
  geom_sf(aes(fill = trajectorydistance_DMOSS)) +
  scale_fill_gradient(
    low = "darkgreen", 
    high = "lightgrey", 
    name = "Mean Trajectory\nDistance"
  ) +
  guides(fill = guide_colorbar(title.theme = element_text(size = 8))) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 6), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), legend.text = element_text(size = 8)) +
  coord_sf(expand = TRUE) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )+
  theme_accessible

figureS3b_rmse <- ggplot(data = spatial_metrics) +
  geom_sf(aes(fill = rmse_diff)) +
  scale_fill_gradient2(
    low = diverging_palette[3], mid = diverging_palette[2], high = diverging_palette[1],
    midpoint = 0, name = "RMSE\nDifference\nbetween\nD-MOSS \nForecast and\nSeasonal\nBaseline"
  )+
  guides(fill = guide_colorbar(title.theme = element_text(size = 8))) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 6), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), legend.text = element_text(size = 8)) + 
  coord_sf(expand = TRUE) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )+
  labs(tag="B") +
  theme_accessible


figureS3b_peak <- ggplot(data = spatial_metrics) +
  geom_sf(aes(fill = peakdiff_diff)) +
  scale_fill_gradient2(
    low = diverging_palette[3], mid = diverging_palette[2], high = diverging_palette[1],
    midpoint = 0, name = "Peak Timing\nDifference\nbetween\nD-MOSS \nForecast and\nSeasonal\nBaseline"
  )+
  guides(fill = guide_colorbar(title.theme = element_text(size = 8))) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 6), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), legend.text = element_text(size = 8))+ 
  coord_sf(expand = TRUE) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )+
  theme_accessible


figureS3b_trajectory <- ggplot(data = spatial_metrics) +
  geom_sf(aes(fill = trajectorydistance_diff)) +
  scale_fill_gradient2(
    low = diverging_palette[3], mid = diverging_palette[2], high = diverging_palette[1],
    midpoint = 0, name = "Trajectory\nDistance \nDifference\nbetween\nD-MOSS \nForecast and\nSeasonal\nBaseline"
  )+
  guides(fill = guide_colorbar(title.theme = element_text(size = 8))) +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), axis.title.x = element_text(size = 8), axis.title.y = element_text(size = 6), axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), legend.text = element_text(size = 8)) +
  coord_sf(expand = TRUE) + 
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )+
  theme_accessible

grid.arrange(figureS3a_rmse, figureS3a_peak, figureS3a_trajectory,  figureS3b_rmse, figureS3b_peak, figureS3b_trajectory, 
             ncol = 3, widths = c(1, 1, 1))


# S4 Figure: Operational scenario accuracy results with sensitivity and PPV values 
thresholds <- seq(0, 1, by = 0.01)
metrics_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  metric_df <- lapply(thresholds, function(thresh) {
    preds <- ifelse(df[[prob_col]] >= thresh, 1, 0)
    actuals <- df[[actual_col]]
    
    TP <- sum(preds == 1 & actuals == 1)
    TN <- sum(preds == 0 & actuals == 0)
    FP <- sum(preds == 1 & actuals == 0)
    FN <- sum(preds == 0 & actuals == 1)
    
    accuracy <- (TP + TN) / (TP + TN + FP + FN)
    sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
    ppv <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
    
    data.frame(
      threshold = thresh,
      accuracy = accuracy,
      sensitivity = sensitivity,
      ppv = ppv,
      Group = name
    )
  }) %>% bind_rows()
  
  metric_df
})
metrics_data <- bind_rows(metrics_list)
plot_data <- metrics_data %>%
  pivot_longer(cols = c(accuracy, sensitivity, ppv),
               names_to = "Metric",
               values_to = "Value")



max_acc_facet_data <- metrics_data %>%
  group_by(Group) %>%
  filter(accuracy == max(accuracy)) %>%
  slice(1) %>%  # pick first in case of ties
  ungroup() %>%
  mutate(
    label_x = pmin(pmax(threshold, 0 + 0.02), 1 - 0.2),
    label_y = pmin(accuracy + 0.05, 0.98),
    label_text = paste0("Max Accuracy: ", round(accuracy, 3),
                        "\nat ", round(threshold, 2), " Probability")
  )

facet_plot <- ggplot(plot_data, aes(x = threshold, y = Value, color = Metric, linetype = Metric)) +
  geom_line(size = 1) +
  facet_wrap(~ Group) +
  
  geom_point(data = max_acc_facet_data,
             aes(x = threshold, y = accuracy),
             shape = 8,
             size = 3.5,
             color = "black",
             inherit.aes = FALSE) +
  
  geom_text(data = max_acc_facet_data,
            aes(x = label_x, y = label_y, label = label_text),
            hjust = 0.5,
            vjust = 0,
            size = 3,
            color = "black",
            inherit.aes = FALSE) +
  
  scale_color_manual(values = c("accuracy" = "darkgrey", "sensitivity" = "#E66100", "ppv" = "#5D3A9b"),
                     labels = c("accuracy" = "Accuracy", "sensitivity" = "Sensitivity", "ppv" = "PPV")) +
  scale_linetype_manual(values = c("accuracy" = "solid", "sensitivity" = "dashed", "ppv" = "dotted"),
                        labels = c("accuracy" = "Accuracy", "sensitivity" = "Sensitivity", "ppv" = "PPV")) +
  labs(
    x = "Probability Threshold",
    y = "Metric Value",
    color = "Metric",
    linetype = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme_accessible
facet_plot



# S5 Figure: Spatial trends in utility accuracy assessment across provinces

scenario1_forecasts_april <- provinces %>%  left_join(scenario1_forecasts_april, by = "areaid")
scenario2_forecasts_april <- provinces %>%  left_join(scenario2_forecasts_april, by = "areaid")
scenario3_forecasts_april <- provinces %>%  left_join(scenario3_forecasts_april, by = "areaid")
scenario4_forecasts_april <- provinces %>%  left_join(scenario4_forecasts_april, by = "areaid")

outcome_colors <- c(
  "True Positive" = "#56B4E9",
  "False Positive" = "#E69F00",
  "False Negative" = "#CC79A7",
  "True Negative" = "#009E73"
)


threshold <- 0.5

scenario1_forecasts_april_outcomes <- scenario1_forecasts_april %>%
  mutate(prediction = ifelse(prob2sd >= threshold, 1, 0),
    outcome = case_when(
      prediction == 1 & epi.2d == 1 ~ "True Positive",
      prediction == 1 & epi.2d == 0 ~ "False Positive",
      prediction == 0 & epi.2d == 1 ~ "False Negative",
      prediction == 0 & epi.2d == 0 ~ "True Negative"))
scenario1_plot <- ggplot(scenario1_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 1\nPrediction Outcome"  
  ) +
  theme_minimal(base_size = 14) +
  labs(tag="A") +
  theme_accessible

scenario2_forecasts_april_outcomes <- scenario2_forecasts_april %>%
  mutate(prediction = ifelse(prob2sd >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.2d == 1 ~ "True Positive",
           prediction == 1 & epi.2d == 0 ~ "False Positive",
           prediction == 0 & epi.2d == 1 ~ "False Negative",
           prediction == 0 & epi.2d == 0 ~ "True Negative"))
scenario2_plot <- ggplot(scenario2_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 2\nPrediction Outcome"  
  ) +
  theme_minimal(base_size = 14) +
  labs(tag="B") +
  theme_accessible

scenario3_forecasts_april_outcomes <- scenario3_forecasts_april %>%
  mutate(prediction = ifelse(prob2sd >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.2d == 1 ~ "True Positive",
           prediction == 1 & epi.2d == 0 ~ "False Positive",
           prediction == 0 & epi.2d == 1 ~ "False Negative",
           prediction == 0 & epi.2d == 0 ~ "True Negative"))
scenario3_plot <- ggplot(scenario3_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 3\nPrediction Outcome" 
  ) +
  theme_minimal(base_size = 14) +
  labs(tag="C") +
  theme_accessible

scenario4_forecasts_april_outcomes <- scenario4_forecasts_april %>%
  mutate(prediction = ifelse(prob2sd >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.2d == 1 ~ "True Positive",
           prediction == 1 & epi.2d == 0 ~ "False Positive",
           prediction == 0 & epi.2d == 1 ~ "False Negative",
           prediction == 0 & epi.2d == 0 ~ "True Negative"))
scenario4_plot <- ggplot(scenario4_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 4\nPrediction Outcome" 
  ) +
  theme_minimal(base_size = 14) +
  labs(tag="D") +
  theme_accessible

grid.arrange(scenario1_plot, scenario2_plot, scenario3_plot, scenario4_plot, 
             ncol = 2, widths = c(1, 1))

# S6 Figure: Utility accuracy assessment for May (mean +2SD)
scenario1_forecasts_may <- read.csv("Scripts_Draft2/Outputs/Scenario1_Mayforecast.csv")
scenario2_forecasts_may <- read.csv( "Scripts_Draft2/Outputs/Scenario2_Mayforecast.csv")
scenario3_forecasts_may  <-read.csv( "Scripts_Draft2/Outputs/Scenario3_Mayforecast.csv")
scenario4_forecasts_may <- read.csv( "Scripts_Draft2/Outputs/Scenario4_Mayforecast.csv")

scenario1_forecasts_may$epi.2d <- scenario1_forecasts_may$epi.2d_more
scenario4_forecasts_may$epi.2d <- scenario4_forecasts_may$epi.2d_peak
prob_col <- "prob2sd"                        # options: "prob1sd", "prob2sd", "probq75", "probq95" OR for scenario 1 "sum_prob1sd", "sum_prob2sd", "sum_probq75", "sum_prob95"
actual_col <- "epi.2d"                       # options: "epi.1d", "epi.2d", "epi.q75", "epi.q95" OR for scenario 1 "epi.1d_more", "epi.2d_more", "epi.q75_more", "epi.q95_more" OR for scenario 4 "epi.1d_peak", "epi.2d_peak", "epi.q75_peak", "epi.q95_peak"
scenario4_forecasts_may <- scenario4_forecasts_may %>%
  group_by(areaid) %>%
  filter(any(consec_exceed_epi.2d_flag == TRUE, na.rm = TRUE)) %>%
  ungroup()
# ROC curves 
subset_dfs <- list(
  "Scenario 1" = scenario1_forecasts_may,
  "Scenario 2" = scenario2_forecasts_may,
  "Scenario 3" = scenario3_forecasts_may,
  "Scenario 4" = scenario4_forecasts_may)
roc_data_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  roc_obj <- roc(df[[actual_col]], df[[prob_col]])
  auc_val <- auc(roc_obj)
  brier_score <- mean((df[[prob_col]] - df[[actual_col]])^2)
  data.frame(
    specificity = rev(roc_obj$specificities),
    sensitivity = rev(roc_obj$sensitivities),
    group = name,
    auc = round(auc_val, 3),
    brier = round(brier_score, 3))})
all_roc_df <- bind_rows(roc_data_list)
metrics_df <- all_roc_df %>%
  group_by(group) %>%
  summarise(
    auc = unique(auc),
    brier = unique(brier)
  ) %>%
  mutate(
    auc_label = paste0(group, " (AUC=", auc, ")"),
    brier_label = paste0(group, " (Brier=", brier, ")"))
plot_a <- ggplot(all_roc_df, aes(x = 1 - specificity, y = sensitivity, color = group)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  scale_color_manual(values = custom_accessible_colors) +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate",
    tag= "A",
    color = "Group"
  ) +
  theme_accessible
# Accuracy
thresholds <- seq(0, 1, by = 0.01)
acc_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  acc_df <- lapply(thresholds, function(thresh) {
    preds <- ifelse(df[[prob_col]] >= thresh, 1, 0)
    actuals <- df[[actual_col]]
    accuracy <- mean(preds == actuals)
    data.frame(
      threshold = thresh,
      accuracy = accuracy,
      Group = name
    )
  }) %>% bind_rows()
  acc_df
})
acc_data <- bind_rows(acc_list)
max_acc_data <- acc_data %>%
  group_by(Group) %>%
  filter(accuracy == max(accuracy)) %>%
  slice(1) %>%       # take the first max occurrence only
  ungroup() %>%
  mutate(Label = paste0(Group, " accuracy max: ", round(accuracy, 3), " at exceedance threshold of ", round(threshold, 2)))
plot_b <- ggplot(acc_data, aes(x = threshold, y = accuracy, color = Group)) +
  geom_line(size = 1.2) +
  geom_segment(data = max_acc_data,
               aes(x = threshold, xend = threshold, y = 0.62, yend = 1, color = Group),
               linetype = "dashed", size = 0.8, show.legend = FALSE) +
  geom_text(data = max_acc_data, aes(x = threshold, y = 0, label = Label, color = Group),
            angle = 270,
            vjust = 0.2,
            hjust = 1,
            size = 3.5,
            show.legend = FALSE,
            position = position_nudge(x = c(-0.01, 0.01, 0, 0))) +
  scale_color_manual(values = custom_accessible_colors) +
  labs(x = "Probability Threshold", y = "Accuracy", color = "Scenario", tag= "A" ) +
  theme_minimal(base_size = 14) +
  theme_accessible
#CONF MATRICES
thresholds <- c(0.25, 0.5, 0.75)
group_names <- names(subset_dfs)
confmat_plot_data <- list()
for (group_name in group_names) {
  df <- subset_dfs[[group_name]]
  for (thresh in thresholds) {
    temp_df <- df %>%
      mutate(
        Predicted = ifelse(.data[[prob_col]] >= thresh, 1, 0),
        Actual = .data[[actual_col]]
      )
    cm <- confusionMatrix(
      as.factor(temp_df$Predicted),
      as.factor(temp_df$Actual),
      positive = "1"
    )
    cm_df <- as.data.frame(cm$table)
    colnames(cm_df) <- c("Predicted", "Actual", "Count")
    cm_df <- cm_df %>%
      mutate(
        Classification = case_when(
          Predicted == Actual & Predicted == 1 ~ "True Positive (TP)",
          Predicted == Actual & Predicted == 0 ~ "True Negative (TN)",
          Predicted != Actual & Predicted == 1 ~ "False Positive (FP)",
          Predicted != Actual & Predicted == 0 ~ "False Negative (FN)"
        ),
        FillColor = ifelse(Classification %in% c("True Positive (TP)", "True Negative (TN)"),
                           "Correct", "Incorrect"),
        Group = group_name,
        Threshold = paste0("Exceedance Probability \u2265 ", thresh)
      ) %>%
      mutate(Proportion = Count / sum(Count)) %>%
      mutate(
        Percent_Label = paste0(round(Proportion * 100, 1), "%"),
        Full_Label = case_when(
          Classification == "False Positive (FP)" ~ paste("False Alarms:", "\n", Percent_Label,  "(n = ", Count, ")"),
          Classification == "False Negative (FN)" ~ paste("Missed Actions:", "\n", Percent_Label,  "(n = ", Count, ")"),
          Classification == "True Positive (TP)" ~ paste("Correct Action:",  "\n", Percent_Label, "(n = ", Count, ")"),
          Classification == "True Negative (TN)" ~ paste("Correct No Action:", "\n", Percent_Label, "(n = ", Count, ")")
        ))
    confmat_plot_data[[length(confmat_plot_data) + 1]] <- cm_df}}
all_cm_df <- bind_rows(confmat_plot_data)
color_scale <- scale_fill_manual(
  values = c("Correct" = correct_colour, "Incorrect" = incorrect_colour),
  name = "Prediction Accuracy"
)
plot_c <- ggplot(all_cm_df, aes(x = Predicted, y = Actual)) +
  geom_tile(color = "black", fill = NA, linewidth = 0.8) +
  geom_point(aes(size = Proportion, fill = FillColor), shape = 21, color = "black") +
  geom_text(aes(label = Full_Label), size = 3, lineheight = 0.8) +
  color_scale +
  scale_size(range = c(10, 40), guide = "none") +
  facet_grid(Group ~ Threshold) +
  labs(
    x = "Predicted Exceedance",
    y = "Observed Exceedance", 
    tag= "B"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  ) + 
  guides(
    fill = guide_legend(
      override.aes = list(size = 6),  # size of points in legend
      keywidth = unit(1.5, "cm"),     # width of legend keys
      keyheight = unit(1.5, "cm")     # height of legend keys
    )) +
  theme_accessible
# Bar plots showing prediction outcomes
all_cm_df <- all_cm_df %>%
  mutate(
    Outcome = case_when(
      Classification == "True Positive (TP)" ~ "Hits",
      Classification == "True Negative (TN)" ~ "Correct Rejections",
      Classification == "False Positive (FP)" ~ "False Alarms",
      Classification == "False Negative (FN)" ~ "Missed"
    ),
    Outcome = factor(Outcome, levels = c("Missed", "False Alarms", "Correct Rejections", "Hits")),
    Label = ifelse(Count == 0, NA, paste0("n = ", Count))
  )
plot_d <- ggplot(all_cm_df, aes(x = Threshold, y = Proportion, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5, color = "black", fontface="bold") +
  facet_wrap(~ Group, ncol = 1) +
  scale_fill_manual(values = classification_colors, name = "Prediction Outcome") +
  labs(
    x = "Exceedance Probability Threshold",
    y = "Proportion of Predictions",
    tag = "B"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  theme_accessible

metrics_df$combined_label <- paste0(  metrics_df$group, " AUC=", metrics_df$auc, ", Brier=", metrics_df$brier)
subtitle_text <- paste(metrics_df$combined_label, collapse = " | ")

finalplot <-(plot_a / plot_d) +
  plot_layout(heights = c(1, 1.8)) + 
  plot_annotation(subtitle = subtitle_text, theme= theme_accessible)
finalplot


# S7 Figure: Utility accuracy assessment for April for different outbreak threshold (75th percentile)
scenario1_forecasts_april <- read.csv("Scripts_Draft2/Outputs/Scenario1_Aprilforecast.csv")
scenario2_forecasts_april <- read.csv( "Scripts_Draft2/Outputs/Scenario2_Aprilforecast.csv")
scenario3_forecasts_april  <-read.csv( "Scripts_Draft2/Outputs/Scenario3_Aprilforecast.csv")
scenario4_forecasts_april <- read.csv( "Scripts_Draft2/Outputs/Scenario4_Aprilforecast.csv")

scenario1_forecasts_april$epi.q75 <- scenario1_forecasts_april$epi.q75_more
scenario4_forecasts_april$epi.q75 <- scenario4_forecasts_april$epi.q75_peak
prob_col <- "probq75"                        # options: "prob1sd", "prob2sd", "probq75", "probq95" OR for scenario 1 "sum_prob1sd", "sum_prob2sd", "sum_probq75", "sum_prob95"
actual_col <- "epi.q75"                       # options: "epi.1d", "epi.2d", "epi.q75", "epi.q95" OR for scenario 1 "epi.1d_more", "epi.2d_more", "epi.q75_more", "epi.q95_more" OR for scenario 4 "epi.1d_peak", "epi.2d_peak", "epi.q75_peak", "epi.q95_peak"
scenario4_forecasts_april <- scenario4_forecasts_april %>%
  group_by(areaid) %>%
  filter(any(consec_exceed_epi.q75_flag == TRUE, na.rm = TRUE)) %>%
  ungroup()
# ROC curves 
subset_dfs <- list(
  "Scenario 1" = scenario1_forecasts_april,
  "Scenario 2" = scenario2_forecasts_april,
  "Scenario 3" = scenario3_forecasts_april,
  "Scenario 4" = scenario4_forecasts_april)
roc_data_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  roc_obj <- roc(df[[actual_col]], df[[prob_col]])
  auc_val <- auc(roc_obj)
  brier_score <- mean((df[[prob_col]] - df[[actual_col]])^2)
  data.frame(
    specificity = rev(roc_obj$specificities),
    sensitivity = rev(roc_obj$sensitivities),
    group = name,
    auc = round(auc_val, 3),
    brier = round(brier_score, 3))})
all_roc_df <- bind_rows(roc_data_list)
metrics_df <- all_roc_df %>%
  group_by(group) %>%
  summarise(
    auc = unique(auc),
    brier = unique(brier)
  ) %>%
  mutate(
    auc_label = paste0(group, " (AUC=", auc, ")"),
    brier_label = paste0(group, " (Brier=", brier, ")")
  )
plot_a <- ggplot(all_roc_df, aes(x = 1 - specificity, y = sensitivity, color = group)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  scale_color_manual(values = custom_accessible_colors) +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate",
    tag= "A",
    color = "Group"
  ) +
  theme_accessible
# Accuracy
thresholds <- seq(0, 1, by = 0.01)
acc_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  acc_df <- lapply(thresholds, function(thresh) {
    preds <- ifelse(df[[prob_col]] >= thresh, 1, 0)
    actuals <- df[[actual_col]]
    accuracy <- mean(preds == actuals)
    data.frame(
      threshold = thresh,
      accuracy = accuracy,
      Group = name)
  }) %>% bind_rows()
  acc_df
})
acc_data <- bind_rows(acc_list)
max_acc_data <- acc_data %>%
  group_by(Group) %>%
  filter(accuracy == max(accuracy)) %>%
  slice(1) %>%       # take the first max occurrence only
  ungroup() %>%
  mutate(Label = paste0(Group, " accuracy max: ", round(accuracy, 3), " at exceedance threshold of ", round(threshold, 2)))
plot_b <- ggplot(acc_data, aes(x = threshold, y = accuracy, color = Group)) +
  geom_line(size = 1.2) +
  geom_segment(data = max_acc_data,
               aes(x = threshold, xend = threshold, y = 0.62, yend = 1, color = Group),
               linetype = "dashed", size = 0.8, show.legend = FALSE) +
  geom_text(data = max_acc_data, aes(x = threshold, y = 0, label = Label, color = Group),
            angle = 270,
            vjust = 0.2,
            hjust = 1,
            size = 3.5,
            show.legend = FALSE,
            position = position_nudge(x = c(-0.01, 0.01, 0, 0))) +
  scale_color_manual(values = custom_accessible_colors) +
  labs(x = "Probability Threshold", y = "Accuracy", color = "Scenario", tag= "A" ) +
  theme_minimal(base_size = 14) +
  theme_accessible
#CONF MATRICES
thresholds <- c(0.25, 0.5, 0.75)
group_names <- names(subset_dfs)
confmat_plot_data <- list()
for (group_name in group_names) {
  df <- subset_dfs[[group_name]]
  for (thresh in thresholds) {
    temp_df <- df %>%
      mutate(
        Predicted = ifelse(.data[[prob_col]] >= thresh, 1, 0),
        Actual = .data[[actual_col]]
      )
    cm <- confusionMatrix(
      as.factor(temp_df$Predicted),
      as.factor(temp_df$Actual),
      positive = "1"
    )
    cm_df <- as.data.frame(cm$table)
    colnames(cm_df) <- c("Predicted", "Actual", "Count")
    cm_df <- cm_df %>%
      mutate(
        Classification = case_when(
          Predicted == Actual & Predicted == 1 ~ "True Positive (TP)",
          Predicted == Actual & Predicted == 0 ~ "True Negative (TN)",
          Predicted != Actual & Predicted == 1 ~ "False Positive (FP)",
          Predicted != Actual & Predicted == 0 ~ "False Negative (FN)"
        ),
        FillColor = ifelse(Classification %in% c("True Positive (TP)", "True Negative (TN)"),
                           "Correct", "Incorrect"),
        Group = group_name,
        Threshold = paste0("Exceedance Probability \u2265 ", thresh)
      ) %>%
      mutate(Proportion = Count / sum(Count)) %>%
      mutate(
        Percent_Label = paste0(round(Proportion * 100, 1), "%"),
        Full_Label = case_when(
          Classification == "False Positive (FP)" ~ paste("False Alarms:", "\n", Percent_Label,  "(n = ", Count, ")"),
          Classification == "False Negative (FN)" ~ paste("Missed Actions:", "\n", Percent_Label,  "(n = ", Count, ")"),
          Classification == "True Positive (TP)" ~ paste("Correct Action:",  "\n", Percent_Label, "(n = ", Count, ")"),
          Classification == "True Negative (TN)" ~ paste("Correct No Action:", "\n", Percent_Label, "(n = ", Count, ")")
        ))
    confmat_plot_data[[length(confmat_plot_data) + 1]] <- cm_df
  }
}
all_cm_df <- bind_rows(confmat_plot_data)
color_scale <- scale_fill_manual(
  values = c("Correct" = correct_colour, "Incorrect" = incorrect_colour),
  name = "Prediction Accuracy"
)
plot_c <- ggplot(all_cm_df, aes(x = Predicted, y = Actual)) +
  geom_tile(color = "black", fill = NA, linewidth = 0.8) +
  geom_point(aes(size = Proportion, fill = FillColor), shape = 21, color = "black") +
  geom_text(aes(label = Full_Label), size = 3, lineheight = 0.8) +
  color_scale +
  scale_size(range = c(10, 40), guide = "none") +
  facet_grid(Group ~ Threshold) +
  labs(
    x = "Predicted Exceedance",
    y = "Observed Exceedance", 
    tag= "B"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  ) + 
  guides(
    fill = guide_legend(
      override.aes = list(size = 6),  # size of points in legend
      keywidth = unit(1.5, "cm"),     # width of legend keys
      keyheight = unit(1.5, "cm")     # height of legend keys
    )) +
  theme_accessible
# Bar plots showing prediction outcomes
all_cm_df <- all_cm_df %>%
  mutate(
    Outcome = case_when(
      Classification == "True Positive (TP)" ~ "Hits",
      Classification == "True Negative (TN)" ~ "Correct Rejections",
      Classification == "False Positive (FP)" ~ "False Alarms",
      Classification == "False Negative (FN)" ~ "Missed"
    ),
    Outcome = factor(Outcome, levels = c("Missed", "False Alarms", "Correct Rejections", "Hits")),
    Label = ifelse(Count == 0, NA, paste0("n = ", Count))
  )
plot_d <- ggplot(all_cm_df, aes(x = Threshold, y = Proportion, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5, color = "black", fontface="bold") +
  facet_wrap(~ Group, ncol = 1) +
  scale_fill_manual(values = classification_colors, name = "Prediction Outcome") +
  labs(
    x = "Exceedance Probability Threshold",
    y = "Proportion of Predictions",
    tag = "B"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  theme_accessible
metrics_df$combined_label <- paste0(  metrics_df$group, " AUC=", metrics_df$auc, ", Brier=", metrics_df$brier)
subtitle_text <- paste(metrics_df$combined_label, collapse = " | ")

finalplot <-(plot_a / plot_d) +
  plot_layout(heights = c(1, 1.8)) + 
  plot_annotation(subtitle = subtitle_text, theme= theme_accessible)
finalplot


# S8 Figure: Utility accuracy assessment for April for different outbreak threshold (95th percentile)
scenario1_forecasts_april <- read.csv("Scripts_Draft2/Outputs/Scenario1_Aprilforecast.csv")
scenario2_forecasts_april <- read.csv( "Scripts_Draft2/Outputs/Scenario2_Aprilforecast.csv")
scenario3_forecasts_april  <-read.csv( "Scripts_Draft2/Outputs/Scenario3_Aprilforecast.csv")
scenario4_forecasts_april <- read.csv( "Scripts_Draft2/Outputs/Scenario4_Aprilforecast.csv")

scenario1_forecasts_april$epi.q95 <- scenario1_forecasts_april$epi.q95_more
scenario4_forecasts_april$epi.q95 <- scenario4_forecasts_april$epi.q95_peak
prob_col <- "probq95"                        # options: "prob1sd", "prob2sd", "probq75", "probq95" OR for scenario 1 "sum_prob1sd", "sum_prob2sd", "sum_probq75", "sum_prob95"
actual_col <- "epi.q95"                       # options: "epi.1d", "epi.2d", "epi.q75", "epi.q95" OR for scenario 1 "epi.1d_more", "epi.2d_more", "epi.q75_more", "epi.q95_more" OR for scenario 4 "epi.1d_peak", "epi.2d_peak", "epi.q75_peak", "epi.q95_peak"
scenario4_forecasts_april <- scenario4_forecasts_april %>%
  group_by(areaid) %>%
  filter(any(consec_exceed_epi.q95_flag == TRUE, na.rm = TRUE)) %>%
  ungroup()
# ROC curves 
subset_dfs <- list(
  "Scenario 1" = scenario1_forecasts_april,
  "Scenario 2" = scenario2_forecasts_april,
  "Scenario 3" = scenario3_forecasts_april,
  "Scenario 4" = scenario4_forecasts_april)
roc_data_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  roc_obj <- roc(df[[actual_col]], df[[prob_col]])
  auc_val <- auc(roc_obj)
  brier_score <- mean((df[[prob_col]] - df[[actual_col]])^2)
  data.frame(
    specificity = rev(roc_obj$specificities),
    sensitivity = rev(roc_obj$sensitivities),
    group = name,
    auc = round(auc_val, 3),
    brier = round(brier_score, 3))})
all_roc_df <- bind_rows(roc_data_list)
metrics_df <- all_roc_df %>%
  group_by(group) %>%
  summarise(
    auc = unique(auc),
    brier = unique(brier)
  ) %>%
  mutate(
    auc_label = paste0(group, " (AUC=", auc, ")"),
    brier_label = paste0(group, " (Brier=", brier, ")")
  )
plot_a <- ggplot(all_roc_df, aes(x = 1 - specificity, y = sensitivity, color = group)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  scale_color_manual(values = custom_accessible_colors) +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate",
    tag= "A",
    color = "Group"
  ) +
  theme_accessible
# Accuracy
thresholds <- seq(0, 1, by = 0.01)
acc_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  acc_df <- lapply(thresholds, function(thresh) {
    preds <- ifelse(df[[prob_col]] >= thresh, 1, 0)
    actuals <- df[[actual_col]]
    accuracy <- mean(preds == actuals)
    data.frame(
      threshold = thresh,
      accuracy = accuracy,
      Group = name)
  }) %>% bind_rows()
  acc_df
})
acc_data <- bind_rows(acc_list)
max_acc_data <- acc_data %>%
  group_by(Group) %>%
  filter(accuracy == max(accuracy)) %>%
  slice(1) %>%       # take the first max occurrence only
  ungroup() %>%
  mutate(Label = paste0(Group, " accuracy max: ", round(accuracy, 3), " at exceedance threshold of ", round(threshold, 2)))
plot_b <- ggplot(acc_data, aes(x = threshold, y = accuracy, color = Group)) +
  geom_line(size = 1.2) +
  geom_segment(data = max_acc_data,
               aes(x = threshold, xend = threshold, y = 0.62, yend = 1, color = Group),
               linetype = "dashed", size = 0.8, show.legend = FALSE) +
  geom_text(data = max_acc_data, aes(x = threshold, y = 0, label = Label, color = Group),
            angle = 270,
            vjust = 0.2,
            hjust = 1,
            size = 3.5,
            show.legend = FALSE,
            position = position_nudge(x = c(-0.01, 0.01, 0, 0))) +
  scale_color_manual(values = custom_accessible_colors) +
  labs(x = "Probability Threshold", y = "Accuracy", color = "Scenario", tag= "A" ) +
  theme_minimal(base_size = 14) +
  theme_accessible
#CONF MATRICES
thresholds <- c(0.25, 0.5, 0.75)
group_names <- names(subset_dfs)
confmat_plot_data <- list()
for (group_name in group_names) {
  df <- subset_dfs[[group_name]]
  for (thresh in thresholds) {
    temp_df <- df %>%
      mutate(
        Predicted = ifelse(.data[[prob_col]] >= thresh, 1, 0),
        Actual = .data[[actual_col]]
      )
    cm <- confusionMatrix(
      as.factor(temp_df$Predicted),
      as.factor(temp_df$Actual),
      positive = "1"
    )
    cm_df <- as.data.frame(cm$table)
    colnames(cm_df) <- c("Predicted", "Actual", "Count")
    cm_df <- cm_df %>%
      mutate(
        Classification = case_when(
          Predicted == Actual & Predicted == 1 ~ "True Positive (TP)",
          Predicted == Actual & Predicted == 0 ~ "True Negative (TN)",
          Predicted != Actual & Predicted == 1 ~ "False Positive (FP)",
          Predicted != Actual & Predicted == 0 ~ "False Negative (FN)"
        ),
        FillColor = ifelse(Classification %in% c("True Positive (TP)", "True Negative (TN)"),
                           "Correct", "Incorrect"),
        Group = group_name,
        Threshold = paste0("Exceedance Probability \u2265 ", thresh)
      ) %>%
      mutate(Proportion = Count / sum(Count)) %>%
      mutate(
        Percent_Label = paste0(round(Proportion * 100, 1), "%"),
        Full_Label = case_when(
          Classification == "False Positive (FP)" ~ paste("False Alarms:", "\n", Percent_Label,  "(n = ", Count, ")"),
          Classification == "False Negative (FN)" ~ paste("Missed Actions:", "\n", Percent_Label,  "(n = ", Count, ")"),
          Classification == "True Positive (TP)" ~ paste("Correct Action:",  "\n", Percent_Label, "(n = ", Count, ")"),
          Classification == "True Negative (TN)" ~ paste("Correct No Action:", "\n", Percent_Label, "(n = ", Count, ")")
        ))
    confmat_plot_data[[length(confmat_plot_data) + 1]] <- cm_df
  }
}
all_cm_df <- bind_rows(confmat_plot_data)
color_scale <- scale_fill_manual(
  values = c("Correct" = correct_colour, "Incorrect" = incorrect_colour),
  name = "Prediction Accuracy"
)
plot_c <- ggplot(all_cm_df, aes(x = Predicted, y = Actual)) +
  geom_tile(color = "black", fill = NA, linewidth = 0.8) +
  geom_point(aes(size = Proportion, fill = FillColor), shape = 21, color = "black") +
  geom_text(aes(label = Full_Label), size = 3, lineheight = 0.8) +
  color_scale +
  scale_size(range = c(10, 40), guide = "none") +
  facet_grid(Group ~ Threshold) +
  labs(
    x = "Predicted Exceedance",
    y = "Observed Exceedance", 
    tag= "B"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  ) + 
  guides(
    fill = guide_legend(
      override.aes = list(size = 6),  # size of points in legend
      keywidth = unit(1.5, "cm"),     # width of legend keys
      keyheight = unit(1.5, "cm")     # height of legend keys
    )) +
  theme_accessible
# Bar plots showing prediction outcomes
all_cm_df <- all_cm_df %>%
  mutate(
    Outcome = case_when(
      Classification == "True Positive (TP)" ~ "Hits",
      Classification == "True Negative (TN)" ~ "Correct Rejections",
      Classification == "False Positive (FP)" ~ "False Alarms",
      Classification == "False Negative (FN)" ~ "Missed"
    ),
    Outcome = factor(Outcome, levels = c("Missed", "False Alarms", "Correct Rejections", "Hits")),
    Label = ifelse(Count == 0, NA, paste0("n = ", Count))
  )
plot_d <- ggplot(all_cm_df, aes(x = Threshold, y = Proportion, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5, color = "black", fontface="bold") +
  facet_wrap(~ Group, ncol = 1) +
  scale_fill_manual(values = classification_colors, name = "Prediction Outcome") +
  labs(
    x = "Exceedance Probability Threshold",
    y = "Proportion of Predictions",
    tag = "B"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  theme_accessible
metrics_df$combined_label <- paste0(  metrics_df$group, " AUC=", metrics_df$auc, ", Brier=", metrics_df$brier)
subtitle_text <- paste(metrics_df$combined_label, collapse = " | ")

finalplot <-(plot_a / plot_d) +
  plot_layout(heights = c(1, 1.8)) + 
  plot_annotation(subtitle = subtitle_text, theme= theme_accessible)
finalplot

# S9 Figure: Utility accuracy assessment for April for different outbreak threshold (mean plus one standard deviation)
scenario1_forecasts_april <- read.csv("Scripts_Draft2/Outputs/Scenario1_Aprilforecast.csv")
scenario2_forecasts_april <- read.csv( "Scripts_Draft2/Outputs/Scenario2_Aprilforecast.csv")
scenario3_forecasts_april  <-read.csv( "Scripts_Draft2/Outputs/Scenario3_Aprilforecast.csv")
scenario4_forecasts_april <- read.csv( "Scripts_Draft2/Outputs/Scenario4_Aprilforecast.csv")

scenario1_forecasts_april$epi.1d <- scenario1_forecasts_april$epi.1d_more
scenario4_forecasts_april$epi.1d <- scenario4_forecasts_april$epi.1d_peak
prob_col <- "prob1sd"                        # options: "prob1sd", "prob2sd", "probq75", "probq95" OR for scenario 1 "sum_prob1sd", "sum_prob2sd", "sum_probq75", "sum_prob95"
actual_col <- "epi.1d"                       # options: "epi.1d", "epi.2d", "epi.q75", "epi.q95" OR for scenario 1 "epi.1d_more", "epi.2d_more", "epi.q75_more", "epi.q95_more" OR for scenario 4 "epi.1d_peak", "epi.2d_peak", "epi.q75_peak", "epi.q95_peak"
scenario4_forecasts_april <- scenario4_forecasts_april %>%
  group_by(areaid) %>%
  filter(any(consec_exceed_epi.1d_flag == TRUE, na.rm = TRUE)) %>%
  ungroup()
# ROC curves 
subset_dfs <- list(
  "Scenario 1" = scenario1_forecasts_april,
  "Scenario 2" = scenario2_forecasts_april,
  "Scenario 3" = scenario3_forecasts_april,
  "Scenario 4" = scenario4_forecasts_april)
roc_data_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  roc_obj <- roc(df[[actual_col]], df[[prob_col]])
  auc_val <- auc(roc_obj)
  brier_score <- mean((df[[prob_col]] - df[[actual_col]])^2)
  data.frame(
    specificity = rev(roc_obj$specificities),
    sensitivity = rev(roc_obj$sensitivities),
    group = name,
    auc = round(auc_val, 3),
    brier = round(brier_score, 3))})
all_roc_df <- bind_rows(roc_data_list)
metrics_df <- all_roc_df %>%
  group_by(group) %>%
  summarise(
    auc = unique(auc),
    brier = unique(brier)
  ) %>%
  mutate(
    auc_label = paste0(group, " (AUC=", auc, ")"),
    brier_label = paste0(group, " (Brier=", brier, ")")
  )
plot_a <- ggplot(all_roc_df, aes(x = 1 - specificity, y = sensitivity, color = group)) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  scale_color_manual(values = custom_accessible_colors) +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate",
    tag= "A",
    color = "Group"
  ) +
  theme_accessible
# Accuracy
thresholds <- seq(0, 1, by = 0.01)
acc_list <- lapply(names(subset_dfs), function(name) {
  df <- subset_dfs[[name]]
  acc_df <- lapply(thresholds, function(thresh) {
    preds <- ifelse(df[[prob_col]] >= thresh, 1, 0)
    actuals <- df[[actual_col]]
    accuracy <- mean(preds == actuals)
    data.frame(
      threshold = thresh,
      accuracy = accuracy,
      Group = name)
  }) %>% bind_rows()
  acc_df
})
acc_data <- bind_rows(acc_list)
max_acc_data <- acc_data %>%
  group_by(Group) %>%
  filter(accuracy == max(accuracy)) %>%
  slice(1) %>%       # take the first max occurrence only
  ungroup() %>%
  mutate(Label = paste0(Group, " accuracy max: ", round(accuracy, 3), " at exceedance threshold of ", round(threshold, 2)))
plot_b <- ggplot(acc_data, aes(x = threshold, y = accuracy, color = Group)) +
  geom_line(size = 1.2) +
  geom_segment(data = max_acc_data,
               aes(x = threshold, xend = threshold, y = 0.62, yend = 1, color = Group),
               linetype = "dashed", size = 0.8, show.legend = FALSE) +
  geom_text(data = max_acc_data, aes(x = threshold, y = 0, label = Label, color = Group),
            angle = 270,
            vjust = 0.2,
            hjust = 1,
            size = 3.5,
            show.legend = FALSE,
            position = position_nudge(x = c(-0.01, 0.01, 0, 0))) +
  scale_color_manual(values = custom_accessible_colors) +
  labs(x = "Probability Threshold", y = "Accuracy", color = "Scenario", tag= "A" ) +
  theme_minimal(base_size = 14) +
  theme_accessible
#CONF MATRICES
thresholds <- c(0.25, 0.5, 0.75)
group_names <- names(subset_dfs)
confmat_plot_data <- list()
for (group_name in group_names) {
  df <- subset_dfs[[group_name]]
  for (thresh in thresholds) {
    temp_df <- df %>%
      mutate(
        Predicted = ifelse(.data[[prob_col]] >= thresh, 1, 0),
        Actual = .data[[actual_col]]
      )
    cm <- confusionMatrix(
      as.factor(temp_df$Predicted),
      as.factor(temp_df$Actual),
      positive = "1"
    )
    cm_df <- as.data.frame(cm$table)
    colnames(cm_df) <- c("Predicted", "Actual", "Count")
    cm_df <- cm_df %>%
      mutate(
        Classification = case_when(
          Predicted == Actual & Predicted == 1 ~ "True Positive (TP)",
          Predicted == Actual & Predicted == 0 ~ "True Negative (TN)",
          Predicted != Actual & Predicted == 1 ~ "False Positive (FP)",
          Predicted != Actual & Predicted == 0 ~ "False Negative (FN)"
        ),
        FillColor = ifelse(Classification %in% c("True Positive (TP)", "True Negative (TN)"),
                           "Correct", "Incorrect"),
        Group = group_name,
        Threshold = paste0("Exceedance Probability \u2265 ", thresh)
      ) %>%
      mutate(Proportion = Count / sum(Count)) %>%
      mutate(
        Percent_Label = paste0(round(Proportion * 100, 1), "%"),
        Full_Label = case_when(
          Classification == "False Positive (FP)" ~ paste("False Alarms:", "\n", Percent_Label,  "(n = ", Count, ")"),
          Classification == "False Negative (FN)" ~ paste("Missed Actions:", "\n", Percent_Label,  "(n = ", Count, ")"),
          Classification == "True Positive (TP)" ~ paste("Correct Action:",  "\n", Percent_Label, "(n = ", Count, ")"),
          Classification == "True Negative (TN)" ~ paste("Correct No Action:", "\n", Percent_Label, "(n = ", Count, ")")
        ))
    confmat_plot_data[[length(confmat_plot_data) + 1]] <- cm_df
  }
}
all_cm_df <- bind_rows(confmat_plot_data)
color_scale <- scale_fill_manual(
  values = c("Correct" = correct_colour, "Incorrect" = incorrect_colour),
  name = "Prediction Accuracy"
)
plot_c <- ggplot(all_cm_df, aes(x = Predicted, y = Actual)) +
  geom_tile(color = "black", fill = NA, linewidth = 0.8) +
  geom_point(aes(size = Proportion, fill = FillColor), shape = 21, color = "black") +
  geom_text(aes(label = Full_Label), size = 3, lineheight = 0.8) +
  color_scale +
  scale_size(range = c(10, 40), guide = "none") +
  facet_grid(Group ~ Threshold) +
  labs(
    x = "Predicted Exceedance",
    y = "Observed Exceedance", 
    tag= "B"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  ) + 
  guides(
    fill = guide_legend(
      override.aes = list(size = 6),  # size of points in legend
      keywidth = unit(1.5, "cm"),     # width of legend keys
      keyheight = unit(1.5, "cm")     # height of legend keys
    )) +
  theme_accessible
# Bar plots showing prediction outcomes
all_cm_df <- all_cm_df %>%
  mutate(
    Outcome = case_when(
      Classification == "True Positive (TP)" ~ "Hits",
      Classification == "True Negative (TN)" ~ "Correct Rejections",
      Classification == "False Positive (FP)" ~ "False Alarms",
      Classification == "False Negative (FN)" ~ "Missed"
    ),
    Outcome = factor(Outcome, levels = c("Missed", "False Alarms", "Correct Rejections", "Hits")),
    Label = ifelse(Count == 0, NA, paste0("n = ", Count))
  )
plot_d <- ggplot(all_cm_df, aes(x = Threshold, y = Proportion, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5, color = "black", fontface="bold") +
  facet_wrap(~ Group, ncol = 1) +
  scale_fill_manual(values = classification_colors, name = "Prediction Outcome") +
  labs(
    x = "Exceedance Probability Threshold",
    y = "Proportion of Predictions",
    tag = "B"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  theme_accessible
metrics_df$combined_label <- paste0(  metrics_df$group, " AUC=", metrics_df$auc, ", Brier=", metrics_df$brier)
subtitle_text <- paste(metrics_df$combined_label, collapse = " | ")

finalplot <-(plot_a / plot_d) +
  plot_layout(heights = c(1, 1.8)) + 
  plot_annotation(subtitle = subtitle_text, theme= theme_accessible)
finalplot


# S10 Figure: Spatial accuracy of utility results using a different threshold (q75)
threshold <- 0.5
scenario1_forecasts_april <- read.csv("Scripts_Draft2/Outputs/Scenario1_Aprilforecast.csv")
scenario2_forecasts_april <- read.csv( "Scripts_Draft2/Outputs/Scenario2_Aprilforecast.csv")
scenario3_forecasts_april  <-read.csv( "Scripts_Draft2/Outputs/Scenario3_Aprilforecast.csv")
scenario4_forecasts_april <- read.csv( "Scripts_Draft2/Outputs/Scenario4_Aprilforecast.csv")

scenario1_forecasts_april <- provinces %>%  left_join(scenario1_forecasts_april, by = "areaid")
scenario2_forecasts_april <- provinces %>%  left_join(scenario2_forecasts_april, by = "areaid")
scenario3_forecasts_april <- provinces %>%  left_join(scenario3_forecasts_april, by = "areaid")
scenario4_forecasts_april <- provinces %>%  left_join(scenario4_forecasts_april, by = "areaid")

scenario1_forecasts_april$epi.q75 <- scenario1_forecasts_april$epi.q75_more
scenario4_forecasts_april$epi.q75 <- scenario4_forecasts_april$epi.q75_peak
prob_col <- "probq75"                        # options: "prob1sd", "prob2sd", "probq75", "probq95" OR for scenario 1 "sum_prob1sd", "sum_prob2sd", "sum_probq75", "sum_prob95"
actual_col <- "epi.q75"                       # options: "epi.1d", "epi.2d", "epi.q75", "epi.q95" OR for scenario 1 "epi.1d_more", "epi.2d_more", "epi.q75_more", "epi.q95_more" OR for scenario 4 "epi.1d_peak", "epi.2d_peak", "epi.q75_peak", "epi.q95_peak"


scenario1_forecasts_april_outcomes <- scenario1_forecasts_april %>%
  mutate(prediction = ifelse(probq75 >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.q75 == 1 ~ "True Positive",
           prediction == 1 & epi.q75 == 0 ~ "False Positive",
           prediction == 0 & epi.q75 == 1 ~ "False Negative",
           prediction == 0 & epi.q75 == 0 ~ "True Negative"))
scenario1_plot <- ggplot(scenario1_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 1\nPrediction Outcome" 
  ) +
  theme_minimal(base_size = 14) +
  labs(tag="A") +
  theme_accessible

scenario2_forecasts_april_outcomes <- scenario2_forecasts_april %>%
  mutate(prediction = ifelse(probq75 >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.q75 == 1 ~ "True Positive",
           prediction == 1 & epi.q75 == 0 ~ "False Positive",
           prediction == 0 & epi.q75 == 1 ~ "False Negative",
           prediction == 0 & epi.q75 == 0 ~ "True Negative"))
scenario2_plot <- ggplot(scenario2_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 2\nPrediction Outcome"  
  ) +
  theme_minimal(base_size = 14) +
  #labs(tag="B") +
  theme_accessible

scenario3_forecasts_april_outcomes <- scenario3_forecasts_april %>%
  mutate(prediction = ifelse(probq75 >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.q75 == 1 ~ "True Positive",
           prediction == 1 & epi.q75 == 0 ~ "False Positive",
           prediction == 0 & epi.q75 == 1 ~ "False Negative",
           prediction == 0 & epi.q75 == 0 ~ "True Negative"))
scenario3_plot <- ggplot(scenario3_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 3\nPrediction Outcome"  
  ) +
  theme_minimal(base_size = 14) +
  #labs(tag="C") +
  theme_accessible

scenario4_forecasts_april_outcomes <- scenario4_forecasts_april %>%
  mutate(prediction = ifelse(probq75 >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.q75 == 1 ~ "True Positive",
           prediction == 1 & epi.q75 == 0 ~ "False Positive",
           prediction == 0 & epi.q75 == 1 ~ "False Negative",
           prediction == 0 & epi.q75 == 0 ~ "True Negative"),
         outcome = case_when(consec_exceed_epi.q75_flag == FALSE ~ NA, TRUE ~ outcome))
scenario4_plot <- ggplot(scenario4_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 4\nPrediction Outcome" 
  ) +
  theme_minimal(base_size = 14) +
  #labs(tag="D") +
  theme_accessible

scenario1_forecasts_april$epi.q95 <- scenario1_forecasts_april$epi.q95_more
scenario4_forecasts_april$epi.q95 <- scenario4_forecasts_april$epi.q95_peak
prob_col <- "probq95"                        # options: "prob1sd", "prob2sd", "probq75", "probq95" OR for scenario 1 "sum_prob1sd", "sum_prob2sd", "sum_probq75", "sum_prob95"
actual_col <- "epi.q95"                       # options: "epi.1d", "epi.2d", "epi.q75", "epi.q95" OR for scenario 1 "epi.1d_more", "epi.2d_more", "epi.q75_more", "epi.q95_more" OR for scenario 4 "epi.1d_peak", "epi.2d_peak", "epi.q75_peak", "epi.q95_peak"

scenario1_forecasts_april_outcomes <- scenario1_forecasts_april %>%
  mutate(prediction = ifelse(probq95 >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.q95 == 1 ~ "True Positive",
           prediction == 1 & epi.q95 == 0 ~ "False Positive",
           prediction == 0 & epi.q95 == 1 ~ "False Negative",
           prediction == 0 & epi.q95 == 0 ~ "True Negative"))
scenario1_plot_95 <- ggplot(scenario1_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 1\nPrediction Outcome"
  ) +
  theme_minimal(base_size = 14) +
  labs(tag="B") +
  theme_accessible

scenario2_forecasts_april_outcomes <- scenario2_forecasts_april %>%
  mutate(prediction = ifelse(probq95 >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.q95 == 1 ~ "True Positive",
           prediction == 1 & epi.q95 == 0 ~ "False Positive",
           prediction == 0 & epi.q95 == 1 ~ "False Negative",
           prediction == 0 & epi.q95 == 0 ~ "True Negative"))
scenario2_plot_95 <- ggplot(scenario2_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 2\nPrediction Outcome"  
  ) +
  theme_minimal(base_size = 14) +
  #labs(tag="B") +
  theme_accessible

scenario3_forecasts_april_outcomes <- scenario3_forecasts_april %>%
  mutate(prediction = ifelse(probq95 >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.q95 == 1 ~ "True Positive",
           prediction == 1 & epi.q95 == 0 ~ "False Positive",
           prediction == 0 & epi.q95 == 1 ~ "False Negative",
           prediction == 0 & epi.q95 == 0 ~ "True Negative"))
scenario3_plot_95 <- ggplot(scenario3_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 3\nPrediction Outcome" 
  ) +
  theme_minimal(base_size = 14) +
  #labs(tag="C") +
  theme_accessible

scenario4_forecasts_april_outcomes <- scenario4_forecasts_april %>%
  mutate(prediction = ifelse(probq95 >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.q95 == 1 ~ "True Positive",
           prediction == 1 & epi.q95 == 0 ~ "False Positive",
           prediction == 0 & epi.q95 == 1 ~ "False Negative",
           prediction == 0 & epi.q95 == 0 ~ "True Negative"),
         outcome = case_when(consec_exceed_epi.q95_flag == FALSE ~ NA, TRUE ~ outcome))
scenario4_plot_95 <- ggplot(scenario4_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 4\nPrediction Outcome"  
  ) +
  theme_minimal(base_size = 14) +
  #labs(tag="D") +
  theme_accessible



scenario1_forecasts_april$epi.1d <- scenario1_forecasts_april$epi.1d_more
scenario4_forecasts_april$epi.1d <- scenario4_forecasts_april$epi.1d_peak
prob_col <- "prob1sd"                        # options: "prob1sd", "prob2sd", "probq75", "probq95" OR for scenario 1 "sum_prob1sd", "sum_prob2sd", "sum_probq75", "sum_prob95"
actual_col <- "epi.1d"                       # options: "epi.1d", "epi.2d", "epi.q75", "epi.q95" OR for scenario 1 "epi.1d_more", "epi.2d_more", "epi.q75_more", "epi.q95_more" OR for scenario 4 "epi.1d_peak", "epi.2d_peak", "epi.q75_peak", "epi.q95_peak"

scenario1_forecasts_april_outcomes <- scenario1_forecasts_april %>%
  mutate(prediction = ifelse(prob1sd >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.1d == 1 ~ "True Positive",
           prediction == 1 & epi.1d == 0 ~ "False Positive",
           prediction == 0 & epi.1d == 1 ~ "False Negative",
           prediction == 0 & epi.1d == 0 ~ "True Negative"))
scenario1_plot_1d <- ggplot(scenario1_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 1\nPrediction Outcome" 
  ) +
  theme_minimal(base_size = 14) +
  labs(tag="C") +
  theme_accessible

scenario2_forecasts_april_outcomes <- scenario2_forecasts_april %>%
  mutate(prediction = ifelse(prob1sd >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.1d == 1 ~ "True Positive",
           prediction == 1 & epi.1d == 0 ~ "False Positive",
           prediction == 0 & epi.1d == 1 ~ "False Negative",
           prediction == 0 & epi.1d == 0 ~ "True Negative"))
scenario2_plot_1d <- ggplot(scenario2_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 2\nPrediction Outcome" 
  ) +
  theme_minimal(base_size = 14) +
  #labs(tag="B") +
  theme_accessible

scenario3_forecasts_april_outcomes <- scenario3_forecasts_april %>%
  mutate(prediction = ifelse(prob1sd >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.1d == 1 ~ "True Positive",
           prediction == 1 & epi.1d == 0 ~ "False Positive",
           prediction == 0 & epi.1d == 1 ~ "False Negative",
           prediction == 0 & epi.1d == 0 ~ "True Negative"))
scenario3_plot_1d <- ggplot(scenario3_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 3\nPrediction Outcome" 
  ) +
  theme_minimal(base_size = 14) +
  #labs(tag="C") +
  theme_accessible

scenario4_forecasts_april_outcomes <- scenario4_forecasts_april %>%
  mutate(prediction = ifelse(prob1sd >= threshold, 1, 0),
         outcome = case_when(
           prediction == 1 & epi.1d == 1 ~ "True Positive",
           prediction == 1 & epi.1d == 0 ~ "False Positive",
           prediction == 0 & epi.1d == 1 ~ "False Negative",
           prediction == 0 & epi.1d == 0 ~ "True Negative"),
         outcome = case_when(consec_exceed_epi.1d_flag == FALSE ~ NA, TRUE ~ outcome))
scenario4_plot_1d <- ggplot(scenario4_forecasts_april_outcomes) +
  geom_sf(aes(fill = outcome), color = "grey90") +
  scale_fill_manual(
    values = outcome_colors,
    name = "Scenario 4\nPrediction Outcome" 
  ) +
  theme_minimal(base_size = 14) +
  #labs(tag="D") +
  theme_accessible

grid.arrange(scenario1_plot, scenario2_plot, scenario3_plot, scenario4_plot, scenario1_plot_95, scenario2_plot_95, scenario3_plot_95, scenario4_plot_95, scenario1_plot_1d, scenario2_plot_1d, scenario3_plot_1d, scenario4_plot_1d, 
             ncol = 2, widths = c(1, 1))
