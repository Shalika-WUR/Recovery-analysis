#the names were processed in the csv files directly
#eread in teh file
#####identifying production shocks
#######################################method 3 Cottrell et al using Loess
library(dplyr)
library(zoo)

#rm previous files
rm(rolling_baseline_data)
rm(loess_model)
rm(Fitted_Production)
rm(maize_data)
rm(single_country_data)

# Assuming your data frame is named 'maize_data'
maize_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAOStat_Maize.csv")
colnames(maize_data)

#sum stats
summary(maize_data$Value)
#by region
summary_stats_by_region <- maize_data %>%
  group_by(Subregion) %>%
  summarise(
    Count = n(),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Print the summary statistics
print(summary_stats_by_region)

# Specify the path and file name for your output CSV
output_file_path <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/sumstatmaizebyregion.csv"
# Export the tibble to a CSV file
write.csv(summary_stats_by_region, output_file_path, row.names = FALSE)


# Create the box plot
library(ggplot2)
ggplot(maize_data, aes(y = Value)) +
  geom_boxplot() +
  labs(title = "Maize production", y = "Value", x = "") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 3e6))  # Zooming in without removing data points

# Assuming your data frame is named 'maize_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(maize_data) {
  return(maize_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 4, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.6)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(maize_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

# Now, all column names have the "_weather" suffix
print(colnames(rolling_baseline_data))

#save the file
write.csv(rolling_baseline_data, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Prod_loess_maize_global.csv", row.names = T)

#fix loess span and cooks distance values, inspect the fitted reg through plots
# Load required packages
#supplementary graphs
#fix loess span and cooks distance values, inspect the fitted reg through plots-you need to loop through for all the countries
####loop over all the countries
library(ggplot2)
library(gridExtra)

# Get unique list of country ISO codes from the data
unique_countries <- unique(rolling_baseline_data$ISO_prod)

# Loop over each country ISO code
for (country_code in unique_countries) {
  # Filter the data for the current country
  country_data <- subset(rolling_baseline_data, ISO_prod == country_code)
  
  # Plot a - Production and LOESS fit with shocks indicated by green triangles
  plot_a <- ggplot(country_data, aes(x = Year_prod, y = Value_prod)) +
    geom_point(size = 2, alpha = 0.6) +
    geom_line(aes(y = Loess_Fitted_Production_prod), color = "blue", size = 1) +
    geom_point(data = subset(country_data, Shock_Point_prod == TRUE), aes(y = Value_prod), shape = 17, color = "green", size = 3) +
    labs(title = paste("Production Over Time with LOESS Fit -", country_code), subtitle = "Green triangles indicate shocks") +
    theme_minimal()
  
  # Plot b - Residuals against lag-1 residuals with shocks
  plot_b <- ggplot(country_data, aes(x = Lag1_Residuals_prod, y = Residuals_prod)) +
    geom_point(color = "blue", size = 2, alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_point(data = subset(country_data, Shock_Point_prod == TRUE), aes(x = Lag1_Residuals_prod, y = Residuals_prod), color = "green", shape = 17, size = 3) +
    geom_text(data = subset(country_data, Shock_Point_prod == TRUE), aes(label = paste("Cook's D:", round(Cooksd_prod, 2))), vjust = -0.5) +
    labs(title = paste("Residuals vs Lag-1 Residuals for", country_code)) +
    theme_minimal()
  
  # Plot c - Cook's distance over time with threshold line and shocks indicated by green triangles
  plot_c <- ggplot(country_data, aes(x = Year_prod)) +
    geom_line(aes(y = Cooksd_prod), color = "black") +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "red") +
    geom_point(data = subset(country_data, Shock_Point_prod == TRUE), aes(y = Cooksd_prod), shape = 17, color = "green", size = 3) +
    labs(title = paste("Cook's Distance Over Time -", country_code)) +
    theme_minimal()
  
  # Combine the plots into a single image
  multi_plot <- marrangeGrob(list(plot_a, plot_b, plot_c), ncol = 1, nrow = 3, top = paste("Country:", country_code))
  
  # Define the file path for the PNG output
  file_path <- file.path("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Timeseries_plots/Maize", paste0(country_code, "_timeseries_plots.png"))
  
  # Save the combined plot to a PNG file
  ggsave(file_path, multi_plot, width = 8, height = 12, dpi = 300)
}

#now save all the images into a doc
library(officer)
library(magrittr)

# Path to the folder with PNG files
folder_path <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Timeseries_plots/Maize/"

# Create a list of all PNG files
png_files <- list.files(path = folder_path, pattern = "*.png", full.names = TRUE)

# Create a new Word document
doc <- read_docx()

# Loop over PNG files and add them to the document
for (file_path in png_files) {
  doc <- doc %>%
    body_add_img(src = file_path, width = 6, height = 9) %>%
    body_add_par("")  # Add a paragraph break after each image
}

# Save the Word document
file_path_word <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Timeseries_plots/Maize/Combined_Report.docx"
print(doc, target = file_path_word)

###survival analysis
#################################survival analysis
##################################################################################################### 2. Survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Prod_loess_maize_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)
# Merge the Continuous_Recovery column into the existing result_df
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))


write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_maize_global_modified.csv")

#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window

#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_maize_global_modified.csv")
colnames(result)

# Calculate the number of shocks per region
shocks_per_region <- result %>%
  filter(Shock_Point_prod == TRUE) %>%
  group_by(Subregion_prod) %>%
  summarise(Num_Shocks = n())
# View the result
print(shocks_per_region)

#create a barchart
library(ggplot2)
# Create a bar chart with region names on the y-axis
bar <- ggplot(shocks_per_region, aes(x = Subregion_prod, y = Num_Shocks)) +
  geom_bar(stat = "identity", fill = "goldenrod4") +
  coord_flip() +  # Flip the coordinates to have region names on the y-axis
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11)  # Adjust text alignment if necessary
  ) +
  labs(y = "Shocks per region",x="")

bar

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/maize_regional_shock_bars.png", bar, width = 10, height = 8, dpi = 300)

# Calculate the number of shocks per country
shocks_per_country <- result %>%
  filter(Shock_Point_prod == TRUE) %>%
  group_by(ISO_prod) %>%
  summarise(Num_Shocks = n())

# View the result
print(shocks_per_country)
write.csv(shocks_per_country,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/shocksbycountry_maize.csv")

#Result 1: create shocks map
# Install and load necessary libraries
#install.packages(c("ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata"))
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
merged_data <- world %>% 
  left_join(shocks_per_country, by = c("iso_a3" = "ISO_prod"))

library(ggplot2)
library(sf)

# Assuming merged_data is an sf object with geometry column
map_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Num_Shocks), color = "white") +
  scale_fill_gradient(low = "lightgoldenrod", high = "firebrick4") +
  theme_minimal() +
  labs(fill = "Number of Shocks: Maize") +
  coord_sf(crs = "+proj=robin", datum = NA) + # Set to Robinson projection
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom", # Move legend to bottom
    legend.direction = "horizontal" # Set legend to horizontal
  ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) # Adjust title of legend

map_plot

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_shock_maizemap.png",map_plot, width = 10, height = 8, dpi = 300)

library(patchwork)

# Assuming 'bar' is your bar chart and 'map_plot' is your map
# You can specify the relative width of each plot.
# Here, '1' is the width for the bar chart and '3' is the width for the map.
# Increasing the second value will give more space to the map relative to the bar chart.
combined_plot <- bar + map_plot + plot_layout(widths = c(1, 4))

# Now, you can display or save the combined plot
print(combined_plot)

# Save the combined plot to a file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_shock_maizemapandbar.png", combined_plot, width = 12, height = 8)

# Result 2: Calculate the average recovery time per region
colnames(result)
avg_recovery_per_region <- result %>%
  group_by(Subregion_prod) %>%
  summarise(Avg_Recovery = mean(Continuous_Recovery_Time_prod, na.rm = TRUE))

# View the result
print(avg_recovery_per_region)

#create a barchart
library(ggplot2)
# Create a bar chart with region names on the y-axis
bar <- ggplot(avg_recovery_per_region, aes(x = Subregion_prod, y = Avg_Recovery)) +
  geom_bar(stat = "identity", fill = "royalblue4") +
  coord_flip() +  # Flip the coordinates to have region names on the y-axis
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11)  # Adjust text alignment if necessary
  ) +
  labs(y = "Average recovery per region",x="")

bar
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/maize_regional_averagerecovery_bars.png", bar, width = 10, height = 8, dpi = 300)

#map
#av rec time map
# Calculate the average recovery time per country
avg_recovery_per_country <- result %>%
  group_by(ISO_prod) %>%
  summarise(Avg_Recovery = mean(Continuous_Recovery_Time_prod, na.rm = TRUE))

library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
merged_data <- world %>% 
  left_join(avg_recovery_per_country, by = c("iso_a3" = "ISO_prod"))

# Assuming merged_data is an sf object with geometry column
library(ggplot2)
library(sf)
summary(avg_recovery_per_country$Avg_Recovery)

# Define the colors you want to use in the gradient
colors <- c("khaki", "olivedrab", "navy")

# Assuming merged_data is an sf object with geometry column
map_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Avg_Recovery), color = "white") +
  scale_fill_gradientn(colours = colors, 
                       breaks = c(1,2,3,4,5,6,7,8,9,10,15,20), 
                       values = scales::rescale(c(1,5,20))) +
  theme_minimal() +
  labs(fill = "Average recovery: Maize") +
  coord_sf(crs = "+proj=robin", datum = NA) + # Set to Robinson projection
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom", # Move legend to bottom
    legend.direction = "horizontal", # Set legend to horizontal
    legend.key.width = grid::unit(1, "cm") # Adjust legend key width if needed
  ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) # Adjust title of legend

map_plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_avrecovery_maizemap.png",map_plot, width = 10, height = 8, dpi = 300)

library(patchwork)

# Assuming 'bar' is your bar chart and 'map_plot' is your map
# You can specify the relative width of each plot.
# Here, '1' is the width for the bar chart and '3' is the width for the map.
# Increasing the second value will give more space to the map relative to the bar chart.
combined_plot <- bar + map_plot + plot_layout(widths = c(1, 4))

# Now, you can display or save the combined plot
print(combined_plot)

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_avrec_maizecombi.png",combined_plot, width = 10, height = 8, dpi = 300)

#Result 3: shock size
colnames(result)
avg_shock_per_region <- result %>%
  group_by(Subregion_prod) %>%
  summarise(Avg_shock = mean(Normalized_Shock_Size_prod, na.rm = TRUE))

# View the result
print(avg_shock_per_region)

#create a barchart
library(ggplot2)
# Create a bar chart with region names on the y-axis
bar <- ggplot(avg_shock_per_region, aes(x = Subregion_prod, y = Avg_shock)) +
  geom_bar(stat = "identity", fill = "darkorange3") +
  coord_flip() +  # Flip the coordinates to have region names on the y-axis
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11)  # Adjust text alignment if necessary
  ) +
  labs(y = "Average shock size per region",x="")

bar
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/maize_regional_averageshocksize_bars.png", bar, width = 10, height = 8, dpi = 300)

#map
#av rec time map
# Calculate the average recovery time per country
avg_shock_per_country <- result %>%
  group_by(ISO_prod) %>%
  summarise(Avg_Shock = mean(Normalized_Shock_Size_prod, na.rm = TRUE))

library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
merged_data <- world %>% 
  left_join(avg_shock_per_country, by = c("iso_a3" = "ISO_prod"))

# Assuming merged_data is an sf object with geometry column
library(ggplot2)
library(sf)
summary(avg_shock_per_country$Avg_Shock)
hist(avg_shock_per_country$Avg_Shock)
# Define the colors you want to use in the gradient
colors <- c("cornsilk", "orange", "coral3")

# Assuming merged_data is an sf object with geometry column
map_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Avg_Shock), color = "white") +
  scale_fill_gradientn(colours = colors, 
                       breaks = c(.1,.2,.3,.4,.5,.6,.7,.8,.9), 
                       values = scales::rescale(c(0,.3,.5))) +
  theme_minimal() +
  labs(fill = "Average shock size: Maize") +
  coord_sf(crs = "+proj=robin", datum = NA) + # Set to Robinson projection
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom", # Move legend to bottom
    legend.direction = "horizontal", # Set legend to horizontal
    legend.key.width = grid::unit(1, "cm") # Adjust legend key width if needed
  ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) # Adjust title of legend

map_plot

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_avshock_maizemap.png",map_plot, width = 10, height = 8, dpi = 300)

library(patchwork)

# Assuming 'bar' is your bar chart and 'map_plot' is your map
# You can specify the relative width of each plot.
# Here, '1' is the width for the bar chart and '3' is the width for the map.
# Increasing the second value will give more space to the map relative to the bar chart.
combined_plot <- bar + map_plot + plot_layout(widths = c(1, 4))

# Now, you can display or save the combined plot
print(combined_plot)
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_avshock_maizecombi.png",combined_plot, width = 10, height = 8, dpi = 300)

#########################################################Survival analysis
#survival analysis
# Install and load the required packages
install.packages("survival")
install.packages("survminer")
rm(findata)
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_maize_global_modified.csv")
colnames(findatamaize)

# Create a survival object. Recovery times are right-censored (some observations may not experience the event).
# For our dataset, we'll assume that if `Recovery_Time` is NA, the event hasn't happened (censored).
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)

# Plot the Kaplan-Meier survival curve
library(survminer)
library(ggplot2)

# Adjusting the font size within the ggsurvplot function
surv_plot <- ggsurvplot(km_fit, data = findatamaize,
                        xlab = "Time (in years)",
                        ylab = "Recovery Likelihood",
                        title = "Kaplan-Meier Curve: Maize",
                        legend.title = "Recovery likelihood",
                        legend.labs = c("Recovery Time"),
                        risk.table = FALSE,
                        conf.int = TRUE,
                        conf.int.fill = "grey",
                        conf.int.alpha = 0.2,
                        ylim = c(0, 1),
                        xlim = c(0, 10),
                        font.main = 26,
                        font.x = 26,
                        font.y = 26,
                        font.tickslab = 26,
                        legend = "top")

# Print the plot
print(surv_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_survfit_maize.png", surv_plot$plot, width = 10, height = 8, dpi = 300)

#reverse KM graph
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_maize_global_modified.csv")
colnames(findatamaize)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)
write.csv(km_summary_tidy, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/sumstatmaize.csv",row.names = FALSE)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob
summary(recovery_prob)
# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)
# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood
# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "orange",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Maize ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_survfit_rev_maize.png", reverse_km_plot, width = 10, height = 8, dpi = 300)


#extract the additional information
# Assuming km_fit is your Kaplan-Meier fit object from survfit()
km_summary <- summary(km_fit)

# Creating a data frame for export
export_df <- data.frame(
  Time = km_summary$time,
  RecoveryProbability = 1 - km_summary$surv,
  LowerCI = 1 - km_summary$upper, # Note: upper CI for survival becomes lower CI for recovery
  UpperCI = 1 - km_summary$lower  # Note: lower CI for survival becomes upper CI for recovery
)

# Calculate Median Recovery Time - Interpreting it as the time where RecoveryProbability reaches or exceeds 0.5
median_recovery_time <- with(export_df, min(Time[RecoveryProbability >= 0.5], na.rm = TRUE))
median_recovery_probability <- with(export_df, RecoveryProbability[Time == median_recovery_time])

# Calculate Cumulative Recovery Rate as the last value of RecoveryProbability
cumulative_recovery_rate <- tail(export_df$RecoveryProbability, 1)

# Adding a row for median and cumulative recovery information might not fit well in a single CSV structure
# as the data frame is structured in a long format. Consider adding this information as metadata or in a separate file.
# Export the data frame to CSV
write.csv(export_df, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_survfit_rev_maize_rectablemeta.csv", row.names = FALSE)

# You might want to separately note or export the median and cumulative recovery rates
metadata_df <- data.frame(
  Metric = c("Median Recovery Time", "Median Recovery Probability", "Cumulative Recovery Rate"),
  Value = c(median_recovery_time, median_recovery_probability, cumulative_recovery_rate)
)

# Export metadata to a separate CSV
write.csv(metadata_df, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_survfit_rev_maize_rectablestat.csv", row.names = F)

#histogram for reference
library(ggplot2)
# Plotting a histogram of recovery times
hist<- ggplot(findata, aes(x=Continuous_Recovery_Time_prod)) + 
  geom_histogram(binwidth=1, fill="orange", color="black", alpha=0.7) + 
  labs(title="Distribution of Recovery Times",
       x="Recovery Time (in years)", 
       y="Frequency") +
  theme_minimal()
# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_shockhist_maize.png", hist, width = 10, height = 8, dpi = 300)

#reverse KM curve for each region
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Read the dataset
findata <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_maize_global_modified.csv")

# Get unique list of regions
unique_regions <- unique(findata$Subregion_prod)

# Loop over each region
for (region in unique_regions) {
  # Filter data for the current region
  region_specific_df <- filter(findata, Subregion_prod == region)
  
  # Calculate the number of events (the count of non-NA recovery times)
  num_events <- sum(!is.na(region_specific_df$Continuous_Recovery_Time_prod))
  
  # Check if the filtered data has non-missing observations for survival analysis
  if (nrow(region_specific_df) > 0 && num_events > 0) {
    # Create a survival object for the specific region
    Surv_obj_region <- with(region_specific_df, Surv(Continuous_Recovery_Time_prod))
    
    # Fit the Kaplan-Meier survival function for the specific region
    km_fit_region <- survfit(Surv_obj_region ~ 1)
    
    # Extract and prepare plot data
    plot_data <- data.frame(Time = km_fit_region$time, RecoveryLikelihood = 1 - km_fit_region$surv)
    
    # Ensure there's a row for time 0 with a RecoveryLikelihood of 0
    if(!0 %in% plot_data$Time) {
      plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
    }
    plot_data <- plot_data[order(plot_data$Time),]
    
    # Plotting
    reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
      geom_step(color = "orange", size = 1.2) +
      labs(x = "Time (in years)", y = "Recovery Likelihood", title = paste("Reverse Recovery Curve -", region)) +
      theme_minimal() +
      theme(text = element_text(size = 22), axis.title.x = element_text(size = 30), axis.title.y = element_text(size = 30)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
      annotate("text", x = Inf, y = 1, label = paste("n =", num_events), hjust = 1, vjust = 1, size = 5, color = "black")
    
    # Print the reverse KM plot
    print(reverse_km_plot)
    
    # Save the plot
    ggsave(paste0("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Maize/Reverse_Survival_Curve_maize_", gsub(" ", "_", region), ".png"),
           reverse_km_plot, width = 10, height = 8, dpi = 300)
  } else {
    message("Insufficient data for survival analysis in region: ", region)
  }
}

####now reverse KM curves for BOTH milk and maize
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Load datasets
maize_data <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_maize_global_modified.csv") %>%
  select(Subregion_prod, Continuous_Recovery_Time_prod) %>%
  mutate(Type = 'Maize')

milk_data <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_milk_global_modified.csv") %>%
  select(Subregion_prod, Continuous_Recovery_Time_prod) %>%
  mutate(Type = 'Milk')

# Combine datasets
combined_data <- rbind(maize_data, milk_data)

# Get unique list of regions
unique_regions <- unique(combined_data$Subregion_prod)

# Loop over each region
for(region in unique_regions) {
  # Filter data for the current region
  region_data <- filter(combined_data, Subregion_prod == region)
  
  # Prepare plot data for each type within the region
  plot_data <- lapply(split(region_data, region_data$Type), function(data) {
    if(sum(!is.na(data$Continuous_Recovery_Time_prod)) > 0) {
      surv_obj <- survfit(Surv(Continuous_Recovery_Time_prod) ~ 1, data = data)
      data.frame(Time = surv_obj$time, RecoveryLikelihood = 1 - surv_obj$surv, Type = unique(data$Type))
    } else {
      return(NULL)
    }
  }) %>% bind_rows()
  
  # Ensure a row for time 0 for each type
  types_present <- unique(region_data$Type)
  for(type in types_present) {
    if(!any(plot_data$Type == type & plot_data$Time == 0)) {
      plot_data <- rbind(plot_data, data.frame(Time = 0, RecoveryLikelihood = 0, Type = type))
    }
  }
  
  # Order plot_data by Time
  plot_data <- plot_data[order(plot_data$Time),]
  
  # Proceed with plotting only if plot_data is not empty
  if(nrow(plot_data) > 0) {
    # Calculate n for each Type
    n_values <- sapply(split(region_data, region_data$Type), function(data) sum(!is.na(data$Continuous_Recovery_Time_prod)))
    
    # Plotting
    p <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood, color = Type)) +
      geom_step(size = 2,alpha=.6) + # Plain step line
      scale_color_manual(values = c('Maize' = 'orange', 'Milk' = 'blue')) +
      labs(x = "Time (in years)", y = "Recovery likelihood", title = paste(region)) +
      theme_minimal() +
      theme(legend.title = element_blank(), axis.text = element_text(size = 32), axis.title = element_text(size = 32),plot.title = element_text(size = 30, hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +
      geom_text(data = data.frame(Type = names(n_values), x = Inf, y = c(0.05, 0.10), 
                                  label = paste("n =", n_values)), 
                aes(x = x, y = y, label = label, color = Type), hjust = 1.1, vjust = 1, size = 10)
    
    print(p)
    
    # Save the plot
    ggsave(paste0("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Panel/Reverse_Recovery_Curve_", gsub(" ", "_", region), ".png"), 
           plot = p, width = 10, height = 8, dpi = 300)
  } else {
    message("No data available for survival analysis in region: ", region)
  }
}

#make the panel graph for the same
#panel for only the select regions and exclude the ones with low CI-done manually in the folder
library(gridExtra)
library(ggplot2)
library(png)

# Set the path to the directory containing the graphs
graph_folder <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Panel/Select"

# List all PNG files in the directory
graph_files <- list.files(graph_folder, pattern = "\\.png$", full.names = TRUE)

# Load all graphs
graphs <- lapply(graph_files, function(file) {
  grid::rasterGrob(png::readPNG(file))
})

# Arrange the graphs in a panel
panel_plot <- gridExtra::grid.arrange(grobs = graphs, ncol = 3)

# Display the panel plot
grid::grid.draw(panel_plot)
# Save the combined plot as a PNG file
png(filename = "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Panel/Combined_Graphs_both_select.png", width = 16, height = 18, units = "in", res = 300)
grid::grid.draw(panel_plot)
dev.off()

#extract the KM summaries for all the regions
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Load datasets
maize_data <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_maize_global_modified.csv") %>%
  select(Subregion_prod, Continuous_Recovery_Time_prod) %>%
  mutate(Type = 'Maize')

library(survival)
library(dplyr)

# Get unique list of subregions
unique_subregions <- unique(maize_data$Subregion_prod)

# Initialize an empty dataframe to store compiled results
all_subregions_df <- data.frame()

# Loop through each subregion
for(subregion in unique_subregions) {
  # Filter data for the current subregion
  subregion_data <- filter(maize_data, Subregion_prod == subregion)
  
  # Perform Kaplan-Meier survival analysis
  Surv_obj <- with(subregion_data, Surv(Continuous_Recovery_Time_prod))
  km_fit <- survfit(Surv_obj ~ 1)
  
  # Extract summary of km_fit for recovery probability and CIs
  km_summary <- summary(km_fit, times = km_fit$time)
  subregion_df <- data.frame(
    Subregion_prod = subregion,
    Time = km_summary$time,
    RecoveryProbability = 1 - km_summary$surv,
    LowerCI = 1 - km_summary$upper,  # Note: Upper CI for survival becomes Lower CI for recovery
    UpperCI = 1 - km_summary$lower   # Note: Lower CI for survival becomes Upper CI for recovery
  )
  
  # Compile results across all subregions
  all_subregions_df <- rbind(all_subregions_df, subregion_df)
}

# Order the compiled dataframe by Subregion and Time
all_subregions_df <- all_subregions_df[order(all_subregions_df$Subregion_prod, all_subregions_df$Time), ]

# Specify the output CSV file path
output_csv_path <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/maizeregionalsumstats.csv"

# Export compiled results to CSV
write.csv(all_subregions_df, output_csv_path, row.names = FALSE)


#extract the KM summaries for all the regions
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

milk_data <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_milk_global_modified.csv") %>%
  select(Subregion_prod, Continuous_Recovery_Time_prod) %>%
  mutate(Type = 'Milk')

library(survival)
library(dplyr)

# Assuming combined_data is your data frame
unique_subregions <- unique(milk_data$Subregion_prod)

all_subregions_df <- data.frame()

for(subregion in unique_subregions) {
  subregion_data <- filter(milk_data, Subregion_prod == subregion)
  
  # Check if there are any non-missing observations in this subregion
  if(sum(!is.na(subregion_data$Continuous_Recovery_Time_prod)) > 0) {
    Surv_obj <- with(subregion_data, Surv(Continuous_Recovery_Time_prod))
    km_fit <- survfit(Surv_obj ~ 1)
    
    km_summary <- summary(km_fit, times = km_fit$time)
    subregion_df <- data.frame(
      Subregion_prod = subregion,
      Time = km_summary$time,
      RecoveryProbability = 1 - km_summary$surv,
      LowerCI = 1 - km_summary$upper,  # Adjusting CI interpretation
      UpperCI = 1 - km_summary$lower
    )
    
    all_subregions_df <- rbind(all_subregions_df, subregion_df)
  } else {
    message(paste("No non-missing observations for subregion:", subregion))
  }
}

# Specify the output path and save the compiled dataframe
output_csv_path <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/milkregionalsumstats.csv"
write.csv(all_subregions_df, output_csv_path, row.names = FALSE)


# reverse KM curves with CI bands- revise later
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Load datasets
maize_data <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_maize_global_modified.csv") %>%
  select(Subregion_prod, Continuous_Recovery_Time_prod) %>%
  mutate(Type = 'Maize')

milk_data <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_milk_global_modified.csv") %>%
  select(Subregion_prod, Continuous_Recovery_Time_prod) %>%
  mutate(Type = 'Milk')

# Combine datasets
combined_data <- rbind(maize_data, milk_data)

# Get unique list of regions
unique_regions <- unique(combined_data$Subregion_prod)

# Loop over each region
for(region in unique_regions) {
  region_data <- filter(combined_data, Subregion_prod == region)
  
  # Initialize an empty data frame for adjusted plot data
  adjusted_plot_data <- data.frame()
  
  # Prepare plot data for each type within the region, including CIs
  types_present <- unique(region_data$Type)
  for(type in types_present) {
    type_data <- filter(region_data, Type == type)
    if(sum(!is.na(type_data$Continuous_Recovery_Time_prod)) > 0) {
      surv_obj <- survfit(Surv(Continuous_Recovery_Time_prod) ~ 1, data = type_data)
      # Extracting time, survival estimates, and confidence intervals
      ci_data <- broom::tidy(surv_obj, conf.int = TRUE, exponentiate = FALSE)
      temp_data <- data.frame(Time = ci_data$time,
                              RecoveryLikelihood = 1 - ci_data$estimate,
                              LowerCI = 1 - ci_data$conf.high,
                              UpperCI = 1 - ci_data$conf.low,
                              Type = type)
      
      # Duplicate each time point for stepwise CI plotting, except for the first
      temp_adjusted <- temp_data[-1, ]
      temp_adjusted$Time <- head(temp_data$Time, -1)
      adjusted_plot_data <- rbind(adjusted_plot_data, temp_data, temp_adjusted)
    }
  }
  
  adjusted_plot_data <- adjusted_plot_data[order(adjusted_plot_data$Time), ]
  
  if(nrow(adjusted_plot_data) > 0) {
    # Calculate n for each Type
    n_values <- sapply(types_present, function(t) sum(!is.na(filter(region_data, Type == t)$Continuous_Recovery_Time_prod)))
    
    # Plotting with stepwise lines and CIs
    p <- ggplot(adjusted_plot_data, aes(x = Time, y = RecoveryLikelihood, color = Type)) +
      geom_step(size = 2, alpha = 0.6) +
      geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI, fill = Type), alpha = 0.2) +
      scale_color_manual(values = c('Maize' = 'orange', 'Milk' = 'blue')) +
      scale_fill_manual(values = c('Maize' = 'orange', 'Milk' = 'blue')) +
      labs(x = "Time (in years)", y = "Recovery likelihood", title = paste("Reverse Recovery Curve -", region)) +
      theme_minimal() +
      theme(legend.title = element_blank(), axis.text = element_text(size = 12), axis.title = element_text(size = 14), plot.title = element_text(size = 16, hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      scale_x_continuous(limits = c(0, max(adjusted_plot_data$Time)), breaks = seq(0, max(adjusted_plot_data$Time), by = 2)) +
      geom_text(data = data.frame(Type = names(n_values), x = Inf, y = c(0.05, 0.10), label = paste("n =", n_values)), 
                aes(x = x, y = y, label = label, color = Type), hjust = 1.1, vjust = 1, size = 5)
    
    print(p)
    
    # Save the plot
    ggsave(paste0("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Panel/Reverse_Recovery_Curve_", gsub(" ", "_", region), ".png"), 
           plot = p, width = 10, height = 8, dpi = 300)
  } else {
    message("No data available for survival analysis in region: ", region)
  }
}


####subset by region-old code for KM curve
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Read the dataset
findata <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_maize_global_modified.csv")

# Get unique list of regions
unique_regions <- unique(findata$Subregion_prod)

# Loop over each region
for (region in unique_regions) {
  # Filter data for the current region
  region_specific_df <- dplyr::filter(findata, Subregion_prod == region)
  
  # Calculate the number of events (the count of non-NA recovery times)
  num_events <- sum(!is.na(region_specific_df$Continuous_Recovery_Time_prod))
  
  # Check if the filtered data has non-missing observations for survival analysis
  if (nrow(region_specific_df) > 0 && num_events > 0) {
    # Create a survival object for the specific region
    Surv_obj_region <- with(region_specific_df, Surv(Continuous_Recovery_Time_prod))
    
    # Fit the Kaplan-Meier survival function for the specific region
    km_fit_region <- survfit(Surv_obj_region ~ 1)
    
    # Plot the Kaplan-Meier survival curve for the specific region
    surv_plot <- ggsurvplot(km_fit_region, data = region_specific_df,
                            xlab = "Time (in years)",
                            ylab = "Recovery Likelihood",
                            title = paste("Recovery Curve -", region),
                            legend.title = "Recovery Curve",
                            legend.labs = c("Recovery Time"),
                            conf.int = TRUE,    # Include the 95% confidence intervals
                            conf.int.style = "ribbon",  # Style for the confidence interval as shaded area
                            conf.int.fill = "grey",     # Color for the shaded area of CI
                            palette = "npg",            # Color palette
                            break.time.by = 1,          # Time interval to break X-axis
                            ylim = c(0, 1),
                            xlim = c(0, 10),             # Limit x-axis to 10
                            font.main = 26,
                            font.x = 26,
                            font.y = 26,
                            font.tickslab = 26,
                            legend = "right")
    
    # Add annotation for the number of events
    surv_plot$plot <- surv_plot$plot + 
      annotate("text", x = 10, y = 1, label = paste("n =", num_events),
               hjust = 1, vjust = 2, size = 10, color = "black")
    
    # Save the plot
    ggsave(paste0("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Maize/Survival_Curve_maize_", gsub(" ", "_", region), ".png"),
           plot = surv_plot$plot, width = 10, height = 8)
  } else {
    message("Insufficient data for survival analysis in region: ", region)
  }
}



#panel for only the select regions and exclude the ones with low CI-done manually in the folder
library(gridExtra)
library(ggplot2)
library(png)

# Set the path to the directory containing the graphs
graph_folder <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Maize/Select"

# List all PNG files in the directory
graph_files <- list.files(graph_folder, pattern = "\\.png$", full.names = TRUE)

# Load all graphs
graphs <- lapply(graph_files, function(file) {
  grid::rasterGrob(png::readPNG(file))
})

# Arrange the graphs in a panel
panel_plot <- gridExtra::grid.arrange(grobs = graphs, ncol = 2, widths = c(3, 3))

# Display the panel plot
grid::grid.draw(panel_plot)
# Save the combined plot as a PNG file
png(filename = "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Maize/Combined_Graphs_maize_select.png", width = 16, height = 18, units = "in", res = 300)
grid::grid.draw(panel_plot)
dev.off()


#########################################SENSITIVITY ANALYSIS by removing long recovery years
library(survival)
library(survminer)
library(ggplot2)
findata<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_maize_global_modified1.csv")
colnames(findata)

##########################filtering long recovery years
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
# Create a subset of findata where modifiedrecovery is less than 10
subset_findata <- findata %>%
  filter(modifiedrecovery < 11)
# View the first few rows of the subset
head(subset_findata)

# Calculate the number of shocks per region
shocks_per_region <- subset_findata %>%
  filter(Shock_Point_prod == TRUE) %>%
  group_by(Subregion_prod) %>%
  summarise(Num_Shocks = n())

# View the result
print(shocks_per_region)

#create a barchart
library(ggplot2)
# Create a bar chart with tilted x-axis labels
bar<- ggplot(shocks_per_region, aes(x = Subregion_prod, y = Num_Shocks)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Region", y = "Number of Shocks", title = "Shocks Per Region")

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/maize_regional_shock_bars_subset.png", bar, width = 10, height = 8, dpi = 300)

# Calculate the number of shocks per country
shocks_per_country <- subset_findata %>%
  filter(Shock_Point_prod == TRUE) %>%
  group_by(ISO_prod) %>%
  summarise(Num_Shocks = n())

# View the result
print(shocks_per_country)

write.csv(shocks_per_country,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/shocksbycountry_maize_subset.csv")

#create shocks map
# Install and load necessary libraries
#install.packages(c("ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata"))
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Assuming you have a tibble like this
# your_data <- tibble(ISO_prod = c("USA", "FRA", "BRA"), Num_Shocks = c(10, 5, 7))

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
merged_data <- world %>% 
  left_join(shocks_per_country, by = c("iso_a3" = "ISO_prod"))

# Create the map
map_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Num_Shocks), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +  # or any other color scale of your choice
  theme_minimal() +
  labs(fill = "Number of Shocks", title = "Map Showing Number of Shocks by Country")

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_shock_maizemap_subset.png",map_plot, width = 10, height = 8, dpi = 300)

# Calculate the average recovery time per region
avg_recovery_per_region <- subset_findata %>%
  group_by(Subregion_prod) %>%
  summarise(Avg_Recovery = mean(modifiedrecovery, na.rm = TRUE))

# View the result
print(avg_recovery_per_region)

##create a bar graph
bar<- ggplot(avg_recovery_per_region, aes(x = Subregion_prod, y = Avg_Recovery)) +
  geom_bar(stat = "identity", fill = "green") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Region", y = "Average recovery time", title = "Recovey time per region")

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/regional_avrec_bars_maize_subset.png", bar, width = 10, height = 8, dpi = 300)

#av rec time map
# Calculate the average recovery time per country
avg_recovery_per_region <- subset_findata %>%
  group_by(ISO_prod) %>%
  summarise(Avg_Recovery = mean(modifiedrecovery, na.rm = TRUE))

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
merged_data <- world %>% 
  left_join(avg_recovery_per_region, by = c("iso_a3" = "ISO_prod"))

# Create the map
library(ggplot2)
library(MetBrewer)

# Choose a MetBrewer palette
# Let's use the "Veronese" palette, but you can choose any other palette from MetBrewer
palette <- MetBrewer::met.brewer("Tam")

# Create the map with the MetBrewer palette
mapplot1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Avg_Recovery), color = "white") +
  scale_fill_gradientn(
    colours = palette,
    breaks = seq(min(merged_data$Avg_Recovery, na.rm = TRUE), 
                 max(merged_data$Avg_Recovery, na.rm = TRUE), by = 5),
    labels = seq(min(merged_data$Avg_Recovery, na.rm = TRUE), 
                 max(merged_data$Avg_Recovery, na.rm = TRUE), by = 5)
  ) +
  theme_minimal() +
  labs(fill = "Average Recovery Time")

mapplot1

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_avrec_maizemap_subset.png",mapplot1, width = 10, height = 8, dpi = 300)

#survival results
# Create a survival object. Recovery times are right-censored (some observations may not experience the event).
# For our dataset, we'll assume that if `Recovery_Time` is NA, the event hasn't happened (censored).
Surv_obj <- with(subset_findata, Surv(modifiedrecovery))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)

library(survival)
library(survminer)

# Assuming km_fit is already created using survfit() and findata is your dataset

# Plot the Kaplan-Meier survival curve
surv_plot <- ggsurvplot(km_fit, data = subset_findata,
                        xlab = "Time (in years)",
                        ylab = "Survival Probability",
                        title = "Kaplan-Meier Survival Curve: Recovery Time",
                        legend.title = "Survival Curve",
                        legend.labs = c("Recovery Time"),
                        risk.table = FALSE,  # Set to FALSE to not include risk table in the plot
                        conf.int = TRUE,
                        conf.int.fill = "grey",
                        conf.int.alpha = 0.2,
                        ylim = c(0, 1),
                        xlim = c(0, 10))

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_survfit_maize_subset.png", surv_plot$plot, width = 10, height = 8, dpi = 300)

#histogram for reference
library(ggplot2)
# Plotting a histogram of recovery times
hist<- ggplot(subset_findata, aes(x=modifiedrecovery)) + 
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) + 
  labs(title="Distribution of Recovery Times",
       x="Recovery Time (in years)", 
       y="Frequency") +
  theme_minimal()
# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_shockhist_maize_subset.png", hist, width = 10, height = 8, dpi = 300)

####subset by region-loop
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
# Assuming subset_findata is your dataset and it contains 'Subregion_prod' for regions
# Get unique list of regions
unique_regions <- unique(subset_findata$Subregion_prod)

# Loop over each region
for (region in unique_regions) {
  # Filter data for the current region
  region_specific_df <- filter(subset_findata, Subregion_prod == region)
  
  # Check if the filtered data has non-missing observations for survival analysis
  if (nrow(region_specific_df) > 0 && sum(!is.na(region_specific_df$modifiedrecovery)) > 0) {
    # Create a survival object for the specific region
    Surv_obj_region <- with(region_specific_df, Surv(modifiedrecovery))
    
    # Fit the Kaplan-Meier survival function for the specific region
    km_fit_region <- survfit(Surv_obj_region ~ 1)
    
    # Plot the Kaplan-Meier survival curve for the specific region with 95% confidence intervals
    surv_plot <- ggsurvplot(km_fit_region, data = region_specific_df,
                            xlab = "Time (in years)",
                            ylab = "Survival Probability",
                            title = region,
                            legend.title = "Survival Curve",
                            legend.labs = c("Recovery Time"),
                            risk.table = FALSE,  # Include the number of subjects at risk
                            conf.int = TRUE,    # Include the 95% confidence intervals
                            conf.int.fill = "grey",  # Color for the shaded area
                            conf.int.alpha = 0.2,    # Transparency of the shaded area
                            ylim = c(0, 1),
                            xlim = c(0, 8))  # Adjust y-axis limits
    
    # Save the plot (extracting the plot component from the ggsurvplot object)
    ggsave(paste0("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Maize/subset/Survival_Curve_maize_subset_", region, ".png"), surv_plot$plot, width = 10, height = 8)
  } else {
    message("Insufficient data for survival analysis in region: ", region)
  }
}

#panel
library(png)
library(grid)
library(gridExtra)

# Directory containing your graph PNG files
graph_dir <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Maize/subset/"
graph_files <- list.files(graph_dir, pattern = "\\.png$", full.names = TRUE)

# Read and store the images in a list as rasterGrob objects
images <- lapply(graph_files, function(file) {
  img <- readPNG(file)
  rasterGrob(img)
})

# Determine the number of rows and columns for the grid layout
# This will create a layout that can accommodate all images
n_images <- length(images)
nrow_layout <- ceiling(sqrt(n_images))
ncol_layout <- ceiling(n_images / nrow_layout)

# Arrange the images in a grid layout
grid_layout <- do.call("grid.arrange", c(images, nrow = nrow_layout, ncol = ncol_layout))

# Save the combined plot as a PNG file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Maize/subset/Combined_Graphs_subset_maize.png", grid_layout, width = 16, height = 18)

#probs at 1 yeat step for the map
# Extract the summary of the km_fit object
summary_km <- summary(km_fit)

# Extract survival probabilities
time_points <- summary_km$time
surv_probs <- summary_km$surv

# Create a data frame with the extracted information
survival_data <- data.frame(Time = time_points, Survival_Probability = surv_probs)

# Filter data for 1-year intervals (assuming the data is in yearly steps)
yearly_survival_data <- survival_data[round(survival_data$Time) == survival_data$Time, ]
# View the data
print(yearly_survival_data)

#survival table
library(survival)
library(dplyr)

# Get unique list of regions
unique_regions <- unique(subset_findata$Subregion_prod)

# Initialize an empty dataframe to store results
combined_survival_data <- data.frame()

# Loop over each region
for (region in unique_regions) {
  # Subset data for the current region
  region_data <- filter(subset_findata, Subregion_prod == region)
  
  # Check if the subset has sufficient data
  if (nrow(region_data) > 0 && sum(!is.na(region_data$modifiedrecovery)) > 0) {
    # Create a survival object for the specific region
    Surv_obj_region <- with(region_data, Surv(modifiedrecovery))  # Replace 'Time' and 'Event' with actual column names
    
    # Fit the Kaplan-Meier survival function for the specific region
    km_fit_region <- survfit(Surv_obj_region ~ 1)
    
    # Extract the summary of the km_fit object
    summary_km_region <- summary(km_fit_region)
    
    # Extract survival probabilities at 1st and 2nd year
    one_two_year_survival <- summary_km_region$surv[summary_km_region$time %in% c(1, 2)]
    one_two_year_time <- summary_km_region$time[summary_km_region$time %in% c(1, 2)]
    
    # Create a data frame with the extracted information
    survival_data_region <- data.frame(Region = region, Time = one_two_year_time, Survival_Probability = one_two_year_survival)
    
    # Combine with the overall dataframe
    combined_survival_data <- rbind(combined_survival_data, survival_data_region)
  } else {
    message("Insufficient data for survival analysis in region: ", region)
  }
}

# View the combined data
print(combined_survival_data)
write.csv(combined_survival_data,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/recoverytablesurv_maize_subset.csv")

#filter secnd year data
library(dplyr)
# Filter the combined_survival_data for Time = 2
second_year_data <- combined_survival_data %>%
  filter(Time == 2)
# View the filtered data
print(second_year_data)

#bar graph if needed
ggplot(second_year_data, aes(x = Region, y = Survival_Probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Region", y = "Survival Probability", title = "2nd Year Survival Probabilities by Region")

#bubble chart
library(dplyr)

# Calculate shocks frequency for each region
shocks_frequency <- subset_findata %>%
  group_by(Subregion_prod) %>%
  summarize(Shocks_Frequency = sum(Shock_Point_prod, na.rm = TRUE)) %>%
  ungroup()  # Ungrouping to prevent issues in subsequent operations

# Calculate average regional production for each region
average_production <- subset_findata %>%
  group_by(Subregion_prod) %>%
  summarize(Avg_Regional_Production = median(Value_prod, na.rm = TRUE)) %>%
  ungroup()

# Rename the 'Subregion_prod' column in both dataframes to 'Region' for consistent merging
shocks_frequency <- rename(shocks_frequency, Region = Subregion_prod)
average_production <- rename(average_production, Region = Subregion_prod)

# Merge the shocks frequency and average production with second_year_data
second_year_combined_data <- second_year_data %>%
  left_join(shocks_frequency, by = "Region") %>%
  left_join(average_production, by = "Region")

# View the combined data
print(second_year_combined_data)

#now the bubble chart
library(ggplot2)
# Create the Bubble Chart with Adjusted Bubble Sizes
bubble_chart <- ggplot(second_year_combined_data, aes(x = Survival_Probability, y = Shocks_Frequency, size = Avg_Regional_Production, color = Region)) +
  geom_point(alpha = 0.7) +  # Adjust alpha as needed
  scale_size_continuous(name = "Regional Production", range = c(5, 20)) +  # Increase the range of bubble sizes
  theme_minimal() +
  labs(x = "Survival Probability in 2nd Year", 
       y = "Frequency of Shocks", 
       title = "Bubble Chart: Shocks Frequency vs. Survival Probability",
       subtitle = "Bubble size represents median regional production, color represents region") +
  scale_color_viridis_d()  # Use a color palette suitable for categorical data

# Save the plot to a file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/bubble_chart_maize_subset.png", bubble_chart, width = 10, height = 8, dpi = 300)


############################MILK ANALYSIS ################################################################################
#the names were processed in the csv files directly
library(dplyr)
library(zoo)

#rm previous files
rm(rolling_baseline_data)
rm(loess_model)
rm(Fitted_Production)
rm(milk_data)
rm(single_country_data)

# Assuming your data frame is named 'milk_data'
milk_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAO_Milk.csv")
colnames(milk_data)
#sum stats
summary(milk_data$Value)
hist(milk_data$Value)
#sum stats
summary(milk_data$Value)
#by region
summary_stats_by_region <- milk_data %>%
  group_by(Subregion) %>%
  summarise(
    Count = n(),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Print the summary statistics
print(summary_stats_by_region)

# Specify the path and file name for your output CSV
output_file_path <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/sumstatmilkbyregion.csv"
# Export the tibble to a CSV file
write.csv(summary_stats_by_region, output_file_path, row.names = FALSE)


# Assuming your data frame is named 'milk_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(milk_data) {
  return(milk_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 4, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.6)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(milk_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

# Now, all column names have the "_weather" suffix
print(colnames(rolling_baseline_data))
#save the file
write.csv(rolling_baseline_data, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Prod_loess_milk_global.csv", row.names = T)

#fix loess span and cooks distance values, inspect the fitted reg through plots
# Load required packages
#supplementary graphs
#fix loess span and cooks distance values, inspect the fitted reg through plots-you need to loop through for all the countries
####loop over all the countries
library(ggplot2)
library(gridExtra)

# Get unique list of country ISO codes from the data
unique_countries <- unique(rolling_baseline_data$ISO_prod)

# Loop over each country ISO code
for (country_code in unique_countries) {
  # Filter the data for the current country
  country_data <- subset(rolling_baseline_data, ISO_prod == country_code)
  
  # Plot a - Production and LOESS fit with shocks indicated by green triangles
  plot_a <- ggplot(country_data, aes(x = Year_prod, y = Value_prod)) +
    geom_point(size = 2, alpha = 0.6) +
    geom_line(aes(y = Loess_Fitted_Production_prod), color = "blue", size = 1) +
    geom_point(data = subset(country_data, Shock_Point_prod == TRUE), aes(y = Value_prod), shape = 17, color = "green", size = 3) +
    labs(title = paste("Production Over Time with LOESS Fit -", country_code), subtitle = "Green triangles indicate shocks") +
    theme_minimal()
  
  # Plot b - Residuals against lag-1 residuals with shocks
  plot_b <- ggplot(country_data, aes(x = Lag1_Residuals_prod, y = Residuals_prod)) +
    geom_point(color = "blue", size = 2, alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    geom_point(data = subset(country_data, Shock_Point_prod == TRUE), aes(x = Lag1_Residuals_prod, y = Residuals_prod), color = "green", shape = 17, size = 3) +
    geom_text(data = subset(country_data, Shock_Point_prod == TRUE), aes(label = paste("Cook's D:", round(Cooksd_prod, 2))), vjust = -0.5) +
    labs(title = paste("Residuals vs Lag-1 Residuals for", country_code)) +
    theme_minimal()
  
  # Plot c - Cook's distance over time with threshold line and shocks indicated by green triangles
  plot_c <- ggplot(country_data, aes(x = Year_prod)) +
    geom_line(aes(y = Cooksd_prod), color = "black") +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "red") +
    geom_point(data = subset(country_data, Shock_Point_prod == TRUE), aes(y = Cooksd_prod), shape = 17, color = "green", size = 3) +
    labs(title = paste("Cook's Distance Over Time -", country_code)) +
    theme_minimal()
  
  # Combine the plots into a single image
  multi_plot <- marrangeGrob(list(plot_a, plot_b, plot_c), ncol = 1, nrow = 3, top = paste("Country:", country_code))
  
  # Define the file path for the PNG output
  file_path <- file.path("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Timeseries_plots/Milk", paste0(country_code, "_timeseries_plots.png"))
  
  # Save the combined plot to a PNG file
  ggsave(file_path, multi_plot, width = 8, height = 12, dpi = 300)
}

#now save all the images into a doc
library(officer)
library(magrittr)

# Path to the folder with PNG files
folder_path <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Timeseries_plots/Milk/"

# Create a list of all PNG files
png_files <- list.files(path = folder_path, pattern = "*.png", full.names = TRUE)

# Create a new Word document
doc <- read_docx()

# Loop over PNG files and add them to the document
for (file_path in png_files) {
  doc <- doc %>%
    body_add_img(src = file_path, width = 6, height = 9) %>%
    body_add_par("")  # Add a paragraph break after each image
}

# Save the Word document
file_path_word <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Timeseries_plots/Milk/Combined_Report_milk.docx"
print(doc, target = file_path_word)

###survival analysis
#################################survival analysis
##################################################################################################### 2. Survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Prod_loess_milk_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

colnames(result_df)

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)

# Merge the Continuous_Recovery column into the existing result_df
# Check for duplicates in the dataframe you want to join
duplicate_keys <- continuous_recovery_df %>%
  group_by(ISO_prod, Year_prod) %>%
  summarise(n = n()) %>%
  filter(n > 1)

# If duplicates are found, you'll need to inspect and decide how to handle them
if (nrow(duplicate_keys) > 0) {
  print("Duplicates detected in continuous_recovery_df:")
  print(duplicate_keys)
  
  # Here you would include your logic to handle duplicates,
  # e.g., by averaging, summing, or choosing one row per group.
  # For example, if you want to keep only the first occurrence:
  continuous_recovery_df <- continuous_recovery_df %>%
    distinct(ISO_prod, Year_prod, .keep_all = TRUE)
}

# After resolving duplicates, you can safely perform the join
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))
write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_milk_global_modified.csv")


#START FROM HERE TOMORROW
#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window
#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region
#now save all the images into a doc


#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_milk_global_modified.csv")
colnames(result)

# Calculate the number of shocks per region
shocks_per_region <- result %>%
  filter(Shock_Point_prod == TRUE) %>%
  group_by(Subregion_prod) %>%
  summarise(Num_Shocks = n())
# View the result
print(shocks_per_region)

#create a barchart
library(ggplot2)
# Create a bar chart with region names on the y-axis
bar <- ggplot(shocks_per_region, aes(x = Subregion_prod, y = Num_Shocks)) +
  geom_bar(stat = "identity", fill = "goldenrod4") +
  coord_flip() +  # Flip the coordinates to have region names on the y-axis
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11)  # Adjust text alignment if necessary
  ) +
  labs(y = "Shocks per region",x="")

bar

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/milk_regional_shock_bars.png", bar, width = 10, height = 8, dpi = 300)

# Calculate the number of shocks per country
shocks_per_country <- result %>%
  filter(Shock_Point_prod == TRUE) %>%
  group_by(ISO_prod) %>%
  summarise(Num_Shocks = n())

# View the result
print(shocks_per_country)
write.csv(shocks_per_country,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/shocksbycountry_milk.csv")

#Result 1: create shocks map
# Install and load necessary libraries
#install.packages(c("ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata"))
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
merged_data <- world %>% 
  left_join(shocks_per_country, by = c("iso_a3" = "ISO_prod"))

library(ggplot2)
library(sf)

# Assuming merged_data is an sf object with geometry column
map_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Num_Shocks), color = "white") +
  scale_fill_gradient(low = "lightgoldenrod", high = "firebrick4") +
  theme_minimal() +
  labs(fill = "Number of Shocks: Milk") +
  coord_sf(crs = "+proj=robin", datum = NA) + # Set to Robinson projection
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom", # Move legend to bottom
    legend.direction = "horizontal" # Set legend to horizontal
  ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) # Adjust title of legend

map_plot

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_shock_milkmap.png",map_plot, width = 10, height = 8, dpi = 300)

library(patchwork)

# Assuming 'bar' is your bar chart and 'map_plot' is your map
# You can specify the relative width of each plot.
# Here, '1' is the width for the bar chart and '3' is the width for the map.
# Increasing the second value will give more space to the map relative to the bar chart.
combined_plot <- bar + map_plot + plot_layout(widths = c(1, 4))

# Now, you can display or save the combined plot
print(combined_plot)

# Save the combined plot to a file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_shock_milkmapandbar.png", combined_plot, width = 12, height = 8)

# Result 2: Calculate the average recovery time per region
colnames(result)
avg_recovery_per_region <- result %>%
  group_by(Subregion_prod) %>%
  summarise(Avg_Recovery = mean(Continuous_Recovery_Time_prod, na.rm = TRUE))

# View the result
print(avg_recovery_per_region)

#create a barchart
library(ggplot2)
library(dplyr)

# Assuming avg_recovery_per_region is your data frame
# Filter out rows where Avg_Recovery is NA (missing) or zero
filtered_data <- avg_recovery_per_region %>%
  filter(!is.na(Avg_Recovery) & Avg_Recovery > 0)

# Create a bar chart with the filtered data
bar <- ggplot(filtered_data, aes(x = Subregion_prod, y = Avg_Recovery)) +
  geom_bar(stat = "identity", fill = "royalblue4") +
  coord_flip() +  # Flip the coordinates to have region names on the y-axis
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11)  # Adjust text alignment if necessary
  ) +
  labs(y = "Average recovery per region", x = "")

bar

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/milk_regional_averagerecovery_bars.png", bar, width = 10, height = 8, dpi = 300)

#map
#av rec time map
# Calculate the average recovery time per country
avg_recovery_per_country <- result %>%
  group_by(ISO_prod) %>%
  summarise(Avg_Recovery = mean(Continuous_Recovery_Time_prod, na.rm = TRUE))

library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
merged_data <- world %>% 
  left_join(avg_recovery_per_country, by = c("iso_a3" = "ISO_prod"))

# Assuming merged_data is an sf object with geometry column
library(ggplot2)
library(sf)
summary(avg_recovery_per_country$Avg_Recovery)
hist(avg_recovery_per_country$Avg_Recovery)
# Define the colors you want to use in the gradient
colors <- c("khaki", "olivedrab", "navy")

# Assuming merged_data is an sf object with geometry column
map_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Avg_Recovery), color = "white") +
  scale_fill_gradientn(colours = colors, 
                       breaks = c(1,2,5,8,10,15,20,27), 
                       values = scales::rescale(c(1,5,27))) +
  theme_minimal() +
  labs(fill = "Average recovery: Milk") +
  coord_sf(crs = "+proj=robin", datum = NA) + # Set to Robinson projection
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom", # Move legend to bottom
    legend.direction = "horizontal", # Set legend to horizontal
    legend.key.width = grid::unit(1, "cm") # Adjust legend key width if needed
  ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) # Adjust title of legend

map_plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_avrecovery_milkmap.png",map_plot, width = 10, height = 8, dpi = 300)

library(patchwork)

# Assuming 'bar' is your bar chart and 'map_plot' is your map
# You can specify the relative width of each plot.
# Here, '1' is the width for the bar chart and '3' is the width for the map.
# Increasing the second value will give more space to the map relative to the bar chart.
combined_plot <- bar + map_plot + plot_layout(widths = c(1, 4))

# Now, you can display or save the combined plot
print(combined_plot)

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_avrec_milkcombi.png",combined_plot, width = 10, height = 8, dpi = 300)

#Result 3: shock size
colnames(result)
avg_shock_per_region <- result %>%
  group_by(Subregion_prod) %>%
  summarise(Avg_shock = mean(Composite_Shock_Size_prod, na.rm = TRUE))

# View the result
print(avg_shock_per_region)

#create a barchart
library(ggplot2)
library(dplyr)

# Assuming avg_shock_per_region is your data frame
# Filter out rows where Avg_shock is NA (missing) or zero
filtered_data <- avg_shock_per_region %>%
  filter(!is.na(Avg_shock) & Avg_shock > 0)

# Create a bar chart with the filtered data
bar <- ggplot(filtered_data, aes(x = Subregion_prod, y = Avg_shock)) +
  geom_bar(stat = "identity", fill = "darkorange3") +
  coord_flip() +  # Flip the coordinates to have region names on the y-axis
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11)  # Adjust text alignment if necessary
  ) +
  labs(y = "Average shock size per region", x = "")

bar

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/milk_regional_averageshocksize_bars.png", bar, width = 10, height = 8, dpi = 300)

#map
#av rec time map
# Calculate the average recovery time per country
avg_shock_per_country <- result %>%
  group_by(ISO_prod) %>%
  summarise(Avg_Shock = mean(Composite_Shock_Size_prod, na.rm = TRUE))

library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
merged_data <- world %>% 
  left_join(avg_shock_per_country, by = c("iso_a3" = "ISO_prod"))

# Assuming merged_data is an sf object with geometry column
library(ggplot2)
library(sf)
summary(avg_shock_per_country$Avg_Shock)
hist(avg_shock_per_country$Avg_Shock)
# Define the colors you want to use in the gradient
colors <- c("cornsilk", "orange", "coral3")

# Assuming merged_data is an sf object with geometry column
map_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Avg_Shock), color = "white") +
  scale_fill_gradientn(colours = colors, 
                       breaks = c(.1,.2,.3,.4,.5,.6), 
                       values = scales::rescale(c(0,.3,.5))) +
  theme_minimal() +
  labs(fill = "Average shock size: Milk") +
  coord_sf(crs = "+proj=robin", datum = NA) + # Set to Robinson projection
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom", # Move legend to bottom
    legend.direction = "horizontal", # Set legend to horizontal
    legend.key.width = grid::unit(1, "cm") # Adjust legend key width if needed
  ) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) # Adjust title of legend

map_plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_avshock_milkmap.png",map_plot, width = 10, height = 8, dpi = 300)

library(patchwork)

# Assuming 'bar' is your bar chart and 'map_plot' is your map
# You can specify the relative width of each plot.
# Here, '1' is the width for the bar chart and '3' is the width for the map.
# Increasing the second value will give more space to the map relative to the bar chart.
combined_plot <- bar + map_plot + plot_layout(widths = c(1, 4))

# Now, you can display or save the combined plot
print(combined_plot)
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_avshock_milkcombi.png",combined_plot, width = 10, height = 8, dpi = 300)

#########################################################Survival analysis
#survival analysis
# Install and load the required packages
install.packages("survival")
install.packages("survminer")

library(survival)
library(survminer)
library(ggplot2)
findatamilk <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_milk_global_modified.csv")
colnames(findatamilk)

# Create a survival object. Recovery times are right-censored (some observations may not experience the event).
# For our dataset, we'll assume that if `Recovery_Time` is NA, the event hasn't happened (censored).
Surv_obj <- with(findatamilk, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)

# Plot the Kaplan-Meier survival curve
library(survminer)
library(ggplot2)

# Adjusting the font size within the ggsurvplot function
surv_plot <- ggsurvplot(km_fit, data = findatamilk,
                        xlab = "Time (in years)",
                        ylab = "Recovery Likelihood",
                        title = "Kaplan-Meier Curve: Milk",
                        legend.title = "Recovery likelihood",
                        legend.labs = c("Recovery Time"),
                        risk.table = FALSE,
                        conf.int = TRUE,
                        conf.int.fill = "grey",
                        conf.int.alpha = 0.2,
                        ylim = c(0, 1),
                        xlim = c(0, 10),
                        break.time.by = 1,          # Time interval to break X-axis
                        font.main = 26,
                        font.x = 26,
                        font.y = 26,
                        font.tickslab = 26,
                        legend = "top")

# Print the plot
print(surv_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_survfit_milk.png", surv_plot$plot, width = 10, height = 8, dpi = 300)

#reverse KM graph
library(survival)
library(ggplot2)
library(survminer)
findatamilk <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_milk_global_modified.csv")
colnames(findatamilk)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamilk, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)

# Fit the Kaplan-Meier survival function
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)
write.csv(km_summary_tidy, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/sumstatmilk.csv",row.names = FALSE)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob

# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)

# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood

# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "blue",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Milk ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12,  by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_survfit_rev_milk.png", reverse_km_plot, width = 10, height = 8, dpi = 300)

#extract the additional information
# Assuming km_fit is your Kaplan-Meier fit object from survfit()
km_summary <- summary(km_fit)

# Creating a data frame for export
export_df <- data.frame(
  Time = km_summary$time,
  RecoveryProbability = 1 - km_summary$surv,
  LowerCI = 1 - km_summary$upper, # Note: upper CI for survival becomes lower CI for recovery
  UpperCI = 1 - km_summary$lower  # Note: lower CI for survival becomes upper CI for recovery
)

# Calculate Median Recovery Time - Interpreting it as the time where RecoveryProbability reaches or exceeds 0.5
median_recovery_time <- with(export_df, min(Time[RecoveryProbability >= 0.5], na.rm = TRUE))
median_recovery_probability <- with(export_df, RecoveryProbability[Time == median_recovery_time])

# Calculate Cumulative Recovery Rate as the last value of RecoveryProbability
cumulative_recovery_rate <- tail(export_df$RecoveryProbability, 1)

# Adding a row for median and cumulative recovery information might not fit well in a single CSV structure
# as the data frame is structured in a long format. Consider adding this information as metadata or in a separate file.
# Export the data frame to CSV
write.csv(export_df, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_survfit_rev_milk_rectablemeta.csv", row.names = FALSE)

# You might want to separately note or export the median and cumulative recovery rates
metadata_df <- data.frame(
  Metric = c("Median Recovery Time", "Median Recovery Probability", "Cumulative Recovery Rate"),
  Value = c(median_recovery_time, median_recovery_probability, cumulative_recovery_rate)
)

# Export metadata to a separate CSV
write.csv(metadata_df, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_survfit_rev_milk_rectablestat.csv", row.names = F)


#histogram for reference
library(ggplot2)
# Plotting a histogram of recovery times
hist<- ggplot(findata, aes(x=Continuous_Recovery_Time_prod)) + 
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) + 
  labs(title="Distribution of Recovery Times",
       x="Recovery Time (in years)", 
       y="Frequency") +
  theme_minimal()
# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_shockhist_milk.png", hist, width = 10, height = 8, dpi = 300)

####subset by region
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Read the dataset
findata <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_milk_global_modified.csv")

# Get unique list of regions
unique_regions <- unique(findata$Subregion_prod)

# Loop over each region
for (region in unique_regions) {
  # Filter data for the current region
  region_specific_df <- dplyr::filter(findata, Subregion_prod == region)
  
  # Calculate the number of events (the count of non-NA recovery times)
  num_events <- sum(!is.na(region_specific_df$Continuous_Recovery_Time_prod))
  
  # Check if the filtered data has non-missing observations for survival analysis
  if (nrow(region_specific_df) > 0 && num_events > 0) {
    # Create a survival object for the specific region
    Surv_obj_region <- with(region_specific_df, Surv(Continuous_Recovery_Time_prod))
    
    # Fit the Kaplan-Meier survival function for the specific region
    km_fit_region <- survfit(Surv_obj_region ~ 1)
    
    # Plot the Kaplan-Meier survival curve for the specific region
    surv_plot <- ggsurvplot(km_fit_region, data = region_specific_df,
                            xlab = "Time (in years)",
                            ylab = "Recovery Likelihood",
                            title = paste("Recovery Curve -", region),
                            legend.title = "Recovery Curve",
                            legend.labs = c("Recovery Time"),
                            conf.int = TRUE,    # Include the 95% confidence intervals
                            conf.int.style = "ribbon",  # Style for the confidence interval as shaded area
                            conf.int.fill = "grey",     # Color for the shaded area of CI
                            palette = "npg",            # Color palette
                            break.time.by = 1,          # Time interval to break X-axis
                            ylim = c(0, 1),
                            xlim = c(0, 10),             # Limit x-axis to 6
                            font.main = 26,
                            font.x = 26,
                            font.y = 26,
                            font.tickslab = 26,
                            legend = "right")
    
    # Add annotation for the number of events
    surv_plot$plot <- surv_plot$plot + 
      annotate("text", x = 10, y = 1, label = paste("n =", num_events),
               hjust = 1, vjust = 2, size = 10, color = "black")
    
    # Save the plot
    ggsave(paste0("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Milk/Survival_Curve_milk_", gsub(" ", "_", region), ".png"),
           plot = surv_plot$plot, width = 10, height = 8)
  } else {
    message("Insufficient data for survival analysis in region: ", region)
  }
}

#panel for only the select regions and exclude the ones with low CI-done manually in the folder
library(gridExtra)
library(ggplot2)
library(png)

# Set the path to the directory containing the graphs
graph_folder <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Milk/Select"

# List all PNG files in the directory
graph_files <- list.files(graph_folder, pattern = "\\.png$", full.names = TRUE)

# Load all graphs
graphs <- lapply(graph_files, function(file) {
  grid::rasterGrob(png::readPNG(file))
})

# Arrange the graphs in a panel
panel_plot <- gridExtra::grid.arrange(grobs = graphs, ncol = 2, widths = c(3, 3))

# Display the panel plot
grid::grid.draw(panel_plot)
# Save the combined plot as a PNG file
png(filename = "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Milk/Combined_Graphs_milk_select.png", width = 16, height = 18, units = "in", res = 300)
grid::grid.draw(panel_plot)
dev.off()

#probs at 1 yeat step for the map for scatter later
# Extract the summary of the km_fit object
summary_km <- summary(km_fit)

# Extract survival probabilities
time_points <- summary_km$time
surv_probs <- summary_km$surv

# Create a data frame with the extracted information
survival_data <- data.frame(Time = time_points, Survival_Probability = surv_probs)

# Filter data for 1-year intervals (assuming the data is in yearly steps)
yearly_survival_data <- survival_data[round(survival_data$Time) == survival_data$Time, ]
# View the data
print(yearly_survival_data)

#survival probs table for mapping
# Assuming 'km_fit' is a stratified Kaplan-Meier fit with stratification by 'Region', extra
# Extract the survival summary for each region
surv_summary <- summary(km_fit)

# Manually create the data frame
surv_df <- data.frame(
  Time = surv_summary$time,
  N.risk = surv_summary$n.risk,
  N.event = surv_summary$n.event,
  N.censor = surv_summary$n.censor,
  Survival = surv_summary$surv,
  StdErr = surv_summary$std.err,
  LowerCI95 = surv_summary$lower,
  UpperCI95 = surv_summary$upper,
  Region= surv_summary$strata
)
# Export the data frame to a CSV file
write.csv(surv_df, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Survsumm_region_milk_global_modified.csv", row.names = T)

#survival table
library(survival)
library(dplyr)

# Get unique list of regions
unique_regions <- unique(findata$Subregion_prod)

# Initialize an empty dataframe to store results
combined_survival_data <- data.frame()

# Loop over each region
for (region in unique_regions) {
  # Subset data for the current region
  region_data <- filter(findata, Subregion_prod == region)
  
  # Check if the subset has sufficient data
  if (nrow(region_data) > 0 && sum(!is.na(region_data$modifiedrecovery)) > 0) {
    # Create a survival object for the specific region
    Surv_obj_region <- with(region_data, Surv(modifiedrecovery))  # Replace 'Time' and 'Event' with actual column names
    
    # Fit the Kaplan-Meier survival function for the specific region
    km_fit_region <- survfit(Surv_obj_region ~ 1)
    
    # Extract the summary of the km_fit object
    summary_km_region <- summary(km_fit_region)
    
    # Extract survival probabilities at 1st and 2nd year
    one_two_year_survival <- summary_km_region$surv[summary_km_region$time %in% c(1, 2)]
    one_two_year_time <- summary_km_region$time[summary_km_region$time %in% c(1, 2)]
    
    # Create a data frame with the extracted information
    survival_data_region <- data.frame(Region = region, Time = one_two_year_time, Survival_Probability = one_two_year_survival)
    
    # Combine with the overall dataframe
    combined_survival_data <- rbind(combined_survival_data, survival_data_region)
  } else {
    message("Insufficient data for survival analysis in region: ", region)
  }
}

#old codes
# View the combined data
print(combined_survival_data)
write.csv(combined_survival_data,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/recoverytablesurv_milk.csv")

#filter second year data
library(dplyr)
combined_survival_data <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/recoverytablesurv_maize.csv")

# Filter the combined_survival_data for Time = 2
second_year_data <- combined_survival_data %>%
  filter(Time == 2)
# View the filtered data
print(second_year_data)

#keep only regions with robust results
library(dplyr)
# List of regions to keep
regions_to_keep <- c("Sub-Saharan Africa", "Western Asia", "Southern Asia", 
                     "Southern Europe", "Eastern Europe", "South-eastern Asia", 
                     "Latin America and the Caribbean")

# Filter the dataframe to keep only the specified regions
second_year_data <- second_year_data %>%
  filter(Region %in% regions_to_keep)

# View the result
print(second_year_data)

#bar graph if needed
ggplot(second_year_data, aes(x = Region, y = Survival_Probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Region", y = "Survival Probability", title = "2nd Year Survival Probabilities by Region")

#bubble chart
library(dplyr)
# Calculate shocks frequency for each region
shocks_frequency <- findata %>%
  group_by(Subregion_prod) %>%
  summarize(Shocks_Frequency = sum(Shock_Point_prod, na.rm = TRUE)) %>%
  ungroup()  # Ungrouping to prevent issues in subsequent operations

# Calculate average regional production for each region
average_production <- findata %>%
  group_by(Subregion_prod) %>%
  summarize(Avg_Regional_Production = median(Value_prod, na.rm = TRUE)) %>%
  ungroup()

# Rename the 'Subregion_prod' column in both dataframes to 'Region' for consistent merging
shocks_frequency <- rename(shocks_frequency, Region = Subregion_prod)
average_production <- rename(average_production, Region = Subregion_prod)

# Merge the shocks frequency and average production with second_year_data
second_year_combined_data <- second_year_data %>%
  left_join(shocks_frequency, by = "Region") %>%
  left_join(average_production, by = "Region")

# View the combined data
print(second_year_combined_data)

#now the bubble chart
library(ggplot2)
# Create the Bubble Chart with Adjusted Bubble Sizes
bubble_chart <- ggplot(second_year_combined_data, aes(x = Survival_Probability, y = Shocks_Frequency, size = Avg_Regional_Production, color = Region)) +
  geom_point(alpha = 0.7) +  # Adjust alpha as needed
  scale_size_continuous(name = "Regional Production", range = c(5, 20)) +  # Increase the range of bubble sizes
  theme_minimal() +
  labs(x = "Survival Probability in 2nd Year", 
       y = "Frequency of Shocks", 
       title = "Maize",
       subtitle = "") +
  scale_color_viridis_d() +  # Use a color palette suitable for categorical data
  theme(text = element_text(size = 16),  # General text size
        axis.title = element_text(size = 16),  # Axis titles
        axis.text = element_text(size = 16),  # Axis text
        plot.title = element_text(size = 16),  # Main title
        plot.subtitle = element_text(size = 16))  # Subtitle, Use a color palette suitable for categorical data

# Save the plot to a file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/bubble_chart_maize.png", bubble_chart, width = 10, height = 8, dpi = 300)

# Filter the combined_survival_data for Time = 1
first_year_data <- combined_survival_data %>%
  filter(Time == 1)

# View the filtered data
print(first_year_data)

#filter first year data
library(dplyr)

# List of regions to keep
regions_to_keep <- c("Sub-Saharan Africa", "Western Asia", "Southern Asia", 
                     "Southern Europe", "Eastern Europe", "South-eastern Asia", 
                     "Latin America and the Caribbean")

# Filter the dataframe to keep only the specified regions
first_year_data <- first_year_data %>%
  filter(Region %in% regions_to_keep)

# View the result
print(first_year_data)

#data prep
#bubble chart
library(dplyr)

# Calculate shocks frequency for each region
shocks_frequency <- findata %>%
  group_by(Subregion_prod) %>%
  summarize(Shocks_Frequency = sum(Shock_Point_prod, na.rm = TRUE)) %>%
  ungroup()  # Ungrouping to prevent issues in subsequent operations

# Calculate average regional production for each region
average_production <- findata %>%
  group_by(Subregion_prod) %>%
  summarize(Avg_Regional_Production = median(Value_prod, na.rm = TRUE)) %>%
  ungroup()

# Rename the 'Subregion_prod' column in both dataframes to 'Region' for consistent merging
shocks_frequency <- rename(shocks_frequency, Region = Subregion_prod)
average_production <- rename(average_production, Region = Subregion_prod)

# Merge the shocks frequency and average production with second_year_data
first_year_combined_data <- first_year_data %>%
  left_join(shocks_frequency, by = "Region") %>%
  left_join(average_production, by = "Region")

# View the combined data
print(first_year_combined_data)

#now the bubble chart
library(ggplot2)
library(viridis)

# Create the Bubble Chart with Adjusted Bubble Sizes
bubble_chart <- ggplot(first_year_combined_data, aes(x = Survival_Probability, y = Shocks_Frequency, size = Avg_Regional_Production, color = Region)) +
  geom_point(alpha = 0.7) +  # Adjust alpha as needed
  scale_size_continuous(name = "Regional Production", range = c(5, 20)) +  # Increase the range of bubble sizes
  theme_minimal() +
  labs(x = "Survival Probability in 1st Year", 
       y = "Frequency of Shocks", 
       title = "Maize",
       subtitle = "") +
  scale_color_viridis_d() +  # Use a color palette suitable for categorical data
  theme(text = element_text(size = 20),  # General text size
        axis.title = element_text(size = 20),  # Axis titles
        axis.text = element_text(size = 20),  # Axis text
        plot.title = element_text(size = 20),  # Main title
        plot.subtitle = element_text(size = 20))  # Subtitle, Use a color palette suitable for categorical data

# Save the plot to a file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/bubble_chart_maize1st.png", bubble_chart, width = 10, height = 8, dpi = 300)








#OLD CODES
#bubble chart
#now the bubble chart
#keep only regions with robust results
library(dplyr)
# List of regions to keep
regions_to_keep <- c("Sub-Saharan Africa", "Northern Europe", "Southern Asia", 
                     "Southern Europe", "Western Asia",  
                     "Latin America and the Caribbean")

# Filter the dataframe to keep only the specified regions
second_year_data <- second_year_data %>%
  filter(Region %in% regions_to_keep)

# View the result
print(second_year_data)

library(dplyr)
# Calculate shocks frequency for each region
shocks_frequency <- findata %>%
  group_by(Subregion_prod) %>%
  summarize(Shocks_Frequency = sum(Shock_Point_prod, na.rm = TRUE)) %>%
  ungroup()  # Ungrouping to prevent issues in subsequent operations

# Calculate average regional production for each region
average_production <- findata %>%
  group_by(Subregion_prod) %>%
  summarize(Avg_Regional_Production = median(Value_prod, na.rm = TRUE)) %>%
  ungroup()

# Rename the 'Subregion_prod' column in both dataframes to 'Region' for consistent merging
shocks_frequency <- rename(shocks_frequency, Region = Subregion_prod)
average_production <- rename(average_production, Region = Subregion_prod)

# Merge the shocks frequency and average production with second_year_data
second_year_combined_data <- second_year_data %>%
  left_join(shocks_frequency, by = "Region") %>%
  left_join(average_production, by = "Region")

# View the combined data
print(second_year_combined_data)

# Create the Bubble Chart with Adjusted Bubble Sizes
library(ggplot2)
# Create the Bubble Chart with Adjusted Bubble Sizes
bubble_chart <- ggplot(second_year_combined_data, aes(x = Survival_Probability, y = Shocks_Frequency, size = Avg_Regional_Production, color = Region)) +
  geom_point(alpha = 0.7) +  # Adjust alpha as needed
  scale_size_continuous(name = "Regional Production", range = c(5, 20)) +  # Increase the range of bubble sizes
  theme_minimal() +
  labs(x = "Survival Probability in 2nd Year", 
       y = "Frequency of Shocks", 
       title = "Milk",
       subtitle = "") +
  scale_color_viridis_d() +  # Use a color palette suitable for categorical data
  theme(text = element_text(size = 20),  # General text size
        axis.title = element_text(size = 20),  # Axis titles
        axis.text = element_text(size = 20),  # Axis text
        plot.title = element_text(size = 20),  # Main title
        plot.subtitle = element_text(size = 20))  # Subtitle, Use a color palette suitable for categorical data

# Save the plot to a file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/bubble_chart_milk2.png", bubble_chart, width = 10, height = 8, dpi = 300)

#filter first year data
library(dplyr)
# Filter the combined_survival_data for Time = 2
first_year_data <- combined_survival_data %>%
  filter(Time == 1)

# View the filtered data
print(first_year_data)
# List of regions to keep
regions_to_keep <- c("Sub-Saharan Africa", "Northern Europe", "Southern Asia", 
                     "Southern Europe", "Western Asia",  
                     "Latin America and the Caribbean")
# Filter the dataframe to keep only the specified regions
first_year_data <- first_year_data %>%
  filter(Region %in% regions_to_keep)
# View the result
print(first_year_data)

#data prep
#bubble chart
library(dplyr)
# Calculate shocks frequency for each region
shocks_frequency <- findata %>%
  group_by(Subregion_prod) %>%
  summarize(Shocks_Frequency = sum(Shock_Point_prod, na.rm = TRUE)) %>%
  ungroup()  # Ungrouping to prevent issues in subsequent operations
# Calculate average regional production for each region
average_production <- findata %>%
  group_by(Subregion_prod) %>%
  summarize(Avg_Regional_Production = median(Value_prod, na.rm = TRUE)) %>%
  ungroup()
# Rename the 'Subregion_prod' column in both dataframes to 'Region' for consistent merging
shocks_frequency <- rename(shocks_frequency, Region = Subregion_prod)
average_production <- rename(average_production, Region = Subregion_prod)

# Merge the shocks frequency and average production with second_year_data
first_year_combined_data <- first_year_data %>%
  left_join(shocks_frequency, by = "Region") %>%
  left_join(average_production, by = "Region")

# View the combined data
print(first_year_combined_data)

#now the bubble chart
library(ggplot2)
library(viridis)
# Create the Bubble Chart with Adjusted Bubble Sizes
library(ggplot2)

# Create the Bubble Chart with Adjusted Bubble Sizes
bubble_chart <- ggplot(first_year_combined_data, aes(x = Survival_Probability, y = Shocks_Frequency, size = Avg_Regional_Production, color = Region)) +
  geom_point(alpha = 0.7) +  # Adjust alpha as needed
  scale_size_continuous(name = "Regional Production", range = c(4, 18)) +  # Increase the range of bubble sizes
  theme_minimal() +
  labs(x = "Survival Probability in 1st Year", 
       y = "Frequency of Shocks", 
       title = "Milk",
       subtitle = "") +
  scale_color_viridis_d() +  # Use a color palette suitable for categorical data
  theme(text = element_text(size = 20),  # General text size
        axis.title = element_text(size = 20),  # Axis titles
        axis.text = element_text(size = 20),  # Axis text
        plot.title = element_text(size = 20),  # Main title
        plot.subtitle = element_text(size = 20))  # Subtitle, Use a color palette suitable for categorical data

# Save the plot to a file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/bubble_chart_milk1st.png", bubble_chart, width = 10, height = 8, dpi = 300)

###survival analysis
#################################survival sensitivity analysis by removing long recovery years
findata<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Recoverytime_milk_global_modified1.csv")
# Create a subset of findata where modifiedrecovery is less than 10
subset_findata <- findata %>%
  filter(modifiedrecovery < 11)
# View the first few rows of the subset
head(subset_findata)

# Calculate the number of shocks per region
shocks_per_region <- subset_findata %>%
  filter(Shock_Point_prod == TRUE) %>%
  group_by(Subregion_prod) %>%
  summarise(Num_Shocks = n())

# View the result
print(shocks_per_region)

#create a barchart
library(ggplot2)

# Create a bar chart
# Create a bar chart with tilted x-axis labels
bar<- ggplot(shocks_per_region, aes(x = Subregion_prod, y = Num_Shocks)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Region", y = "Number of Shocks", title = "Shocks Per Region")

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/regional_shock_bars_milk_subset.png", bar, width = 10, height = 8, dpi = 300)

# Calculate the number of shocks per country
shocks_per_country <- subset_findata %>%
  filter(Shock_Point_prod == TRUE) %>%
  group_by(ISO_prod) %>%
  summarise(Num_Shocks = n())

# View the result
print(shocks_per_country)

write.csv(shocks_per_country,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/shocksbycountry_milk_subset.csv")

#create shocks map
# Install and load necessary libraries
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Assuming you have a tibble like this
# your_data <- tibble(ISO_prod = c("USA", "FRA", "BRA"), Num_Shocks = c(10, 5, 7))

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
merged_data <- world %>% 
  left_join(shocks_per_country, by = c("iso_a3" = "ISO_prod"))

# Create the map
map_plot <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Num_Shocks), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +  # or any other color scale of your choice
  theme_minimal() +
  labs(fill = "Number of Shocks", title = "Map Showing Number of Shocks by Country")

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_shock_milkmap_subset.png",map_plot, width = 10, height = 8, dpi = 300)

# Calculate the average recovery time per region
avg_recovery_per_region <- subset_findata %>%
  group_by(Subregion_prod) %>%
  summarise(Avg_Recovery = mean(modifiedrecovery, na.rm = TRUE))

# View the result
print(avg_recovery_per_region)

##create a bar graph
bar<- ggplot(avg_recovery_per_region, aes(x = Subregion_prod, y = Avg_Recovery)) +
  geom_bar(stat = "identity", fill = "green") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Region", y = "Average recovery time", title = "Recovey time per region")

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/regional_avrec_bars_milk_subset.png", bar, width = 10, height = 8, dpi = 300)

#av rec time map
# Calculate the average recovery time per country
avg_recovery_per_region <- subset_findata %>%
  group_by(ISO_prod) %>%
  summarise(Avg_Recovery = mean(modifiedrecovery, na.rm = TRUE))

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
merged_data <- world %>% 
  left_join(avg_recovery_per_region, by = c("iso_a3" = "ISO_prod"))

# Create the map
library(ggplot2)
library(MetBrewer)

# Choose a MetBrewer palette
# Let's use the "Veronese" palette, but you can choose any other palette from MetBrewer
palette <- MetBrewer::met.brewer("Tam")

# Create the map with the MetBrewer palette
mapplot1 <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Avg_Recovery), color = "white") +
  scale_fill_gradientn(
    colours = palette,
    breaks = seq(min(merged_data$Avg_Recovery, na.rm = TRUE), 
                 max(merged_data$Avg_Recovery, na.rm = TRUE), by = 2),
    labels = seq(min(merged_data$Avg_Recovery, na.rm = TRUE), 
                 max(merged_data$Avg_Recovery, na.rm = TRUE), by = 2)
  ) +
  theme_minimal() +
  labs(fill = "Average Recovery Time")

mapplot1

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_avrec_milkmap_subset.png",mapplot1, width = 10, height = 8, dpi = 300)

# Create a survival object. Recovery times are right-censored (some observations may not experience the event).
# For our dataset, we'll assume that if `Recovery_Time` is NA, the event hasn't happened (censored).
Surv_obj <- with(subset_findata, Surv(modifiedrecovery))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)

library(survival)
library(survminer)

# Assuming km_fit is already created using survfit() and findata is your dataset
# Plot the Kaplan-Meier survival curve
surv_plot <- ggsurvplot(km_fit, data = findata,
                        xlab = "Time (in years)",
                        ylab = "Survival Probability",
                        title = "Kaplan-Meier Survival Curve: Recovery Time",
                        legend.title = "Survival Curve",
                        legend.labs = c("Recovery Time"),
                        risk.table = FALSE,  # Set to FALSE to not include risk table in the plot
                        conf.int = TRUE,
                        conf.int.fill = "grey",
                        conf.int.alpha = 0.2,
                        ylim = c(0, 1),
                        xlim = c(0, 10))

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_survfit_milk_subset.png", surv_plot$plot, width = 10, height = 8, dpi = 300)

#histogram for reference
library(ggplot2)
# Plotting a histogram of recovery times
hist<- ggplot(subset_findata, aes(x=modifiedrecovery)) + 
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) + 
  labs(title="Distribution of Recovery Times",
       x="Recovery Time (in years)", 
       y="Frequency") +
  theme_minimal()
# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Global_shockhist_milk_subset.png", hist, width = 10, height = 8, dpi = 300)

####subset by region-loop
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Assuming findata is your dataset and it contains 'Subregion_prod' for regions
# Get unique list of regions
unique_regions <- unique(subset_findata$Subregion_prod)

# Loop over each region
for (region in unique_regions) {
  # Filter data for the current region
  region_specific_df <- filter(subset_findata, Subregion_prod == region)
  
  # Check if the filtered data has non-missing observations for survival analysis
  if (nrow(region_specific_df) > 0 && sum(!is.na(region_specific_df$modifiedrecovery)) > 0) {
    # Create a survival object for the specific region
    Surv_obj_region <- with(region_specific_df, Surv(modifiedrecovery))
    
    # Fit the Kaplan-Meier survival function for the specific region
    km_fit_region <- survfit(Surv_obj_region ~ 1)
    
    # Plot the Kaplan-Meier survival curve for the specific region with 95% confidence intervals
    surv_plot <- ggsurvplot(km_fit_region, data = region_specific_df,
                            xlab = "Time (in years)",
                            ylab = "Survival Probability",
                            title = region,
                            legend.title = "Survival Curve",
                            legend.labs = c("Recovery Time"),
                            risk.table = FALSE,  # Include the number of subjects at risk
                            conf.int = TRUE,    # Include the 95% confidence intervals
                            conf.int.fill = "grey",  # Color for the shaded area
                            conf.int.alpha = 0.2,    # Transparency of the shaded area
                            ylim = c(0, 1),
                            xlim = c(0, 8))  # Adjust y-axis limits
    
    # Save the plot (extracting the plot component from the ggsurvplot object)
    ggsave(paste0("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Milk/Survival_Curve_milk_subset_", region, ".png"), surv_plot$plot, width = 10, height = 8)
  } else {
    message("Insufficient data for survival analysis in region: ", region)
  }
}

#panel
library(png)
library(grid)
library(gridExtra)

# Directory containing your graph PNG files
graph_dir <- "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Milk/"
graph_files <- list.files(graph_dir, pattern = "\\.png$", full.names = TRUE)

# Read and store the images in a list as rasterGrob objects
images <- lapply(graph_files, function(file) {
  img <- readPNG(file)
  rasterGrob(img)
})

# Determine the number of rows and columns for the grid layout
# This will create a layout that can accommodate all images
n_images <- length(images)
nrow_layout <- ceiling(sqrt(n_images))
ncol_layout <- ceiling(n_images / nrow_layout)

# Arrange the images in a grid layout
grid_layout <- do.call("grid.arrange", c(images, nrow = nrow_layout, ncol = ncol_layout))

# Save the combined plot as a PNG file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Region/Milk/Combined_Graphs_subset.png", grid_layout, width = 16, height = 18)

library(survival)
library(dplyr)
# Get unique list of regions
unique_regions <- unique(subset_findata$Subregion_prod)
# Initialize an empty dataframe to store results
combined_survival_data <- data.frame()
# Loop over each region
for (region in unique_regions) {
  # Subset data for the current region
  region_data <- filter(subset_findata, Subregion_prod == region)
  
  # Check if the subset has sufficient data
  if (nrow(region_data) > 0 && sum(!is.na(region_data$modifiedrecovery)) > 0) {
    # Create a survival object for the specific region
    Surv_obj_region <- with(region_data, Surv(modifiedrecovery))  # Replace 'Time' and 'Event' with actual column names
    
    # Fit the Kaplan-Meier survival function for the specific region
    km_fit_region <- survfit(Surv_obj_region ~ 1)
    
    # Extract the summary of the km_fit object
    summary_km_region <- summary(km_fit_region)
    
    # Extract survival probabilities at 1st and 2nd year
    one_two_year_survival <- summary_km_region$surv[summary_km_region$time %in% c(1, 2)]
    one_two_year_time <- summary_km_region$time[summary_km_region$time %in% c(1, 2)]
    
    # Create a data frame with the extracted information
    survival_data_region <- data.frame(Region = region, Time = one_two_year_time, Survival_Probability = one_two_year_survival)
    
    # Combine with the overall dataframe
    combined_survival_data <- rbind(combined_survival_data, survival_data_region)
  } else {
    message("Insufficient data for survival analysis in region: ", region)
  }
}

# View the combined data
print(combined_survival_data)

write.csv(combined_survival_data,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/recoverytablesurv_milk_subset.csv")

#filter secnd year data
library(dplyr)

# Filter the combined_survival_data for Time = 2
second_year_data <- combined_survival_data %>%
  filter(Time == 2)

# View the filtered data
print(second_year_data)

#bar graph if needed
ggplot(second_year_data, aes(x = Region, y = Survival_Probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Region", y = "Survival Probability", title = "2nd Year Survival Probabilities by Region")

#bubble chart
library(dplyr)

# Calculate shocks frequency for each region
shocks_frequency <- subset_findata %>%
  group_by(Subregion_prod) %>%
  summarize(Shocks_Frequency = sum(Shock_Point_prod, na.rm = TRUE)) %>%
  ungroup()  # Ungrouping to prevent issues in subsequent operations

# Calculate average regional production for each region
average_production <- subset_findata %>%
  group_by(Subregion_prod) %>%
  summarize(Avg_Regional_Production = median(Value_prod, na.rm = TRUE)) %>%
  ungroup()

# Rename the 'Subregion_prod' column in both dataframes to 'Region' for consistent merging
shocks_frequency <- rename(shocks_frequency, Region = Subregion_prod)
average_production <- rename(average_production, Region = Subregion_prod)

# Merge the shocks frequency and average production with second_year_data
second_year_combined_data <- second_year_data %>%
  left_join(shocks_frequency, by = "Region") %>%
  left_join(average_production, by = "Region")

# View the combined data
print(second_year_combined_data)

#now the bubble chart
library(ggplot2)
# Create the Bubble Chart with Adjusted Bubble Sizes
bubble_chart <- ggplot(second_year_combined_data, aes(x = Survival_Probability, y = Shocks_Frequency, size = Avg_Regional_Production, color = Region)) +
  geom_point(alpha = 0.7) +  # Adjust alpha as needed
  scale_size_continuous(name = "Regional Production", range = c(5, 20)) +  # Increase the range of bubble sizes
  theme_minimal() +
  labs(x = "Survival Probability in 2nd Year", 
       y = "Frequency of Shocks", 
       title = "Bubble Chart: Shocks Frequency vs. Survival Probability",
       subtitle = "Bubble size represents median regional production, color represents region") +
  scale_color_brewer(palette = "Set1")  # Use a color palette suitable for categorical data

# Save the plot to a file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/bubble_chart_milk_subset.png", bubble_chart, width = 10, height = 8, dpi = 300)

#filter first year data
library(dplyr)
# Filter the combined_survival_data for Time = 2
first_year_data <- combined_survival_data %>%
  filter(Time == 1)
# View the filtered data
print(first_year_data)

#data prep
#bubble chart
library(dplyr)
# Calculate shocks frequency for each region
shocks_frequency <- subset_findata %>%
  group_by(Subregion_prod) %>%
  summarize(Shocks_Frequency = sum(Shock_Point_prod, na.rm = TRUE)) %>%
  ungroup()  # Ungrouping to prevent issues in subsequent operations

# Calculate average regional production for each region
average_production <- subset_findata %>%
  group_by(Subregion_prod) %>%
  summarize(Avg_Regional_Production = median(Value_prod, na.rm = TRUE)) %>%
  ungroup()

# Rename the 'Subregion_prod' column in both dataframes to 'Region' for consistent merging
shocks_frequency <- rename(shocks_frequency, Region = Subregion_prod)
average_production <- rename(average_production, Region = Subregion_prod)

# Merge the shocks frequency and average production with second_year_data
first_year_combined_data <- first_year_data %>%
  left_join(shocks_frequency, by = "Region") %>%
  left_join(average_production, by = "Region")

# View the combined data
print(first_year_combined_data)

#now the bubble chart
library(ggplot2)
library(viridis)

# Create the Bubble Chart with Adjusted Bubble Sizes
bubble_chart <- ggplot(first_year_combined_data, aes(x = Survival_Probability, y = Shocks_Frequency, size = Avg_Regional_Production, color = Region)) +
  geom_point(alpha = 0.7) +  # Adjust alpha as needed
  scale_size_continuous(name = "Regional Production", range = c(5, 20)) +  # Increase the range of bubble sizes
  theme_minimal() +
  labs(x = "Survival Probability in 1st Year", 
       y = "Frequency of Shocks", 
       title = "Bubble Chart: Shocks Frequency vs. Survival Probability",
       subtitle = "Bubble size represents median regional production, color represents region") +
  scale_color_viridis_d()  # Use a color palette suitable for categorical data

# Save the plot to a file
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/bubble_chart_milk1st_subset.png", bubble_chart, width = 10, height = 8, dpi = 300)

#cooccurence heat map for both
library(ggplot2)
library(reshape2)

maize<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/recoverytablesurv_maize.csv")
milk <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/recoverytablesurv_milk.csv")

# Assuming you have two data frames 'maize_data' and 'milk_data' with columns 'Region', 'Survival_Probability'
# Merge the two data frames by 'Region'
combined_data <- merge(maize, milk, by = c("Region", "Time"), suffixes = c("_maize", "_milk"))

library(dplyr)

# Assuming 'Time' is a column in your combined_data
filtered_data <- combined_data %>%
  filter(Time == 1)

# Now create the survival_data data frame using the filtered data
survival_data <- data.frame(
  Region = filtered_data$Region,
  Survival_Probability_Maize = filtered_data$Survival_Probability_maize,
  Survival_Probability_Milk = filtered_data$Survival_Probability_milk
)

# Replace specific values with NA
survival_data$Survival_Probability_Maize[survival_data$Region %in% c("Northern Africa", "Northern America")] <- NA

# Melt the data for use with ggplot
library(reshape2)
melted_data <- melt(survival_data, id.vars = "Region", variable.name = "System", value.name = "Survival_Probability")

# Create the heatmap
ggplot(melted_data, aes(x = Region, y = System, fill = Survival_Probability)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "darkred", 
    mid = "darkorange", 
    high = "yellow", 
    midpoint = 0.5,
    breaks = seq(0, 1, by = 0.1) # Breaks at every 0.1 step
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key.size = unit(1, 'cm') # Adjust legend key size if needed
  ) +
  labs(
    fill = "Survival Probability", 
    x = "Region", 
    y = "System", 
    title = "Heatmap of Survival Probabilities for Maize and Milk Systems"
  )

#scatter
summary(survival_data$Survival_Probability_Maize)
summary(survival_data$Survival_Probability_Milk)
library(ggrepel)
scatter <- ggplot(survival_data, aes(x = Survival_Probability_Maize, y = Survival_Probability_Milk)) +
  geom_point(aes(color = Region), size = 5) +
  geom_text_repel(
    aes(label = Region),
    box.padding   = 0.35, 
    point.padding = 0.5,
    size = 4 # Increase the size here for the labels
  ) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(
    x = "Survival Probability for Maize",
    y = "Survival Probability for Milk",
    title = "Recovery likelihood: First year"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20), # Increase the size for the title
    axis.title = element_text(size = 16), # Increase the size for axis titles
    axis.text = element_text(size = 12) # Adjust size as needed for axis text
  ) +
  geom_vline(xintercept = 0.35, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.44, linetype = "dashed", color = "grey") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/scattermilkmaize.png", scatter, width = 10, height = 8, dpi = 300)

##############replicate for second year
#cooccurence heat map for both
library(ggplot2)
library(reshape2)

maize<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/recoverytablesurv_maize.csv")
milk <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/recoverytablesurv_milk.csv")

# Assuming you have two data frames 'maize_data' and 'milk_data' with columns 'Region', 'Survival_Probability'
# Merge the two data frames by 'Region'
combined_data <- merge(maize, milk, by = c("Region", "Time"), suffixes = c("_maize", "_milk"))

library(dplyr)

# Assuming 'Time' is a column in your combined_data
filtered_data <- combined_data %>%
  filter(Time == 2)

# Now create the survival_data data frame using the filtered data
survival_data <- data.frame(
  Region = filtered_data$Region,
  Survival_Probability_Maize = filtered_data$Survival_Probability_maize,
  Survival_Probability_Milk = filtered_data$Survival_Probability_milk
)

# Replace specific values with NA
survival_data$Survival_Probability_Maize[survival_data$Region %in% c("Northern Africa", "Northern America")] <- NA
#scatter
summary(survival_data$Survival_Probability_Maize)
summary(survival_data$Survival_Probability_Milk)
library(ggrepel)
scatter <- ggplot(survival_data, aes(x = Survival_Probability_Maize, y = Survival_Probability_Milk)) +
  geom_point(aes(color = Region), size = 5) +
  geom_text_repel(
    aes(label = Region),
    box.padding   = 0.35, 
    point.padding = 0.5,
    size = 4 # Increase the size here for the labels
  ) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(
    x = "Survival Probability for Maize",
    y = "Survival Probability for Milk",
    title = "Recovery likelihood: Second year"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20), # Increase the size for the title
    axis.title = element_text(size = 20), # Increase the size for axis titles
    axis.text = element_text(size = 20) # Adjust size as needed for axis text
  ) +
  geom_vline(xintercept = 0.18, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0.22, linetype = "dashed", color = "grey") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/scattermilkmaizesecondyear.png", scatter, width = 10, height = 8, dpi = 300)



#map for later
library(dplyr)
library(sf)
library(ggplot2)

# Classification based on your criteria
survival_data <- survival_data %>%
  mutate(
    Category = case_when(
      Survival_Probability_Maize > 0.45 & Survival_Probability_Milk > 0.45 ~ "High Maize, High Milk",
      Survival_Probability_Maize > 0.45 & Survival_Probability_Milk <= 0.45 ~ "High Maize, Low Milk",
      Survival_Probability_Maize <= 0.45 & Survival_Probability_Milk > 0.45 ~ "Low Maize, High Milk",
      TRUE ~ "Low Maize, Low Milk"
    )
  )

# Load your country-level shapefile
countries_shp <- st_read("path_to_your_shapefile/shapefile.shp")

# Assuming you have a dataframe that maps each country to its region
# It should have at least two columns: 'country' and 'region'
countries_regions <- read.csv("path_to_your_mapping/mapping.csv")

# Merge the region classification with the countries data
countries_classified <- countries_regions %>%
  left_join(survival_data, by = "region") %>%
  left_join(countries_shp, by = c("country" = "NAME")) # Replace "NAME" with the actual name of the country column in your shapefile

# Plot the map
ggplot(data = countries_classified) +
  geom_sf(aes(fill = Category), color = "white") +
  scale_fill_manual(values = c(
    "High Maize, High Milk" = "blue",
    "High Maize, Low Milk" = "green",
    "Low Maize, High Milk" = "yellow",
    "Low Maize, Low Milk" = "red"
  )) +
  theme_minimal() +
  labs(fill = "Category", title = "Map of Survival Probability Categories") +
  coord_sf() # Use coord_sf() to use the proper aspect ratio

#SENSITIVITY ANALYSIS MAIZE
#######################################method 3 Cottrell et al using Loess
#CHANGE YEARS
library(dplyr)
library(zoo)

#rm previous files
rm(rolling_baseline_data)
rm(loess_model)
rm(Fitted_Production)
rm(maize_data)
rm(single_country_data)

#5 years
# Assuming your data frame is named 'maize_data'
maize_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAOStat_Maize.csv")
colnames(maize_data)

#5 YEARS
# Assuming your data frame is named 'maize_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(maize_data) {
  return(maize_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 6, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.6)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(maize_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

# Now, all column names have the "_weather" suffix
print(colnames(rolling_baseline_data))

#save the file
write.csv(rolling_baseline_data, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_maize_global.csv", row.names = T)

#fix loess span and cooks distance values, inspect the fitted reg through plots
# Load required packages
#supplementary graphs
#fix loess span and cooks distance values, inspect the fitted reg through plots-you need to loop through for all the countries

###survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_maize_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)
# Merge the Continuous_Recovery column into the existing result_df
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))

write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")

#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window

#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(result)


#########################################################Survival analysis
#survival analysis
# Install and load the required packages
install.packages("survival")
install.packages("survminer")
rm(findata)
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(findatamaize)

# Create a survival object. Recovery times are right-censored (some observations may not experience the event).
# For our dataset, we'll assume that if `Recovery_Time` is NA, the event hasn't happened (censored).
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)

# Plot the Kaplan-Meier survival curve
library(survminer)
library(ggplot2)

# Adjusting the font size within the ggsurvplot function
surv_plot <- ggsurvplot(km_fit, data = findatamaize,
                        xlab = "Time (in years)",
                        ylab = "Recovery Likelihood",
                        title = "Kaplan-Meier Curve: Maize",
                        legend.title = "Recovery likelihood",
                        legend.labs = c("Recovery Time"),
                        risk.table = FALSE,
                        conf.int = TRUE,
                        conf.int.fill = "grey",
                        conf.int.alpha = 0.2,
                        ylim = c(0, 1),
                        xlim = c(0, 10),
                        font.main = 26,
                        font.x = 26,
                        font.y = 26,
                        font.tickslab = 26,
                        legend = "top")

# Print the plot
print(surv_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_maize_5yr.png", surv_plot$plot, width = 10, height = 8, dpi = 300)

#reverse KM graph
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(findatamaize)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob
summary(recovery_prob)
# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)
# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood
# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "orange",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Maize ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_rev_maize5.png", reverse_km_plot, width = 10, height = 8, dpi = 300)

#7 years
# Assuming your data frame is named 'maize_data'
maize_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAOStat_Maize.csv")
colnames(maize_data)

#7 YEARS
# Assuming your data frame is named 'maize_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(maize_data) {
  return(maize_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 8, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.6)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(maize_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

# Now, all column names have the "_weather" suffix
print(colnames(rolling_baseline_data))

#save the file
write.csv(rolling_baseline_data, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_maize_global.csv", row.names = T)

#fix loess span and cooks distance values, inspect the fitted reg through plots
# Load required packages
#supplementary graphs
#fix loess span and cooks distance values, inspect the fitted reg through plots-you need to loop through for all the countries

###survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_maize_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)
# Merge the Continuous_Recovery column into the existing result_df
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))

write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")

#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window

#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(result)


#########################################################Survival analysis
#survival analysis
# Install and load the required packages
install.packages("survival")
install.packages("survminer")
rm(findata)
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(findatamaize)

# Create a survival object. Recovery times are right-censored (some observations may not experience the event).
# For our dataset, we'll assume that if `Recovery_Time` is NA, the event hasn't happened (censored).
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)

# Plot the Kaplan-Meier survival curve
library(survminer)
library(ggplot2)

# Adjusting the font size within the ggsurvplot function
surv_plot <- ggsurvplot(km_fit, data = findatamaize,
                        xlab = "Time (in years)",
                        ylab = "Recovery Likelihood",
                        title = "Kaplan-Meier Curve: Maize",
                        legend.title = "Recovery likelihood",
                        legend.labs = c("Recovery Time"),
                        risk.table = FALSE,
                        conf.int = TRUE,
                        conf.int.fill = "grey",
                        conf.int.alpha = 0.2,
                        ylim = c(0, 1),
                        xlim = c(0, 10),
                        font.main = 26,
                        font.x = 26,
                        font.y = 26,
                        font.tickslab = 26,
                        legend = "top")

# Print the plot
print(surv_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_maize_7yr.png", surv_plot$plot, width = 10, height = 8, dpi = 300)

#reverse KM graph
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(findatamaize)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob
summary(recovery_prob)
# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)
# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood
# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "orange",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Maize ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_rev_maize7.png", reverse_km_plot, width = 10, height = 8, dpi = 300)

#7 years
# Assuming your data frame is named 'maize_data'
maize_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAOStat_Maize.csv")
colnames(maize_data)

#7 YEARS
# Assuming your data frame is named 'maize_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(maize_data) {
  return(maize_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 7, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.6)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(maize_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

# Now, all column names have the "_weather" suffix
print(colnames(rolling_baseline_data))

#save the file
write.csv(rolling_baseline_data, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_maize_global.csv", row.names = T)

#fix loess span and cooks distance values, inspect the fitted reg through plots
# Load required packages
#supplementary graphs
#fix loess span and cooks distance values, inspect the fitted reg through plots-you need to loop through for all the countries

###survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_maize_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)
# Merge the Continuous_Recovery column into the existing result_df
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))

write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")

#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window

#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(result)


#########################################################Survival analysis
#survival analysis
# Install and load the required packages
install.packages("survival")
install.packages("survminer")
rm(findata)
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(findatamaize)

# Create a survival object. Recovery times are right-censored (some observations may not experience the event).
# For our dataset, we'll assume that if `Recovery_Time` is NA, the event hasn't happened (censored).
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)

# Plot the Kaplan-Meier survival curve
library(survminer)
library(ggplot2)

# Adjusting the font size within the ggsurvplot function
surv_plot <- ggsurvplot(km_fit, data = findatamaize,
                        xlab = "Time (in years)",
                        ylab = "Recovery Likelihood",
                        title = "Kaplan-Meier Curve: Maize",
                        legend.title = "Recovery likelihood",
                        legend.labs = c("Recovery Time"),
                        risk.table = FALSE,
                        conf.int = TRUE,
                        conf.int.fill = "grey",
                        conf.int.alpha = 0.2,
                        ylim = c(0, 1),
                        xlim = c(0, 10),
                        font.main = 26,
                        font.x = 26,
                        font.y = 26,
                        font.tickslab = 26,
                        legend = "top")

# Print the plot
print(surv_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_maize_7yr.png", surv_plot$plot, width = 10, height = 8, dpi = 300)

#reverse KM graph
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(findatamaize)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob
summary(recovery_prob)
# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)
# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood
# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "orange",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Maize ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_rev_maize7.png", reverse_km_plot, width = 10, height = 8, dpi = 300)

#change span with 3 years, .8
# Assuming your data frame is named 'maize_data'
maize_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAOStat_Maize.csv")
colnames(maize_data)

# Assuming your data frame is named 'maize_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(maize_data) {
  return(maize_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 4, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.8)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(maize_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

###survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_maize_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)
# Merge the Continuous_Recovery column into the existing result_df
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))

write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")

#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window

#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(result)


#########################################################Survival analysis
#survival analysis
# Install and load the required packages
install.packages("survival")
install.packages("survminer")
rm(findata)
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(findatamaize)

# Create a survival object. Recovery times are right-censored (some observations may not experience the event).
# For our dataset, we'll assume that if `Recovery_Time` is NA, the event hasn't happened (censored).
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)

# Plot the Kaplan-Meier survival curve
library(survminer)
library(ggplot2)

# Adjusting the font size within the ggsurvplot function
surv_plot <- ggsurvplot(km_fit, data = findatamaize,
                        xlab = "Time (in years)",
                        ylab = "Recovery Likelihood",
                        title = "Kaplan-Meier Curve: Maize",
                        legend.title = "Recovery likelihood",
                        legend.labs = c("Recovery Time"),
                        risk.table = FALSE,
                        conf.int = TRUE,
                        conf.int.fill = "grey",
                        conf.int.alpha = 0.2,
                        ylim = c(0, 1),
                        xlim = c(0, 10),
                        font.main = 26,
                        font.x = 26,
                        font.y = 26,
                        font.tickslab = 26,
                        legend = "top")

# Print the plot
print(surv_plot)


#reverse KM graph
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_maize_global_modified.csv")
colnames(findatamaize)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob
summary(recovery_prob)
# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)
# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood
# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "orange",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Maize ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_rev_maize3span.8.png", reverse_km_plot, width = 10, height = 8, dpi = 300)

#SENSITIVITY MILK
#######################################method 3 Cottrell et al using Loess
#CHANGE YEARS
library(dplyr)
library(zoo)

#rm previous files
rm(rolling_baseline_data)
rm(loess_model)
rm(Fitted_Production)
rm(maize_data)
rm(single_country_data)

#5 years
# Assuming your data frame is named 'maize_data'
maize_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAO_Milk.csv")
colnames(maize_data)

#5 YEARS
# Assuming your data frame is named 'maize_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(maize_data) {
  return(maize_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 6, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.6)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(maize_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

# Now, all column names have the "_weather" suffix
print(colnames(rolling_baseline_data))

#save the file
write.csv(rolling_baseline_data, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_milk_global.csv", row.names = T)

#fix loess span and cooks distance values, inspect the fitted reg through plots
# Load required packages
#supplementary graphs
#fix loess span and cooks distance values, inspect the fitted reg through plots-you need to loop through for all the countries

###survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_milk_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)
# Merge the Continuous_Recovery column into the existing result_df
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))

write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")

#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window

#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")
colnames(result)


#########################################################Survival analysis
#reverse KM graph
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")
colnames(findatamaize)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob
summary(recovery_prob)
# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)
# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood
# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "blue",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Milk ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_rev_milk5.png", reverse_km_plot, width = 10, height = 8, dpi = 300)

library(dplyr)
library(zoo)

#rm previous files
rm(rolling_baseline_data)
rm(loess_model)
rm(Fitted_Production)
rm(maize_data)
rm(single_country_data)

#5 years
# Assuming your data frame is named 'maize_data'
maize_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAO_Milk.csv")
colnames(maize_data)

#5 YEARS
# Assuming your data frame is named 'maize_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(maize_data) {
  return(maize_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 6, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.6)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(maize_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

# Now, all column names have the "_weather" suffix
print(colnames(rolling_baseline_data))

#save the file
write.csv(rolling_baseline_data, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_milk_global.csv", row.names = T)

#fix loess span and cooks distance values, inspect the fitted reg through plots
# Load required packages
#supplementary graphs
#fix loess span and cooks distance values, inspect the fitted reg through plots-you need to loop through for all the countries

###survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_milk_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)
# Merge the Continuous_Recovery column into the existing result_df
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))

write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")

#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window

#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")
colnames(result)


#########################################################Survival analysis
#reverse KM graph
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")
colnames(findatamaize)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob
summary(recovery_prob)
# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)
# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood
# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "blue",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Milk ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_rev_milk5.png", reverse_km_plot, width = 10, height = 8, dpi = 300)

#7 years
# Assuming your data frame is named 'maize_data'
maize_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAO_Milk.csv")
colnames(maize_data)

#7 YEARS
# Assuming your data frame is named 'maize_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(maize_data) {
  return(maize_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 8, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.6)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(maize_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

# Now, all column names have the "_weather" suffix
print(colnames(rolling_baseline_data))

#save the file
write.csv(rolling_baseline_data, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_milk_global.csv", row.names = T)

#fix loess span and cooks distance values, inspect the fitted reg through plots
# Load required packages
#supplementary graphs
#fix loess span and cooks distance values, inspect the fitted reg through plots-you need to loop through for all the countries

###survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_milk_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)
# Merge the Continuous_Recovery column into the existing result_df
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))

write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")

#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window

#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")
colnames(result)


#########################################################Survival analysis
#reverse KM graph
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")
colnames(findatamaize)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob
summary(recovery_prob)
# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)
# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood
# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "blue",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Milk ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_rev_milk7.png", reverse_km_plot, width = 10, height = 8, dpi = 300)

#CHANGE SPAN, 3 YEARS .4
library(dplyr)
library(zoo)

#rm previous files
rm(rolling_baseline_data)
rm(loess_model)
rm(Fitted_Production)
rm(maize_data)
rm(single_country_data)

#5 years
# Assuming your data frame is named 'maize_data'
maize_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAO_Milk.csv")
colnames(maize_data)

#5 YEARS
# Assuming your data frame is named 'maize_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(maize_data) {
  return(maize_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 4, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.4)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(maize_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

# Now, all column names have the "_weather" suffix
print(colnames(rolling_baseline_data))

#save the file
write.csv(rolling_baseline_data, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_milk_global.csv", row.names = T)

#fix loess span and cooks distance values, inspect the fitted reg through plots
# Load required packages
#supplementary graphs
#fix loess span and cooks distance values, inspect the fitted reg through plots-you need to loop through for all the countries

###survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_milk_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)
# Merge the Continuous_Recovery column into the existing result_df
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))

write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")

#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window

#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")
colnames(result)


#########################################################Survival analysis
#reverse KM graph
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")
colnames(findatamaize)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob
summary(recovery_prob)
# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)
# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood
# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "blue",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Milk ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_rev_milk3span.4.png", reverse_km_plot, width = 10, height = 8, dpi = 300)

#CHANGE SPAN, 3 YEARS .8
library(dplyr)
library(zoo)

#rm previous files
rm(rolling_baseline_data)
rm(loess_model)
rm(Fitted_Production)
rm(maize_data)
rm(single_country_data)

#5 years
# Assuming your data frame is named 'maize_data'
maize_data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/FAO_Milk.csv")
colnames(maize_data)

#5 YEARS
# Assuming your data frame is named 'maize_data'
# Function to calculate rolling median baseline and identify shock points, calculate shock size
calculate_rolling_baseline_and_normalized_shock_size <- function(maize_data) {
  return(maize_data %>%
           group_by(Area) %>%
           arrange(Area, Year) %>%
           mutate(Rolling_Median_Production = zoo::rollapply(Value, width = 4, 
                                                             FUN = function(x) median(head(x, -1), na.rm = TRUE), 
                                                             fill = NA, align = "right"),
                  Loess_Fitted_Production = loess(Value ~ Year, span = 0.8)$fitted,
                  Residuals = Value - Loess_Fitted_Production,
                  Lag1_Residuals = lag(Residuals),
                  Cooksd = ifelse(!is.na(Lag1_Residuals), cooks.distance(lm(Residuals ~ Lag1_Residuals)), NA),
                  Shock_Point = ifelse(Cooksd > 0.1 & Value < Rolling_Median_Production, TRUE, FALSE),
                  Normalized_Shock_Size = ifelse(Shock_Point, (Rolling_Median_Production - Value) / Rolling_Median_Production, 0)))
}

# Calculate Rolling Median Baseline and identify shock points
rolling_baseline_data <- calculate_rolling_baseline_and_normalized_shock_size(maize_data)

#check the sum
sum_of_true_values <- sum(rolling_baseline_data$Shock_Point[rolling_baseline_data$Shock_Point == TRUE], na.rm = TRUE)
sum_of_true_values

# View the updated data frame with the Shock_Point column
print(rolling_baseline_data)
print(colnames(rolling_baseline_data))
#change col names for easy merging
# Add suffix "_prod" to all column names
colnames(rolling_baseline_data) <- paste0(colnames(rolling_baseline_data), "_prod")

# Now, all column names have the "_weather" suffix
print(colnames(rolling_baseline_data))

#save the file
write.csv(rolling_baseline_data, "D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_milk_global.csv", row.names = T)

#fix loess span and cooks distance values, inspect the fitted reg through plots
# Load required packages
#supplementary graphs
#fix loess span and cooks distance values, inspect the fitted reg through plots-you need to loop through for all the countries

###survival analysis
##################Data prep, add subset identifiers, create recovery quantity and years
data<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Prod_loess_milk_global.csv")
colnames(data)
calculate_shock_and_recovery <- function(data, latest_year) {
  data$Normalized_Shock_Size_prod <- rep(NA, nrow(data))
  data$Composite_Shock_Size_prod <- rep(NA, nrow(data))
  data$Recovery_Time_prod <- rep("Not calculated", nrow(data))
  
  for (i in 1:nrow(data)) {
    median_baseline <- data$Rolling_Median_Production_prod[i]
    
    # Normalized shock size for each individual shock year
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      data$Normalized_Shock_Size_prod[i] <- (median_baseline - data$Value_prod[i]) / median_baseline
      
      # Calculate recovery time for each shock
      j <- i + 1
      while (j <= nrow(data) && (is.na(data$Value_prod[j]) || data$Value_prod[j] < median_baseline * 0.95)) {
        j <- j + 1
      }
      if (j <= nrow(data)) {
        data$Recovery_Time_prod[i] <- as.character(data$Year_prod[j] - data$Year_prod[i])
      }
    }
    
    # Composite shock size calculation for consecutive shocks
    if (i > 1 && !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]) {
      avg_production_during_shocks <- mean(c(data$Value_prod[i - 1], data$Value_prod[i]), na.rm = TRUE)
      data$Composite_Shock_Size_prod[i - 1] <- (median_baseline - avg_production_during_shocks) / median_baseline
    }
  }
  
  return(data)
}

# Specify the latest year in your dataset
latest_year = 2021

# Apply the function to your dataset grouped by country
result_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_shock_and_recovery(., latest_year))

#####now deal with consecutive shocks
library(dplyr)
calculate_continuous_recovery <- function(data) {
  data$Continuous_Recovery_Time_prod <- rep(NA, nrow(data))
  
  in_recovery_period <- FALSE
  recovery_start_index <- NA
  
  for (i in 1:nrow(data)) {
    if (!is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]) {
      # Check if this is the start of a new recovery period
      if (!in_recovery_period) {
        in_recovery_period <- TRUE
        recovery_start_index <- i
      }
    } else {
      if (in_recovery_period) {
        # Check if recovery is achieved
        if (!is.na(data$Value_prod[i]) && data$Value_prod[i] >= data$Rolling_Median_Production_prod[recovery_start_index] * 0.95) {
          in_recovery_period <- FALSE
          data$Continuous_Recovery_Time_prod[recovery_start_index] <- data$Year_prod[i] - data$Year_prod[recovery_start_index]
        }
      }
    }
  }
  
  return(data)
}

# Apply the continuous recovery function to each group in the dataset
continuous_recovery_df <- data %>%
  group_by(ISO_prod) %>%
  do(calculate_continuous_recovery(.))
colnames(continuous_recovery_df)
# Merge the Continuous_Recovery column into the existing result_df
result_df <- result_df %>%
  left_join(select(continuous_recovery_df, ISO_prod, Year_prod, Continuous_Recovery_Time_prod), 
            by = c("ISO_prod", "Year_prod"))

#tag consecutive shocks
# and it's already grouped by country (if not, group it by country first)
tag_consecutive_shocks <- function(data) {
  data$Consecutive_Shock_Tag <- 0 # Initialize the tag column
  current_tag <- 0
  
  for (i in 2:nrow(data)) {
    # Check for NA values in Shock_Point_prod
    is_current_shock <- !is.na(data$Shock_Point_prod[i]) && data$Shock_Point_prod[i]
    is_prev_shock <- !is.na(data$Shock_Point_prod[i - 1]) && data$Shock_Point_prod[i - 1]
    
    if (is_current_shock && is_prev_shock) {
      # If both current and previous year have shocks
      data$Consecutive_Shock_Tag[i] <- current_tag
    } else if (is_current_shock) {
      # New shock sequence starts
      current_tag <- current_tag + 1
      data$Consecutive_Shock_Tag[i] <- current_tag
    }
    # If there is no shock, the tag remains 0 or the last tag value
  }
  
  return(data)
}

# Apply the function to each group (country) in the dataset
result_df <- result_df %>%
  group_by(ISO_prod) %>%
  do(tag_consecutive_shocks(.))

write.csv(result_df,"D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")

#check the sheet manually and adjust, recovery where the shock is near the end of observed periods, consecutive shock values remove for single shocks
#final cols were added after merging data for individual and conscutive shocks mannually, plus checking for countries which dont recover in the observed window

#START HERE-NOW CAN PROCEED WITH SURVIVAL FUNCTIONS WITH THIS MODIFIED DATA
#calculate shocks per region

library(dplyr)
#read in the file when redoing the analysis
result<- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")
colnames(result)


#########################################################Survival analysis
#reverse KM graph
library(survival)
library(survminer)
library(ggplot2)
findatamaize <- read.csv("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Recoverytime_milk_global_modified.csv")
colnames(findatamaize)

# Assuming findata is already loaded
# Create a survival object
Surv_obj <- with(findatamaize, Surv(Continuous_Recovery_Time_prod))

# Fit the Kaplan-Meier survival function
km_fit <- survfit(Surv_obj ~ 1)
km_summary <- summary(km_fit)
median_recovery_time <- km_summary$median
# Convert km_fit summary to a tidy data frame
km_summary_tidy <- broom::tidy(km_fit)

# Extract time points and survival probabilities
time <- km_fit$time
surv_prob <- km_fit$surv

# Calculate the cumulative probability of recovery
recovery_prob <- 1 - surv_prob
summary(recovery_prob)
# Create a data frame for plotting
plot_data <- data.frame(Time = time, RecoveryLikelihood = recovery_prob)

library(ggplot2)
# Assuming plot_data is already created from previous steps

# Ensure there's a row for time 0 with a RecoveryLikelihood of 0 if not already present
if(!0 %in% plot_data$Time) {
  plot_data <- rbind(data.frame(Time = 0, RecoveryLikelihood = 0), plot_data)
}

# Ensure data is ordered by Time after adding the new row
plot_data <- plot_data[order(plot_data$Time),]

library(ggplot2)

# Assuming plot_data is already created and contains Time and RecoveryLikelihood
# Plotting with adjusted x-axis intervals
reverse_km_plot <- ggplot(plot_data, aes(x = Time, y = RecoveryLikelihood)) +
  geom_step(color = "blue",size = 1.2) + # Using geom_step for the step function
  labs(x = "Time (in years)", y = "Recovery likelihood",
       title = "Reverse Kaplan-Meier Curve: Milk ") +
  theme_minimal() +
  theme(text = element_text(size = 22), # General text size for the plot
        axis.title.x = element_text(size = 30), # Increase x-axis label size
        axis.title.y = element_text(size = 30)) + # Increase y-axis label size
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) + # Setting y-axis from 0 to 1 with a scale of 0.1
  scale_x_continuous(limits = c(0, 12),breaks = seq(0, 12, by = 2)) # Setting x-axis intervals at 2 years

# Print the reverse KM plot
print(reverse_km_plot)

# Save the plot
ggsave("D:/OneDrive - CGIAR/Alliance/PhD/Recovery/Sensitivity/Global_survfit_rev_milk3span.8.png", reverse_km_plot, width = 10, height = 8, dpi = 300)

