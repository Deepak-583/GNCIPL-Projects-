# =====================================================
# CROP YIELD EXPLORATORY DATA ANALYSIS (EDA)
# Professional Data Analysis using R
# =====================================================

# Load required libraries
library(dplyr)        # Data manipulation
library(ggplot2)      # Data visualization
library(readr)        # Data reading
library(corrplot)     # Correlation plots
library(gridExtra)    # Multiple plots
library(VIM)          # Missing value analysis
library(summarytools) # Summary statistics
library(plotly)       # Interactive plots

# Set working directory (adjust path as needed)
# setwd("C:/Mark Nicholas/Works/Python/ML")

# =====================================================
# 1. DATA LOADING AND INITIAL INSPECTION
# =====================================================

# Load the cleaned dataset
crop_data <- read_csv("crop_yield_cleaned_postgresql.csv")

# Basic dataset information
cat("=== DATASET OVERVIEW ===\n")
cat("Dataset Dimensions:", dim(crop_data), "\n")
cat("Column Names:", colnames(crop_data), "\n")
cat("Data Types:\n")
str(crop_data)

# First few rows
cat("\n=== FIRST 6 ROWS ===\n")
head(crop_data)

# Last few rows
cat("\n=== LAST 6 ROWS ===\n")
tail(crop_data)

# =====================================================
# 2. DATA QUALITY ASSESSMENT
# =====================================================

cat("\n=== DATA QUALITY ASSESSMENT ===\n")

# Missing values check
cat("Missing Values Summary:\n")
missing_summary <- crop_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Variable", value = "Missing_Count") %>%
  arrange(desc(Missing_Count))

print(missing_summary)

# Duplicate records check
duplicate_count <- sum(duplicated(crop_data))
cat("Duplicate Records:", duplicate_count, "\n")

# Data type validation
cat("\nData Type Summary:\n")
sapply(crop_data, class)

# =====================================================
# 3. DESCRIPTIVE STATISTICS
# =====================================================

cat("\n=== DESCRIPTIVE STATISTICS ===\n")

# Summary statistics for numerical variables
numerical_vars <- c("crop_year", "area", "production", "annual_rainfall", 
                   "fertilizer", "pesticide", "yield")

cat("Summary Statistics for Numerical Variables:\n")
summary(crop_data[numerical_vars])

# Detailed statistics
cat("\nDetailed Statistics:\n")
crop_data[numerical_vars] %>%
  summarise_all(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    q25 = ~quantile(., 0.25, na.rm = TRUE),
    q75 = ~quantile(., 0.75, na.rm = TRUE)
  )) %>%
  round(2)

# Categorical variables summary
cat("\nCategorical Variables Summary:\n")
cat("Unique Crops:", length(unique(crop_data$crop)), "\n")
cat("Unique States:", length(unique(crop_data$state)), "\n")
cat("Unique Seasons:", length(unique(crop_data$season)), "\n")
cat("Year Range:", min(crop_data$crop_year), "to", max(crop_data$crop_year), "\n")

# =====================================================
# 4. DATA VISUALIZATION
# =====================================================

# Create output directory for plots
if (!dir.exists("EDA_Plots")) {
  dir.create("EDA_Plots")
}

# 4.1 Distribution of Yield
p1 <- ggplot(crop_data, aes(x = yield)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Crop Yield",
       x = "Yield (tonnes/hectare)",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Log-transformed yield distribution
p2 <- ggplot(crop_data, aes(x = log(yield + 1))) +
  geom_histogram(bins = 50, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Log-Transformed Yield",
       x = "Log(Yield + 1)",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save yield distribution plots
ggsave("EDA_Plots/yield_distribution.png", p1, width = 10, height = 6, dpi = 300)
ggsave("EDA_Plots/yield_log_distribution.png", p2, width = 10, height = 6, dpi = 300)

# 4.2 Yield by Crop Type (Top 15 crops)
top_crops <- crop_data %>%
  group_by(crop) %>%
  summarise(avg_yield = mean(yield, na.rm = TRUE),
            count = n()) %>%
  filter(count >= 50) %>%  # Only crops with sufficient data
  arrange(desc(avg_yield)) %>%
  head(15)

p3 <- ggplot(top_crops, aes(x = reorder(crop, avg_yield), y = avg_yield)) +
  geom_col(fill = "coral", alpha = 0.8) +
  coord_flip() +
  labs(title = "Average Yield by Crop Type (Top 15)",
       x = "Crop Type",
       y = "Average Yield (tonnes/hectare)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("EDA_Plots/yield_by_crop.png", p3, width = 12, height = 8, dpi = 300)

# 4.3 Yield by State (Top 15 states)
top_states <- crop_data %>%
  group_by(state) %>%
  summarise(avg_yield = mean(yield, na.rm = TRUE),
            count = n()) %>%
  filter(count >= 100) %>%  # Only states with sufficient data
  arrange(desc(avg_yield)) %>%
  head(15)

p4 <- ggplot(top_states, aes(x = reorder(state, avg_yield), y = avg_yield)) +
  geom_col(fill = "lightblue", alpha = 0.8) +
  coord_flip() +
  labs(title = "Average Yield by State (Top 15)",
       x = "State",
       y = "Average Yield (tonnes/hectare)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("EDA_Plots/yield_by_state.png", p4, width = 12, height = 8, dpi = 300)

# 4.4 Yield by Season
p5 <- ggplot(crop_data, aes(x = season, y = yield, fill = season)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Yield Distribution by Season",
       x = "Season",
       y = "Yield (tonnes/hectare)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  coord_flip()

ggsave("EDA_Plots/yield_by_season.png", p5, width = 10, height = 6, dpi = 300)

# 4.5 Yield Trends Over Time
yearly_yield <- crop_data %>%
  group_by(crop_year) %>%
  summarise(avg_yield = mean(yield, na.rm = TRUE),
            median_yield = median(yield, na.rm = TRUE),
            count = n())

p6 <- ggplot(yearly_yield, aes(x = crop_year)) +
  geom_line(aes(y = avg_yield, color = "Average"), size = 1.2) +
  geom_line(aes(y = median_yield, color = "Median"), size = 1.2) +
  labs(title = "Crop Yield Trends Over Time (1997-2020)",
       x = "Year",
       y = "Yield (tonnes/hectare)",
       color = "Metric") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Average" = "blue", "Median" = "red"))

ggsave("EDA_Plots/yield_trends.png", p6, width = 12, height = 6, dpi = 300)

# =====================================================
# 5. CORRELATION ANALYSIS
# =====================================================

# Correlation matrix for numerical variables
correlation_matrix <- cor(crop_data[numerical_vars], use = "complete.obs")

# Correlation plot
png("EDA_Plots/correlation_matrix.png", width = 10, height = 8, units = "in", res = 300)
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black",
         title = "Correlation Matrix of Numerical Variables",
         mar = c(0,0,2,0))
dev.off()

# Print correlation with yield
cat("\n=== CORRELATION WITH YIELD ===\n")
yield_correlations <- correlation_matrix["yield", ]
yield_correlations <- yield_correlations[order(abs(yield_correlations), decreasing = TRUE)]
print(round(yield_correlations, 3))

# =====================================================
# 6. SCATTER PLOT ANALYSIS
# =====================================================

# 6.1 Yield vs Area
p7 <- ggplot(crop_data, aes(x = area, y = yield)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Yield vs Area",
       x = "Area (hectares)",
       y = "Yield (tonnes/hectare)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 6.2 Yield vs Annual Rainfall
p8 <- ggplot(crop_data, aes(x = annual_rainfall, y = yield)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Yield vs Annual Rainfall",
       x = "Annual Rainfall (mm)",
       y = "Yield (tonnes/hectare)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 6.3 Yield vs Fertilizer
p9 <- ggplot(crop_data, aes(x = fertilizer, y = yield)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Yield vs Fertilizer Usage",
       x = "Fertilizer (kg)",
       y = "Yield (tonnes/hectare)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save scatter plots
ggsave("EDA_Plots/yield_vs_area.png", p7, width = 10, height = 6, dpi = 300)
ggsave("EDA_Plots/yield_vs_rainfall.png", p8, width = 10, height = 6, dpi = 300)
ggsave("EDA_Plots/yield_vs_fertilizer.png", p9, width = 10, height = 6, dpi = 300)

# =====================================================
# 7. OUTLIER ANALYSIS
# =====================================================

cat("\n=== OUTLIER ANALYSIS ===\n")

# Function to detect outliers using IQR method
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Detect outliers in yield
yield_outliers <- detect_outliers(crop_data$yield)
outlier_count <- sum(yield_outliers, na.rm = TRUE)
cat("Yield Outliers (IQR method):", outlier_count, "(", 
    round(outlier_count/nrow(crop_data)*100, 2), "%)\n")

# Outlier summary
outlier_summary <- crop_data[yield_outliers, ] %>%
  summarise(
    min_yield = min(yield, na.rm = TRUE),
    max_yield = max(yield, na.rm = TRUE),
    avg_yield = mean(yield, na.rm = TRUE),
    count = n()
  )
print(outlier_summary)

# =====================================================
# 8. SEASONAL ANALYSIS
# =====================================================

cat("\n=== SEASONAL ANALYSIS ===\n")

# Yield by season summary
seasonal_summary <- crop_data %>%
  group_by(season) %>%
  summarise(
    count = n(),
    avg_yield = mean(yield, na.rm = TRUE),
    median_yield = median(yield, na.rm = TRUE),
    sd_yield = sd(yield, na.rm = TRUE),
    min_yield = min(yield, na.rm = TRUE),
    max_yield = max(yield, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_yield))

print(seasonal_summary)

# =====================================================
# 9. GEOGRAPHICAL ANALYSIS
# =====================================================

cat("\n=== GEOGRAPHICAL ANALYSIS ===\n")

# Top 10 states by average yield
top_states_detailed <- crop_data %>%
  group_by(state) %>%
  summarise(
    count = n(),
    avg_yield = mean(yield, na.rm = TRUE),
    total_production = sum(production, na.rm = TRUE),
    total_area = sum(area, na.rm = TRUE)
  ) %>%
  filter(count >= 50) %>%
  arrange(desc(avg_yield)) %>%
  head(10)

print(top_states_detailed)

# =====================================================
# 10. CROP-SPECIFIC ANALYSIS
# =====================================================

cat("\n=== CROP-SPECIFIC ANALYSIS ===\n")

# Top 10 crops by average yield
top_crops_detailed <- crop_data %>%
  group_by(crop) %>%
  summarise(
    count = n(),
    avg_yield = mean(yield, na.rm = TRUE),
    total_production = sum(production, na.rm = TRUE),
    total_area = sum(area, na.rm = TRUE),
    avg_rainfall = mean(annual_rainfall, na.rm = TRUE)
  ) %>%
  filter(count >= 50) %>%
  arrange(desc(avg_yield)) %>%
  head(10)

print(top_crops_detailed)

# =====================================================
# 11. EXPORT SUMMARY STATISTICS
# =====================================================

# Create comprehensive summary
eda_summary <- list(
  dataset_info = list(
    total_records = nrow(crop_data),
    total_columns = ncol(crop_data),
    unique_crops = length(unique(crop_data$crop)),
    unique_states = length(unique(crop_data$state)),
    year_range = paste(min(crop_data$crop_year), "to", max(crop_data$crop_year))
  ),
  yield_statistics = summary(crop_data$yield),
  correlation_with_yield = yield_correlations,
  seasonal_summary = seasonal_summary,
  top_states = top_states_detailed,
  top_crops = top_crops_detailed
)

# Save summary to file
write.csv(seasonal_summary, "EDA_Plots/seasonal_summary.csv", row.names = FALSE)
write.csv(top_states_detailed, "EDA_Plots/top_states_analysis.csv", row.names = FALSE)
write.csv(top_crops_detailed, "EDA_Plots/top_crops_analysis.csv", row.names = FALSE)

# =====================================================
# 12. FINAL SUMMARY
# =====================================================

cat("\n=== EDA COMPLETED ===\n")
cat("Total Records Analyzed:", nrow(crop_data), "\n")
cat("Plots saved in 'EDA_Plots' directory\n")
cat("Summary files exported as CSV\n")
cat("Key Findings:\n")
cat("- Average yield:", round(mean(crop_data$yield, na.rm = TRUE), 2), "tonnes/hectare\n")
cat("- Yield range:", round(min(crop_data$yield, na.rm = TRUE), 2), "to", 
    round(max(crop_data$yield, na.rm = TRUE), 2), "tonnes/hectare\n")
cat("- Most productive season:", seasonal_summary$season[1], "\n")
cat("- Top performing state:", top_states_detailed$state[1], "\n")
cat("- Top performing crop:", top_crops_detailed$crop[1], "\n")

cat("\nEDA Analysis Complete! Check the 'EDA_Plots' folder for visualizations.\n")
