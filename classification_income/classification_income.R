# Load necessary libraries
library(dplyr)

# Define the path to the dataset (make sure this is the correct path)
file_path <- "C:/Users/wb617153/OneDrive - WBG/Desktop/LP_europe/LP_dataset.csv"

# Step 1: Read the dataset
data <- read.csv(file_path)

# Step 2: Filter out regional data (e.g., EU27_2020, EA19, EA20)
region_codes <- c('EU27_2020', 'EA19', 'EA20')
country_data <- data %>%
  filter(!geo %in% region_codes)

# Step 3: Calculate the median Real GDP per capita for each year
# For each year, we take the real GDP per capita of all countries and calculate the median
median_gdp_per_year <- country_data %>%
  group_by(Year) %>%
  summarize(median_gdp_per_capita = median(Real_GDP_per_capita, na.rm = TRUE))

# Step 4: Join the median GDP per capita back to the original dataset
country_data <- country_data %>%
  left_join(median_gdp_per_year, by = "Year")

# Step 5: Create the high-income dummy
# High-income countries (1) are those whose Real GDP per capita is above the yearly median
# Low-income countries (0) are those below the median
# Missing data (NA) will be kept as NA
country_data <- country_data %>%
  mutate(high_income_dummy = case_when(
    is.na(Real_GDP_per_capita) ~ NA_real_,  # Leave NA if no GDP data
    Real_GDP_per_capita > median_gdp_per_capita ~ 1,  # High-income country
    Real_GDP_per_capita <= median_gdp_per_capita ~ 0   # Low-income country
  ))

# Step 6: Save the updated dataset with high-income classification
# This saves the final dataset back to the working directory
output_path <- "C:/Users/wb617153/OneDrive - WBG/Desktop/LP_europe/final_high_income_classification.csv"
write.csv(country_data, file = output_path, row.names = FALSE)

# Done! The file has been saved as "final_high_income_classification.csv"
