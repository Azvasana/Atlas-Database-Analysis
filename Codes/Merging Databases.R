# Load necessary libraries
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(writexl)

df <- read_xlsx("Datasets/Annual Checkup Data.xlsx")

# Separate the Geolocation column into Longitude and Latitude columns
newdf <- df %>%
  mutate(
    Longitude = as.numeric(gsub('POINT \\((-?\\d+\\.\\d+) \\d+\\.\\d+\\)', '\\1', Geolocation)),
    Latitude = as.numeric(gsub('POINT \\(-?\\d+\\.\\d+ (-?\\d+\\.\\d+)\\)', '\\1', Geolocation))
  ) %>%
  select(-Geolocation)

# Write the modified data back to the Excel file
write_xlsx(newdf, "Modified Annual Checkup Data.xlsx")

## Merging Annual Checkup and DCI 
# Install and load necessary packages
install.packages("zipcodeR")
install.packages("readxl")
library(zipcodeR)
library(readxl)
library(writexl)

# Read the Excel file into R
data <- read_excel("Datasets/Merged DCI with Distances.xlsx")

# Assume the column with zip codes is named "zipcode"
zipcodes <- data$`Zip Code`

# Apply reverse_zipcode function to get the counties
counties <- sapply(zipcodes, function(z) {
  result <- reverse_zipcode(zip = z)
  if (!is.null(result$county)) {
    return(result$county)
  } else {
    return(NA) # If no county found for a zip code
  }
})

# Add the county information as a new column
data$county <- counties

# Write the modified dataset back to Excel
write_xlsx(data, "Datasets/Merged DCI with Distances.xlsx")

# Install and load necessary packages
install.packages("readxl")
library(readxl)
library(dplyr)
library(writexl)

# Read the Excel files
file1 <- read_excel("Datasets/Merged DCI with Distances.xlsx")
file2 <- read_excel("Datasets/Modified Annual Checkup Data.xlsx")

file1$County <- sub("County", "", file1$County)
file1$County <- gsub(" $", "", file1$County)

aggregated_data <- file2 %>%
  group_by(County) %>%
  summarise(Data_Value = mean(Data_Value, na.rm = TRUE))  # You can change mean to any other aggregation function

# Perform left join with aggregated data
merged_data <- file1 %>%
  left_join(aggregated_data, by = "County")

# View the merged dataset
print(merged_data)

# Write the merged dataset to a new Excel file if needed
write_xlsx(merged_data, "Datasets/Final DCI.xlsx")
