# Convert zipcodes into geographical coordinates
# Install and load required libraries
install.packages("zipcodeR")
library(zipcodeR)
library(readxl)
library(dplyr)
library(writexl)

# Function to get coordinates for zip codes and store in separate columns
geocode_zipcodes <- function(zip_data, zip_column_name) {
  # Initialize vectors to store latitude and longitude
  latitude <- numeric(nrow(zip_data))
  longitude <- numeric(nrow(zip_data))
  
  # Iterate through each row of the data frame
  latitude <- for (i in seq_along(zip_data[[zip_column_name]])) {
    geocode_zip(i)$lat
  }
  longitude <- for (i in seq_along(zip_data[[zip_column_name]])) {
    geocode_zip(i)$lng
  }
  
  # Add latitude and longitude columns to the data frame
  zip_data <- cbind(zip_data, latitude, longitude)
  return(zip_data)
}

# Read Excel file containing zip code data
input_file <- "~/Desktop/Research/NCDB Project/DCI.xlsx"
zip_data <- readxl::read_excel(input_file, col_types = "text")

# Specify the name of the column containing zip codes
zip_column_name <- "Zip_Code"

# Process zip codes and store latitude and longitude in separate columns
zip_data_with_coords <- geocode_zipcodes(zip_data, zip_column_name)

# Save data with coordinates to another Excel file
output_file <- "~/Desktop/Research/NCDB Project/Zip Code Data.xlsx"
write_xlsx(zip_data_with_coords, output_file)

# Print message
print("Coordinates saved to output_excel_file.xlsx")