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

data <- read_xlsx("Datasets/Merged DCI with Distances.xlsx")

# Load required library
library(zipcodeR)

data <- read_xlsx("Datasets/Merged DCI with Distances.xlsx")

data$latitude <- sapply(data$`Zip Code`, function(zip) geocode_zip(zip)$lat)
data$longitude <- sapply(data$`Zip Code`, function(zip) geocode_zip(zip)$lng)

write_xlsx(data, "Datasets/Merged DCI with Distances.xlsx")

# Load the library
library(zipcodeR)
library(ggmap)

# Assuming your latitude column is named 'latitude' and longitude column is named 'longitude'
# Assuming your data frame is named 'data'

data <- read_xlsx("Datasets/Modified Annual Checkup Data.xlsx")

register_google(key = "AIzaSyDb85qnWZF2caFWbUshB2MtwxZdlm8dWKk")

# Create a function to get ZIP code from latitude and longitude
get_zip_from_lat_long <- function(lat, long) {
  result <- revgeocode(c(long, lat), output="address")
  zip <- substr(result$postal_code, 1, 5) # Extract first 5 characters as ZIP code
  return(zip)
}

# Apply the function to your data frame and create a new column for ZIP code
data$zipcode <- mapply(get_zip_from_lat_long, data$Latitude, data$Longitude)