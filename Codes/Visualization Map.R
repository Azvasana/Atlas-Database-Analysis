# Install and load necessary packages
install.packages(c("ggplot2", "ggmap", "dplyr", "readxl"))
library(ggplot2)
library(ggmap)
library(dplyr)
library(readxl)

# Replace with the coordinates of your hospital
hospital_location <- c(-76.592941, 39.296318)

# Read zip code data from Excel file
zip_data <- read_excel("~/Desktop/Research/NCDB Project/DCI.xlsx")

# Registering API Key
register_google(key = "AIzaSyA21KuZCAwK05gb5g5p3jP5tQvSJGjjeSA")

# Issue arises here onwards
# Geocode the zip codes to get their coordinates
zip_data <- mutate(zip_data, 
                   geocode_result = geocode(paste0(zip_data$Zip_Code, ", USA")),
                   latitude = sapply(geocode_result, function(x) x$lat),
                   longitude = sapply(geocode_result, function(x) x$lon))

# Calculate distances between zip codes and hospital
zip_data <- mutate(zip_data,
                   distance_to_hospital = geosphere::distVincentySphere(cbind(zip_data$longitude, zip_data$latitude), hospital_location))

# Plot map with distances
ggmap(get_map(location = hospital_location, zoom = 6)) +
  geom_point(data = zip_data, aes(x = longitude, y = latitude, color = distance_to_hospital), size = 3) +
  scale_color_gradient(name = "Distance to Hospital", low = "green", high = "red", guide = "legend") +
  labs(title = "Distances to Hospital from Zip Codes")
