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
