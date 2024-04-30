# Load required libraries
library(readxl)
library(ggplot2)

# Function to create a histogram
create_histogram <- function(file_path, column_name, output_file_name) {
  # Read data from Excel file
  data <- readxl::read_excel(file_path)
  
  # Extract column data
  column_data <- data[[column_name]]
  
  # Create histogram
  histogram <- ggplot(data, aes_string(x = column_name)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +  # Adjust binwidth as needed
    labs(title = paste("Histogram of", column_name),
         x = column_name,
         y = "Frequency") +
    theme_minimal()
  
  # Save the histogram as a PNG file
  ggsave(output_file_name, plot = histogram, width = 6, height = 6)
  
  print("Histogram created successfully!")
}

create_histogram("example.xlsx", "Column1", "histogram.png")

## HISTOGRAM OF DISTANCE TRAVELED 
# Load required libraries
library(readxl)
library(zipcodeR)
library(dplyr)
library(ggplot2)
library(writexl)

# Load data from Excel file
data <- read_excel("~/Desktop/Research/NCDB Project/DCI.xlsx")

# Calculate distances between each zip code in the dataset and zip code 21287
distances <- sapply(data$Zip_Code, function(zip) zip_distance(zipcode_a = zip, zipcode_b = "21218", units = "miles"))
calculate_distance <- function(zip) {
  distance <- zip_distance(zipcode_a = zip, zipcode_b = "21218", units = "miles")
  return(distance$distance)
}

# Johns Hopkins Hospital Zipcode does not exist, had to change to homewood campus zipcode for closest fit 
# Add a new column for distance traveled
data$Distance_to_21287 <- sapply(data$Zip_Code, calculate_distance)

# Write the updated data back to the Excel file
write_xlsx(data, "DCI with Distances")

# Plot histogram of distances
distances <- hist(data$Distance_to_21287, breaks = 10, col = "skyblue", border = "black",
     main = "Distance Travelled to Zip Code 21287", xlab = "Distance (miles)", ylab = "Frequency")