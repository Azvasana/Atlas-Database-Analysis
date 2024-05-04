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

create_histogram("Datasets/Merged DCI with Distances.xlsx", "Distance_to_21287", "Distance Traveled to Hospital.png")

## HISTOGRAM OF DISTANCE TRAVELED 
# Load required libraries
library(readxl)
library(zipcodeR)
library(dplyr)
library(ggplot2)
library(writexl)

# Load data from Excel file
data <- read_excel("~/Desktop/Research/Atlas Database Analysis/Datasets/Merged DCI.xlsx")

# Calculate distances between each zip code in the dataset and zip code 21287
calculate_distance <- function(zip) {
  distance <- zip_distance(zipcode_a = zip, zipcode_b = "21218", units = "miles")
  return(distance$distance)
}

# Johns Hopkins Hospital Zipcode does not exist, had to change to homewood campus zipcode for closest fit 
# Add a new column for distance traveled
data$Distance_to_21287 <- sapply(data$`Zip Code`, calculate_distance)

# Write the updated data back to the Excel file
write_xlsx(data, "Merged DCI with Distances.xlsx")

# Plot histogram of distances
histogram <- hist(data$Distance_to_21287, breaks = 10, col = "skyblue", border = "black",
     main = "Distance Travelled to Zip Code 21287", xlab = "Distance (miles)", ylab = "Frequency")

create_histogram("~/Desktop/Research/NCDB Project/DCI.xlsx", "Zip_Code", "Distances.png")

ggsave("Distances to Hospital.png", plot = histogram)


# Calculate distances between each zip code in the dataset and zip code 21287 within the state of Maryland
# Load required packages
library(readxl)

# Read the Excel file
data <- read_excel("Datasets/Merged DCI with Distances.xlsx")

# Filter data by state
maryland_distances <- subset(data, hrrstate == "MD")$Distance_to_21287

# Create a PNG file
png("maryland_distance_histogram.png", width = 800, height = 600)

# Create a histogram of distances traveled within Maryland
hist(maryland_distances, breaks = "Sturges", main = "Distance Travelled Between Zipcodes in Maryland", xlab = "Distance (miles)")

# Close the PNG device
dev.off()

# Calculating death and distribution of age 
# Filter data by death
subsetdata <- subset(data, DeathWithin90DaysofSurgery == 1 | DeathWithin30DaysofSurgery == 1)$Age

# Create a PNG file
png("Death Age Distribution.png", width = 800, height = 600)

# Create a histogram of distances traveled within Maryland
hist(subsetdata, breaks = "Sturges", main = "Distribution of Age Among Patients that have Died", xlab = "Age (years)")

# Close the PNG device
dev.off()
