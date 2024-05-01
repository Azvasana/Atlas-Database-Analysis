# States analysis 
# Load required library
library(ggplot2)

# Read the CSV file
data <- read.xlsx("Dartmouth Atlas/Dartmouth Atlas.xlsx")
column <- data$hrrstate

# Create a dataframe to count the frequency of each value in the column
freq_table <- as.data.frame(table(column))

# Plotting
ggplot(freq_table, aes(x = column, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Values", x = "Values", y = "Frequency")

# Distance Traveled Analysis 
# Load required libraries
library(readxl)
library(zipcodeR)
library(dplyr)
library(ggplot2)
library(writexl)

# Load data from Excel file
data <- read_excel("Dartmouth Atlas/Dartmouth Atlas.xlsx")

# Calculate distances between each zip code in the dataset and zip code 21287
distances <- sapply(data$zipcode19, function(zip) zip_distance(zipcode_a = zip, zipcode_b = "21218", units = "miles"))
calculate_distance <- function(zip) {
  distance <- zip_distance(zipcode_a = zip, zipcode_b = "21218", units = "miles")
  return(distance$distance)
}

# Johns Hopkins Hospital Zipcode does not exist, had to change to homewood campus zipcode for closest fit 
# Add a new column for distance traveled
data$Distance_to_21287 <- sapply(data$zipcode19, calculate_distance)

# Plot histogram of distances
hist(data$Distance_to_21287, breaks = 10, col = "skyblue", border = "black",
                  main = "Distance Travelled to Zip Code 21287", xlab = "Distance (miles)", ylab = "Frequency")
