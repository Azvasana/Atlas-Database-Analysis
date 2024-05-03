# Load required libraries
library(ggplot2)

# Function to create a pie chart
create_pie_chart <- function(file_path, column_name, output_file_name) {
  # Read data from Excel file
  data <- readxl::read_excel(file_path)
  
  # Extract column data
  column_data <- data[[column_name]]
  
  # Count occurrences of each unique value in the column
  value_counts <- table(column_data)
  
  # Create a data frame from value counts
  pie_data <- data.frame(names(value_counts), as.numeric(value_counts))
  colnames(pie_data) <- c("Category", "Count")
  
  # Create pie chart
  pie_chart <- ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste("Pie Chart of", column_name),
         fill = "Category",
         x = NULL,
         y = NULL) +
    theme_void()
  
  # Save the pie chart as a PNG file
  ggsave(output_file_name, plot = pie_chart, width = 6, height = 6)
  
  print("Pie chart created successfully!")
}

## Replace "example.xlsx" with the path to your Excel file, replace "Column1" with the name of the column containing the data for the pie chart, and replace "pie_chart.png" with the desired output file name
create_pie_chart("example.xlsx", "Column1", "pie_chart.png")

# People within Baltimore vs outside of Baltimore within Maryland 
# Load required packages
library(readxl)

# Read the Excel file
data <- read_excel("Datasets/Merged DCI with Distances.xlsx")

# Filter data by state (assuming 'State' is the column name)
maryland_data <- subset(data, hrrstate == "MD")

# Count the number of people from Baltimore and outside of Baltimore
baltimore_count <- sum(maryland_data$hrrcity == "Baltimore")
outside_baltimore_count <- sum(maryland_data$hrrcity != "Baltimore")

# Create a pie chart
pie_data <- c(Baltimore = baltimore_count, "Outside Baltimore" = outside_baltimore_count)
pie_labels <- paste(names(pie_data), "\n", round(pie_data / sum(pie_data) * 100, 1), "%", sep = "")
pie(pie_data, labels = pie_labels, main = "Distribution of People in Maryland", col = c("blue", "green"))

# Add a legend
legend("topright", legend = names(pie_data), fill = c("blue", "green"))

# Save the pie chart as a PNG file
png("Maryland Baltimore Distribution.png", width = 800, height = 600)
pie(pie_data, labels = pie_labels, main = "Distribution of People in Maryland", col = c("blue", "green"))
legend("topright", legend = names(pie_data), fill = c("blue", "green"))
dev.off()

# People within Maryland
# Count the number of people from each city within Maryland
city_counts <- table(maryland_data$hrrcity)

# Create a pie chart
pie(city_counts, main = "Distribution of People from Each City in Maryland")

# Save the pie chart as a PNG file
png("Maryland City Distribution.png", width = 800, height = 600)
pie(city_counts, main = "Distribution of People from Each City in Maryland")
dev.off()
