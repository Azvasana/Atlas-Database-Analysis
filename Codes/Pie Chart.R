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