# Function to create a scatter plot from an Excel file
create_scatterplot <- function(file_path, x_column, y_column, output_file_name) {
  # Read data from Excel file
  data <- readxl::read_excel(file_path)
  
  # Extract x and y column data
  x_data <- data[[x_column]]
  y_data <- data[[y_column]]
  
  # Create scatter plot
  scatter_plot <- ggplot(data, aes_string(x = x_column, y = y_column)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add line of best fit
    labs(title = paste("Scatter Plot of", x_column, "vs", y_column),
         x = x_column,
         y = y_column) +
    theme_minimal()
  
  # Save the scatter plot as a PNG file
  ggsave(output_file_name, plot = scatter_plot, width = 6, height = 6)
  
  print("Scatter plot created successfully!")
}


# Replace "example.xlsx" with the path to your Excel file, replace "X_Column" with the name of the column containing the x data, replace "Y_Column" with the name of the column containing the y data, replace "scatter_plot.png" with the desired output file name
create_scatterplot("example.xlsx", "X_Column", "Y_Column", "scatter_plot.png")

create_scatterplot("Datasets/Merged DCI with Distances.xlsx", "Distance_to_21287", "Crude_Rate", "Access to Healthcare as Distance.png")