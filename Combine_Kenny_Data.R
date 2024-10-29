library(dplyr)  # (Data manipulation)

# Set directory containing the og .csv files
file_directory <- "C:/Users/RW Aerial/Documents/Research Questions/Pendleton modeling/Kenny QCed Final datasets/CCS-A/CCS-A by Year"

# Set the directory to save the combined .csv file output
output_directory <- "C:/Users/RW Aerial/Documents/Research Questions/Pendleton modeling/Kenny QCed Final datasets/CCS-A/Combined CCS-A"

# Create the output directory
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

# List all .csv files in the directory (case-insensitive)
file_list <- list.files(path = file_directory, pattern = "(?i)\\.csv$", full.names = TRUE)

# Check that there are .csv files in the list
if (length(file_list) == 0) {
  stop("No .csv files found in the specified directory.")
}

# Function to read a CSV file and convert all columns to character (Doesn't like combining character & integer values in same column)
read_as_character <- function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE, colClasses = "character")
  return(data)
}

# Initialize combined_data as an empty data frame
combined_data <- data.frame(stringsAsFactors = FALSE)

# Read and append each .csv file, ensuring all columns are character
for (i in seq_along(file_list)) {
  temp_data <- read_as_character(file_list[i])
  combined_data <- bind_rows(combined_data, temp_data)
}

# Save the combined data to the new folder
output_file <- file.path(output_directory, "combined_data.csv")
write.csv(combined_data, file = output_file, row.names = FALSE)

# Print a message indicating the process is complete
cat("All CSV files have been combined (excluding duplicate headers) and saved as 'combined_data.csv'.\n")
