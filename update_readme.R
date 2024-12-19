# Load necessary library
library(stringr)

# Define the path to your README.md file
readme_path <- "README.md"

# Get the current date in "YYYY-MM-DD" format
current_date <- Sys.Date()

# Read the current README.md content
readme_content <- readLines(readme_path)

# Find the line with the "Last updated" placeholder and replace it with the current date
readme_content <- str_replace_all(readme_content, "<!--LAST_UPDATED-->", current_date)

# Write the modified content back to the README.md file
writeLines(readme_content, readme_path)

# Print confirmation
cat("README.md updated with the current date:", current_date)
