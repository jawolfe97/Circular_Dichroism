{#Environment Setup
{
#Clear Working Environment 
{
rm(list = ls())  # Removes all objects from the global environment
gc()  # Frees up memory#Clear Environment
}
#Package Setup
{
# List of required packages
required_packages <- c("rstudioapi")

# Function to check and install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)  # Install if not available
  }
  library(pkg, character.only = TRUE)  # Load the package
}

# Apply the function to each required package
sapply(required_packages, install_if_missing)

# Print confirmation
print("rstudioapi and ggplot2 packages are now loaded.")
}
#Locate Current File and Set Working Directory to Containing Folder
{
# Identify Operating System
os_name <- Sys.info()["sysname"]

# Get the path of the current file
file_path <- getSourceEditorContext()$path

if (!is.null(file_path) && file_path != "") {
  # Extract the directory from the file path
  dir_path <- dirname(file_path)
  
  # Handle Windows-specific backslashes
  if (os_name == "Windows") {
    dir_path <- gsub("/", "\\\\", dir_path)  # Convert forward slashes to backslashes
  }
  
  # Set the working directory
  setwd(dir_path)
  
  # Print confirmation
  print(paste("Operating System:", os_name))
  print(paste("Working directory set to:", getwd()))
} else {
  print("Error: Could not retrieve the script path. Make sure the file is saved.")
}
}
#Obtain_Files for Analysis
{
# Example list of file endings
file_endings <- c("ASC")

# Create a regular expression pattern dynamically
pattern <- paste(file_endings, collapse = "|")
pattern <- paste0(pattern, "$")  # Ensure it matches the end of the filenames

# List files with the derived pattern
FILES <- list.files(pattern = pattern, ignore.case = TRUE)
}
#Remove Unwanted Files for Analysis from List
{
FILES <- FILES[!FILES %in% list.files(pattern = "\\_Output.csv")] #Remove Files from Analysis List made by this Code
FILES <- FILES[!FILES %in% list.files(pattern = "\\HVs.o3a")] #Remove Files from Analysis List Containing HV values
FILES <- FILES[!FILES %in% list.files(pattern = "\\HVs.asc")] #Remove Files from Analysis List Containing HV values
FILES <- FILES[!FILES %in% list.files(pattern = "\\.xlsx")] #Remove Analysis Files from Analysis List 
FILES <- FILES[!FILES %in% list.files(pattern = "\\.pzfx")] #Remove Analysis Files from Analysis List
}
}

#Collate Files for Subtraction
{
# Ensure the length is divisible by number of unique first character of each string denoting number of measurements, otherwise, pad or truncate as needed
n <- length(FILES)
RUNS <- length(unique(substr(FILES, 1, 1)))
if (n %% RUNS != 0) {
  stop("Number of Files is not divisible by number of runs!")
}

# Reshape the list into 3 equal columns
FILES_MATRIX <- matrix(FILES, ncol = RUNS, byrow = FALSE)
}
}
###USER INPUTS HERE###
#Make Plots and Summary File
Buffer_Run_Label <- 2 #What is the starting number for the files containing the Buffer
Start_Wavelength <- 190
End_Wavelength <- 260
Plot_y_label = "m°"
Plot_x_label = "wavelength (nm)"
###USER INPUTS END HERE###
{
Buffer_Run_Label <- which(substr(FILES_MATRIX[1,], 1, 1) == Buffer_Run_Label)
N_Datapoints <- 5 #Number of Datapoints to Subtract from Baseline, This Assumes the Rightmost Values are the baseline.
OUTPUT_SUMMARY <- data.frame(Wavelength_nm = rep(0, End_Wavelength-Start_Wavelength+1)) #Write DataFrame for Final CSV File

for (n in 1:nrow(FILES_MATRIX)){
  for (m in 1:ncol(FILES_MATRIX)){
    if (FILES_MATRIX[n,Buffer_Run_Label] == FILES_MATRIX[n,m]) {
    }else{
  Buffer_DATA <- read.table(FILES_MATRIX[n,Buffer_Run_Label])
  Spectrum_DATA <- read.table(FILES_MATRIX[n,m])
  
  Subtracted_Data <- Spectrum_DATA$V2-Buffer_DATA$V2
  Baseline <- mean(tail(Spectrum_DATA$V2, N_Datapoints)) - mean(tail(Buffer_DATA$V2, N_Datapoints))
  Subtracted_Data <- Subtracted_Data - Baseline
  LABEL <- paste0(FILES_MATRIX[n,m],"_",FILES_MATRIX[n,Buffer_Run_Label])
  
  # Append the column with the dynamic name
  OUTPUT_SUMMARY[[LABEL]] <- Subtracted_Data
  
  #Plot and Save as PNG
  # Open a PNG device to save the plot
  png(paste0(LABEL,".png"), width = 800, height = 600)
  
  # Create the plot
  plot(
    Spectrum_DATA$V1,
    Subtracted_Data, 
    main = paste(LABEL),
    ylab = Plot_y_label,
    xlab = Plot_x_label
  )
  
  # Close the device to save the file
  dev.off()
  }
  }
}
OUTPUT_SUMMARY$Wavelength_nm <- Buffer_DATA$V1

# Write the data frame to a CSV file
write.csv(OUTPUT_SUMMARY, "Summary_Output.csv", row.names = FALSE)

#Clear Working Environment 
{
    rm(list = ls())  # Removes all objects from the global environment
    gc()  # Frees up memory#Clear Environment
    dev.off()
}
}
