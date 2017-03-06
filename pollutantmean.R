pollutantmean <- function(directory, pollutant, id = 1:332) {
  # Initialize some variables

  # Empty vector for means
  result_vector <- c()
  
  # List of filesassi
  dir_files <- as.character(list.files(directory))
  
  # File Paths
  file_paths <- paste(directory, dir_files, sep="")
  
  # Iterate through the files via I
  for (fid in id) {
    # Set the file and read it in
    working_file <- read.csv(file_paths[fid], header=TRUE, sep=",")

    # Create a version with just the clean values of the particular
    # pollutant that we wish to analyze
    clean_data <- working_file[!is.na(working_file[, pollutant]), pollutant]
    result_vector = c(result_vector, clean_data)
  }
  
  # Now, calculate mean across the combined results
  result <- mean(result_vector)
  # Could round, but won't, per course instructions
  
  return(result)
  
}

complete <- function(directory, id=1:332) {
  dir_files <- as.character(list.files(directory))
  file_paths <- paste(directory, dir_files, sep="")
  complete_file_observes <- data.frame(id = numeric(0), nobs = numeric(0))
  
  for (fid in id) {
    work_file = read.csv(file_paths[fid], header=TRUE, sep=",")
    completes = work_file[complete.cases(work_file), ]
    obs_row = data.frame(id = fid, nobs = NROW(completes))
    complete_file_observes = rbind(complete_file_observes, obs_row)
  }
  
  return(complete_file_observes)
}


  
