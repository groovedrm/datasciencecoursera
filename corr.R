corr <- function(directory, threshold = 0) {
  
  dir_files <- as.character(list.files(directory))
  file_paths <- paste(directory, dir_files, sep="")
  frows <- NROW(file_paths)
  working_vector <- c()
  
  for(id in 1:frows) {
    work_file <- read.csv(file_paths[id], header=TRUE, sep=",")
    completes <- work_file[complete.cases(work_file), ]
    
    if (NROW(completes) > threshold) {
      working_vector <- c(working_vector, cor(completes$sulfate, completes$nitrate))
    }
  }
  
  return(working_vector)
}