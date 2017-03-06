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