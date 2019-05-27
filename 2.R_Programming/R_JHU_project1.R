#part1 pollutantmean
#install.packages("data.table")
library("data.table")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  # Format number with fixed width and then append .csv to number
  fileNames <- paste0(directory, '/', formatC(id, width=3, flag="0"), ".csv" )
  
  # Reading in all files and making a large data.table
  lst <- lapply(fileNames, data.table::fread)
  dt <- rbindlist(lst)
  
  if (c(pollutant) %in% names(dt)){
    return(dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = pollutant][[1]])
  } 
}

# Example usage
pollutantmean(directory = '~/Documents/Github/datasciencecoursera/2.R_Programming/specdata', pollutant = 'sulfate', id = 1:10)
pollutantmean(directory = '~/Documents/Github/datasciencecoursera/2.R_Programming/specdata', pollutant = 'nitrate', id = 70:72)
pollutantmean(directory = '~/Documents/Github/datasciencecoursera/2.R_Programming/specdata', pollutant = 'sulfate', id = 34)
pollutantmean(directory = '~/Documents/Github/datasciencecoursera/2.R_Programming/specdata', pollutant = 'nitrate')

#part2 complete.R
complete <- function(directory,  id = 1:332) {
  
  # Format number with fixed width and then append .csv to number
  fileNames <- paste0(directory, '/', formatC(id, width=3, flag="0"), ".csv" )
  
  # Reading in all files and making a large data.table
  lst <- lapply(fileNames, data.table::fread)
  dt <- rbindlist(lst)
  
  return(dt[complete.cases(dt), .(nobs = .N), by = ID])
  
}

#Example usage
cc<-complete(directory = '~/Documents/Github/datasciencecoursera/2.R_Programming/specdata', id = c(6,10,20,34,100,200,310))
print(cc$nobs)

cc <- complete('~/Documents/Github/datasciencecoursera/2.R_Programming/specdata', id=54)
print(cc$nobs)

set.seed(42)
cc <- complete('~/Documents/Github/datasciencecoursera/2.R_Programming/specdata', id=332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

#part3 corr.R
corr <- function(directory, threshold = 0) {
  
  # Reading in all files and making a large data.table
  lst <- lapply(file.path(directory, list.files(path = directory, pattern="*.csv")), data.table::fread)
  dt <- rbindlist(lst)
  
  # Only keep completely observed cases
  dt <- dt[complete.cases(dt),]
  
  # Apply threshold
  dt <- dt[, .(nobs = .N, corr = cor(x = sulfate, y = nitrate)), by = ID][nobs > threshold]
  return(dt[, corr])
}

# Example Usage
cr<-corr(directory = '~/Documents/Github/datasciencecoursera/2.R_Programming/specdata')
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr('~/Documents/Github/datasciencecoursera/2.R_Programming/specdata',threshold = 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr('~/Documents/Github/datasciencecoursera/2.R_Programming/specdata', 2000)                
n <- length(cr)                
cr <- corr('~/Documents/Github/datasciencecoursera/2.R_Programming/specdata', 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
