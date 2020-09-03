#' Read ONDRI formatted DATA csvs
#' 
#' @param file_name character string. The name (and full path) of the _DATA file
#' @param missing_as_NA boolean. When TRUE, reads ONDRI's missing codes as NAs. Preserves them when FALSE
#' 
#' @return data.frame. Preserves the SUBJECT, VISIT, SITE, and DATE columns as "character", everything else is automatically determined by read.csv()
#' 


ondri_read_DATA <- function(file_name, missing_as_NA = T){
  
  
  for_header <- read.csv(file_name, header = F, nrows=1)
  ## this could be a bit better to actually search for SUBJECT, VISIT, *_SITE, and *_DATE
  ## also there may be more than four (e.g., NPSY_FULL)
  
  COL_CLASSES <- rep("character",4)
  names(COL_CLASSES) <- as.character(for_header[1:4])
  
  
  
  if(missing_as_NA){
    MISSING_CODES <- paste0("M_",c("CB","PI","VR","AE","DNA","TE","NP","ART","OTHER"))
    data_in <- read.csv(file_name, na.strings = MISSING_CODES, colClasses = COL_CLASSES, stringsAsFactors =  F)
  }else{
    data_in <- read.csv(file_name, colClasses = COL_CLASSES, stringsAsFactors = F)  
  }
  
  data_in
  
}
