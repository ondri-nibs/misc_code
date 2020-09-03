#' Read ONDRI formatted DATA and change precision for all (numeric) columns
#' 
#' @param file_name character string. The name (and full path) of the _DATA file
#' @param digits numeric. Precision from the maximum decimals for all columns.
#' 
#' @return data.frame. Precision levels will differ from the original based on digits
#' 

ondri_format_all_columns <- function(file_name, digits = 3){
  
  MISSING_CODES <- paste0("M_",c("CB","PI","VR","AE","DNA","TE","NP","ART","OTHER"))
  
  for_header <- read.csv(file_name, header = F, nrows=1)
  COL_CLASSES <- rep("character",4)
  names(COL_CLASSES) <- as.character(for_header[1:4])
  
  
  data_NAs <- read.csv(file_name, na.strings = MISSING_CODES, colClasses = COL_CLASSES, stringsAsFactors =  F)
  data_NoNAs <- read.csv(file_name, colClasses = COL_CLASSES, stringsAsFactors = F)
  
  
  
  MISSING_MAP <- which(apply(data_NoNAs,c(1,2),'%in%',MISSING_CODES),arr.ind=T)
  
  
  NUMERIC_COLS <- unlist(lapply(data_NAs,
                                function(x){ is.numeric(x) }
  ))
  
  data_NAs[,NUMERIC_COLS] <- format( round(data_NAs[,NUMERIC_COLS], digits = digits ), nsmall = digits)
  
  if(nrow(MISSING_MAP)>0){
    
    data_NAs[MISSING_MAP] <-
      data_NoNAs[MISSING_MAP]
    
  }
  
  data_NAs
  
}
