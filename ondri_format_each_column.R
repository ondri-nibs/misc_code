#' Read ONDRI formatted DATA and change precision for each (numeric) column
#' 
#' @param file_name character string. The name (and full path) of the _DATA file
#' @param digits numeric. When NA, determines precision from the maximum decimals in each column. Else when numeric, uses that value for precision
#' 
#' @return data.frame. Precision levels will differ from the original based on digits
#' 

ondri_format_each_column <- function(file_name, digits = NA){
  
  MISSING_CODES <- paste0("M_",c("CB","PI","VR","AE","DNA","TE","NP","ART","OTHER"))
  
  for_header <- read.csv(file_name, header = F, nrows=1)
  COL_CLASSES <- rep("character",4)
  names(COL_CLASSES) <- as.character(for_header[1:4])
  
  data_NAs <- read.csv(file_name, na.strings = MISSING_CODES, colClasses = COL_CLASSES, stringsAsFactors =  F)
  data_NoNAs <- read.csv(file_name, colClasses = COL_CLASSES, stringsAsFactors = F)
  
  
  
  MISSING_MAP <- which(apply(data_NoNAs,c(1,2),'%in%',MISSING_CODES),arr.ind=T)
  
  
  NUMERIC_NOT_INTEGER_COLS <- unlist(lapply(data_NAs,
                                            function(x){ !is.integer(x) & is.numeric(x) }
  ))
  
  
  for(i in which(NUMERIC_NOT_INTEGER_COLS)){
    
    number_decimals <- nchar(sapply(strsplit(as.character(  data_NAs[,i]  ), "\\."),"[",2))
    export_digits <- min(max(number_decimals, na.rm = T),digits, na.rm = T)
    
    data_NAs[,i] <- format( round(data_NAs[,i],digits=export_digits), nsmall=3)
    
  }
  
  if(nrow(MISSING_MAP)>0){
    
    data_NAs[MISSING_MAP] <-
      data_NoNAs[MISSING_MAP]
    
  }
  
  data_NAs
  
}

## enforce formatting within each column
ondri_format_all_columns <- function(file_name, digits = 3)
