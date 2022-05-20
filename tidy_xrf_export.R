#' ## Run Tidy Function Script Without Shiny App
#
#' Import Sample Result exported from 
#' "Olympus Vanta C-Series Handheld XRF Spectrometer" 
#' as `csv` format.
#' 
#
##load functions
source("global.R")
#
## File Path:
data_path <- file.path("data")
## File Names
raw_csv_filename = "chemistry-841524-2022-01-10-14-39-13"
subset_file_name = "selected_elements_list"

## Full paths for data and element subset list.
raw_csv_file = file.path(data_path, paste0(raw_csv_filename, ".csv"))
#' Also import standards data if applicable

subset_file = file.path(data_path, paste0(subset_file_name, ".csv"))
#' 
#' The table has about 11 columns which describe the sample (such as
#' instrument serial number, Date, Method, etc.). 
#' Sometimes there are columns that start with "Real Time" which are also part
#' of the description but are appended to the last columns. 
#' 
#' The remaining columns are a list of different element related variables
#' preceded by a symbol of the element or "LE" for "light elements". A list 
#' of the element variables are:
#' 
#'  - Compound
#'  - Compound Level
#'  - Compound Error
#'  - Concentration
#'  - Error1s
#'  
#'  There is also an empty last column which `readr` automatically names
#'  with column number in the form of, "...215". Remove empty column by pattern.

#' 


tidy_xrf(raw_csv_file = raw_csv_file,
         subset_file = NULL,
         set_lod_zero = FALSE,
         drop_na_concentration = TRUE,
         save_excel = TRUE,
         doreturn = FALSE)

