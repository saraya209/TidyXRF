##packages
library(DT)
library(shiny)
library(tidyverse)
library(openxlsx)
## import custom xrf tidy functions
source("R/tidy_xrf_functions.R", local = TRUE)
## Read Element and Standards list 
elements_file_path = "R/vanta_xrf_elements.xlsx"
elements_sheet_name = "xrf_elements"
stdref_file_path = "R/vanta_xrf_elements.xlsx"
stdref_sheet_name = "standards_desc"

elements.tbl <<- readxl::read_excel(path = elements_file_path,
                                  sheet = elements_sheet_name )
stdref.tbl <<- readxl::read_excel(path = stdref_file_path,
                                sheet =  stdref_sheet_name)