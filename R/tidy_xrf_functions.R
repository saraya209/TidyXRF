# Function to Transform to Long Data
tidy_xrf <- function(raw_csv_file,
                     subset_file = NULL,
                     handle_lod = 0,
                     drop_na_concentration = TRUE,
                     save_excel = TRUE,
                     doreturn = TRUE,
                     elements_tbl = elements.tbl,
                     stdref_tbl = stdref.tbl,
                     compare_guide = FALSE) {
  
  
  ## Tidyup XRF exported table
  ## Args:
  # raw_csv_file: Full path to raw csv file
  # subset_file: Full path to csv file with selected elements. If NULL Elements are not subset.
  # handle_lod: one of c("0", "NA", "0.5"). "0": <LOD -> 0, "NA": <LOD -> NA, "0,5" -> Half of LOD 
  # save_excel_file: Full path to save tidy table.
  ## Returns
  ## Transformed tidy table
  #
  require("tidyverse")
  require("openxlsx")
  ## Read CSV all columns as character
  df = readr::read_csv(file = raw_csv_file,
                skip = 1,
                col_types = cols(.default = "c"))
 
  ## Element Symbol Sorted by Atomic Number
  element_symbol = elements_tbl %>% 
    dplyr::arrange(Atomic_Number) %>% 
    dplyr::pull(Symbol)
  #
  ## Select sample description columns
  desc_left_col = names(df)[1:which(names(df) == "Units")]
  desc_right_col = stringr::str_subset(names(df), "Real Time")
  desc_col = c(desc_left_col, desc_right_col)
  
  # Pivot table and Split measurement columns
  dfl = df %>% 
    dplyr::select(-starts_with(".")) %>% 
    tidyr::pivot_longer(cols = -any_of(desc_col),
                        values_to = "Raw_Value") %>% 
    tidyr::drop_na(Raw_Value) %>% 
    tidyr::separate(name, into = c("Analyte", "Variable"),
                    sep = " ", extra = "merge")
  ## Append if custom compounds that may exist in the table as element
  element_symbol = unique(c(element_symbol, unique(dfl$Analyte) ))
  

  # Convert elements into ordered factors sorted by atomic number
  dfl = dfl %>%
    dplyr::left_join(elements_tbl, by = c("Analyte" = "Symbol")) %>%
    dplyr::mutate(
      #LOD = if_else(Raw_Value == "<LOD", 1, 0),
      Value = ifelse(Raw_Value == "<LOD", handle_lod * LOD , as.numeric(Raw_Value)),
      Analyte = factor(Analyte, levels = element_symbol)
    )
  
  
  ## Wide
  
  if(is.character(subset_file)){
    ## Read CSV all columns as character
    element_subset = readr::read_csv(file = subset_file, 
                              col_types = cols(.default = "c"),
                              col_names = FALSE,
                              n_max = 1) %>% 
      unlist(use.names = FALSE) %>% 
      forcats::as_factor() ## Creates factor in the original order
    ##
    
    dfw = dfl %>% 
      dplyr::filter(as.character(Analyte) %in% as.character(element_subset) ) %>%
      dplyr::mutate(Analyte = factor(Analyte, 
                                     levels = levels(element_subset)) ) %>% 
      dplyr::arrange(Date, Time, Analyte) %>% 
      dplyr::ungroup()
    
  }else {
    dfw = dfl %>% 
      #dplyr::arrange(Date, Time, element) %>% 
      dplyr::ungroup()
  }
  
  dfw_c = dfw %>% 
    dplyr::filter(Variable == "Concentration") %>%
    dplyr::select(-any_of(c(names(elements_tbl),"Variable", "Raw_Value", "LOD"))) %>%
    {if(drop_na_concentration){
      tidyr::drop_na(., Value)
    }else {
      .
    }} %>% 
    tidyr::pivot_wider(names_from = Analyte, values_from = Value)
  
  dfw_e = dfw %>% 
    dplyr::filter(Variable %in% c("Concentration", "Error1s")) %>%
    dplyr::select(-any_of(c(names(elements_tbl), "Raw_Value", "LOD"))) %>%
    tidyr::pivot_wider(names_from = c(Analyte, Variable), 
                       values_from = Value,
                       names_sep = " ")
  
  ## PREPARE EXCEL EXPPORT
  
  params.df = tibble::tibble(
    VARIABLES = c(
      #"Original File Name:",
      "Subset Elements:",
      "Subset Elements List:",
      "Set <LOD Values To:",
      "Drop Elements With NA Concentrations"
    ),
    PARAMETERS = c(
      #basename(raw_csv_file),
      paste(is.character(subset_file)),
      ifelse(
        is.character(subset_file),
        paste(element_subset, collapse = ", "),
        ""
      ),
      ifelse(
        is.na(handle_lod),
        paste(handle_lod),
        ifelse(handle_lod == 0,
               paste(handle_lod),
               paste("LOD *", handle_lod))
      ),
      as.character(drop_na_concentration)
    )
  )
  
    ## create a workbook and add a worksheet
    wb <- createWorkbook()
    addWorksheet(wb, "Concentrations")
    addWorksheet(wb, "Concentrations-and-Error")
    addWorksheet(wb, "Tidy-Long-Format")
    addWorksheet(wb, "Original-Raw")
    addWorksheet(wb, "Tidying-Parameters")
    
    # Add data to workbook
    writeData(wb, "Concentrations", dfw_c)
    writeData(wb, "Concentrations-and-Error", dfw_e)
    writeData(wb, "Tidy-Long-Format", dfl %>% dplyr::select(-any_of(names(elements_tbl))))
    writeData(wb, "Original-Raw", df)
    writeData(wb, "Tidying-Parameters", params.df)
    
    # Conditional formatting cells
    #negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
    #posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
    
    #conditionalFormatting(wb, "CompareStandards", cols = 1, rows = 1:11, rule = "<0", style = negStyle)
    #conditionalFormatting(wb, "CompareStandards", cols = 1, rows = 1:11, rule = ">=0", style = posStyle)
    if(save_excel){
      save_excel_file = file.path(dirname(raw_csv_file),
                                  paste0("Tidy_", 
                                         strsplit(basename(raw_csv_file), split="\\.")[[1]][1],
                                         ".xlsx") )
    
    saveWorkbook(wb, file = save_excel_file, overwrite = TRUE)
  }
  
  if(doreturn){
    df_list = list(raw = df, 
                   long = dfl %>% dplyr::select(-any_of(names(elements_tbl))), 
                   wide = dfw_c,
                   excel = wb) 
    return(df_list)
  }
  
  
}
