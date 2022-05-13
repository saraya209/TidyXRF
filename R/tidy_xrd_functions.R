# Function to Transform to Long Data
tidy_xrf <- function(raw_csv_file,
                     subset_file = NULL,
                     set_lod_zero = FALSE,
                     drop_na_concentration = TRUE,
                     save_excel = TRUE,
                     doreturn = TRUE) {
  
  
  ## Tidyup XRF exported table
  ## Args:
  # raw_csv_file: Full path to raw csv file
  # subset_file: Full path to csv file with selected elements. If NULL Elements are not subset.
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
  element_symbol = c('H', 'He', 'Li', 'Be', 'B', 'C', 'N', 'O', 'F', 'Ne', 
                     'Na', 'Mg', 'Al', 'Si', 'P', 'S', 'Cl', 'Ar', 'K', 
                     'Ca', 'Sc', 'Ti', 'V', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 
                     'Cu', 'Zn', 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 
                     'Sr', 'Y', 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 
                     'Ag', 'Cd', 'In', 'Sn', 'Sb', 'Te', 'I', 'Xe', 'Cs', 
                     'Ba', 'La', 'Ce', 'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 
                     'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb', 'Lu', 'Hf', 'Ta', 
                     'W', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', 'Tl', 'Pb', 
                     'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', 'Pa', 
                     'U', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm', 
                     'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt', 
                     'Ds', 'Rg', 'Cn', 'Uut', 'Fl', 'Uup', 'Lv', 'Uus', 'Uuo')
  #
  ## Option to deal with <LOD
  if(set_lod_zero){
    lod_value = "0"
  }else{
    lod_value = NA_character_
  }
  ## Select sample description columns
  desc_left_col = names(df)[1:which(names(df) == "Units")]
  desc_right_col = stringr::str_subset(names(df), "Real Time")
  desc_col = c(desc_left_col, desc_right_col)
  
  # Pivot table and Split measurement columns
  dfl = df %>% 
    dplyr::select(-starts_with(".")) %>% 
    tidyr::pivot_longer(cols = -any_of(desc_col)) %>% 
    tidyr::drop_na(value) %>% 
    tidyr::separate(name, into = c("element", "variable"),
                    sep = " ", extra = "merge")
  ## Append LE and other custom compounds that may exist in the table as element
  element_symbol = unique(c(element_symbol, unique(dfl$element) ))
  

  # Convert elements into ordered factors sorted by atomic number
  dfl = dfl %>% 
    dplyr::mutate(lod = if_else(value == "<LOD", 1, 0),
                  value = ifelse(value == "<LOD", lod_value, value),
                  value = as.numeric(value),
                  element = factor(element, levels = element_symbol))
  
  
  ## Wide
  
  if(is.character(subset_file)){
    ## Read CSV all columns as character
    element_subset = read_csv(file = subset_file, 
                            show_col_types = FALSE,
                            col_select = 1) %>% 
      dplyr::pull(var = 1) %>% 
      forcats::as_factor() ## Creates factor in the original order
    ##
    
    dfw = dfl %>% 
      dplyr::filter(as.character(element) %in% as.character(element_subset) ) %>%
      dplyr::mutate(element = factor(element, 
                                     levels = levels(element_subset)) ) %>% 
      dplyr::arrange(Date, Time, element) %>% 
      dplyr::ungroup()
    
  }else {
    dfw = dfl %>% 
      #dplyr::arrange(Date, Time, element) %>% 
      dplyr::ungroup()
  }
  
  dfw_c = dfw %>% 
    dplyr::filter(variable == "Concentration") %>%
    dplyr::select(-c(variable, lod)) %>%
    {if(drop_na_concentration){
      tidyr::drop_na(., value)
    }else {
      .
    }} %>% 
    tidyr::pivot_wider(names_from = element, values_from = value)
  
  dfw_e = dfw %>% 
    dplyr::filter(variable %in% c("Concentration", "Error1s")) %>%
    dplyr::select(-c(lod)) %>%
    tidyr::pivot_wider(names_from = c(element, variable), 
                       values_from = value,
                       names_sep = " ")
  
  ## PREPARE EXCEL EXPPORT
    
    params.df = tibble::tibble(
      VARIABLES = c(#"Original File Name:", 
                   "Subset Elements?:",
                   "Subset Elements List:",
                   "Set <LOD Values To 0:",
                   "Drop Elements With NA Concentrations"
                   ),
      PARAMETERS = c(#basename(raw_csv_file),
                    as.character(is.character(subset_file)),
                    ifelse(is.character(subset_file), paste(element_subset, collapse = ", "), ""),
                    as.character(set_lod_zero),
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
    writeData(wb, "Tidy-Long-Format", dfl)
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
                   long = dfl, 
                   wide = dfw_c,
                   excel = wb) 
    return(df_list)
  }
  
  
}
