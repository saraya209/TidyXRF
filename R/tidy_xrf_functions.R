# Function to Transform to Long Data
tidy_xrf <- function(
  # Input files
  raw_csv_file,
  elements_tbl = elements.tbl,
  stdref_tbl = stdref.tbl,
  # Optional tidy parameters
  subset_file = NULL,
  handle_lod = 0,
  drop_na_concentration = TRUE,
  compare_guide = "None",
  # function export
  save_excel = TRUE,
  doreturn = TRUE) {
  
  
  
  ## Tidyup XRF exported table
  ## Args:
  # raw_csv_file: Full path to raw csv file
  # subset_file: csv list of elements to supset for wide table. If NULL Elements are not subset.
  # handle_lod: one of c("0", "NA", "0.5"). "0": <LOD -> 0, "NA": <LOD -> NA, "0,5" -> Half of LOD 
  # save_excel_file: Full path to save tidy table.
  ## Returns
  ## Transformed tidy table
  #
  require("tidyverse")
  require("openxlsx")
  ## Read CSV all columns as character
  ## fread() jumps empty row at the beganning.
  df = data.table::fread(input = raw_csv_file,
                         colClasses = "character",
                         quote = "")
  
  ## Element Symbol Sorted by Atomic Number
  element_symbol = elements_tbl %>%
    dplyr::arrange(Atomic_Number) %>%
    dplyr::pull(Symbol)
  ## List of possible variables appended to element or custom compounds
  variable_list = c("Compound",
                    "Compound Level",
                    "Compound Error",
                    "Concentration",
                    "Error1s")
  
  # pivot all columns that start with "element symbol + a space" OR
  #   contain one of the variables, this is so custom created compounds are included.
  # Separate Analyte and Variable,
  # Append elements table
  # Format column types
  
  # Pivot to Long ----
  dfl = df %>%
    tidyr::pivot_longer(cols = c(starts_with(paste0(element_symbol, " ")), contains(variable_list)),
                        values_to = "Raw_Value") %>%
    tidyr::drop_na(Raw_Value) %>%
    tidyr::separate(
      name,
      into = c("Analyte", "Variable"),
      sep = " ",
      extra = "merge"
    ) %>%
    dplyr::left_join(elements_tbl, by = c("Analyte" = "Symbol")) %>%
    dplyr::mutate(
      Value = ifelse(Raw_Value == "<LOD", handle_lod * LOD , as.numeric(Raw_Value)),
      Analyte = forcats::as_factor(Analyte)
    )
  
  ## create a list of all analytes in table
  analyte_list = unique(unique(dfl$Analyte))
  
  
  # Pivot to Wide ----
  
  ## Subroutine: Subset ----
  
  if (is.character(subset_file)) {
    # Import comma separated symbols
    element_subset = str_split(subset_file, pattern = ",", simplify = T) %>%
      forcats::as_factor() ## Creates factor in the original order
    ##
    dfw = dfl %>%
      dplyr::filter(as.character(Analyte) %in% as.character(element_subset)) %>%
      dplyr::mutate(Analyte = factor(Analyte,
                                     levels = levels(element_subset))) %>%
      dplyr::arrange(Date, Time, Analyte) %>%
      dplyr::ungroup()
    
  } else {
    dfw = dfl %>%
      dplyr::ungroup()
  }
  
  # Concentration only
  dfw_c = dfw %>%
    dplyr::filter(Variable == "Concentration") %>%
    dplyr::select(-any_of(c(
      names(elements_tbl), "Variable", "Raw_Value", "LOD"
    ))) %>%
    {
      if (drop_na_concentration) {
        tidyr::drop_na(., Value)
      } else {
        .
      }
    } %>%
    tidyr::pivot_wider(names_from = Analyte, values_from = Value)
  
  # Concentration and errors
  dfw_e = dfw %>%
    dplyr::filter(Variable %in% c("Concentration", "Error1s")) %>%
    dplyr::select(-any_of(c(names(elements_tbl), "Raw_Value", "LOD"))) %>%
    tidyr::pivot_wider(
      names_from = c(Analyte, Variable),
      values_from = Value,
      names_sep = " "
    )
  
  ## Subroutine: Compare with Guide ----
  
  if (compare_guide != "None") {
    dfw_g <- dfl %>%
      dplyr::filter(!is.na(!!sym(compare_guide)) &
                      Variable == "Concentration") %>%
      dplyr::mutate(
        val_pct_above = paste0(" (",
                               round(100 * ((Value-!!sym(compare_guide)) / !!sym(compare_guide)
                               ),
                               2), "%)"),
        val_pct_above = ifelse(
          Value > !!sym(compare_guide),
          paste0(Value, val_pct_above),
          Value
        ),
        Variable = "PPM (% Above Guideline)"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-any_of(c(
        names(elements_tbl), "Raw_Value", "Value", "LOD"
      ))) %>%
      tidyr::pivot_wider(names_from = Analyte,
                         values_from = val_pct_above)
    
    select_lod = elements_tbl %>%
      dplyr::select(all_of(c("Name", "Symbol", compare_guide))) %>%
      tidyr::drop_na()
    
    select_standard = stdref_tbl %>%
      dplyr::filter(Standard == {
        {
          compare_guide
        }
      })
    
    # Append guides to concentration with guides
    # Add standards
    std_names = stdref_tbl %>%
      dplyr::pull(Standard)
    
    # create guides table
    guide_wide = elements_tbl %>%
      dplyr::select(all_of(c("Symbol", std_names))) %>%
      tidyr::pivot_longer(-Symbol) %>%
      tidyr::pivot_wider(names_from = Symbol, values_from = value) %>%
      dplyr::select(any_of(c("name", names(dfw_g)))) %>%
      #janitor::remove_empty("cols") %>%
      mutate(across(everything(), as.character))
    
    # Make guides table list of elements align with data
    guide_fmt_wide = dfw_g %>%
      dplyr::bind_rows(guide_wide) %>%
      dplyr::filter(!is.na(name)) %>%
      dplyr::mutate(`Instrument Serial Num` = name) %>%
      dplyr::select(-name)
    
    
    select_standard_row = which(guide_fmt_wide$`Instrument Serial Num` == select_standard$Standard)
  }
  
  # Prepare Excel ----
  
  params.df = tibble::tibble(
    VARIABLES = c(
      #"Original File Name:",
      "Set <LOD Values To:",
      "Drop Elements With NA Concentrations",
      "Subset Elements:",
      "Subset Elements List:"
      
    ),
    PARAMETERS = c(
      #basename(raw_csv_file),
      ifelse(
        is.na(handle_lod),
        paste(handle_lod),
        ifelse(handle_lod == 0,
               paste(handle_lod),
               paste("LOD *", handle_lod))
      ),
      as.character(drop_na_concentration),
      paste(is.character(subset_file)),
      ifelse(
        is.character(subset_file),
        paste(element_subset, collapse = ", "),
        ""
      )
      
    )
  )
  
  
  ## create a workbook and add a worksheet
  wb <- createWorkbook()
  addWorksheet(wb, "Concentrations")
  addWorksheet(wb, "Concentrations-and-Error")
  
  if (compare_guide != "None") {
    addWorksheet(wb, "Concentrations-v-Guidelines")
    addWorksheet(wb, "Guidelines-Description")
  }
  addWorksheet(wb, "Tidy-Long-Format")
  addWorksheet(wb, "Original-Raw")
  addWorksheet(wb, "Tidying-Parameters")
  
  
  # formatting cells
  myHeader <-
    createStyle(textDecoration = "bold", border = "bottom")
  negStyle <-
    createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  selectStyle <-
    createStyle(fontColour = "#9C0006", textDecoration = "bold")
  guideStyle <-
    createStyle(border = "TopBottom",
                fontColour = "#4F81BD",
                borderStyle = "thick")
  
  # Add data to workbook
  
  
  writeData(wb, "Concentrations",
            x = dfw_c,
            headerStyle = myHeader)
  writeData(wb, "Concentrations-and-Error", dfw_e, headerStyle = myHeader)
  writeData(wb,
            "Tidy-Long-Format",
            dfl %>% dplyr::select(-any_of(names(elements_tbl))),
            headerStyle = myHeader)
  writeData(wb, "Original-Raw", df)
  writeData(wb, "Tidying-Parameters", params.df)
  
  
  
  if (compare_guide != "None") {
    writeData(wb, "Concentrations-v-Guidelines",
              dfw_g,
              headerStyle = myHeader)
    
    #Add Guideline rows
    guide_row_start = nrow(dfw_g) + 1
    guide_row_end = guide_row_start + nrow(guide_fmt_wide) - 1
    highlight_row = guide_row_start + select_standard_row - 1
    col_start = which(names(dfw_g) == "Variable") + 1
    col_end = ncol(dfw_g)
    
    
    writeData(
      wb,
      "Concentrations-v-Guidelines",
      colNames = FALSE,
      startRow = guide_row_start,
      x = guide_fmt_wide
    )
    
    #Format guide table
    addStyle(
      wb,
      sheet = "Concentrations-v-Guidelines",
      style =  guideStyle,
      rows = guide_row_start:guide_row_end,
      cols = 1:col_end,
      gridExpand = TRUE
    )
    
    #format selected guide row
    addStyle(
      wb,
      sheet = "Concentrations-v-Guidelines",
      style =  selectStyle,
      rows = highlight_row,
      cols = 1:col_end,
      gridExpand = TRUE
    )
    
    #Format Concentrations above
    
    conditionalFormatting(
      wb,
      "Concentrations-v-Guidelines",
      cols = col_start:col_end,
      rows = 2:guide_row_start - 1,
      type = "contains",
      rule = "%",
      style = negStyle
    )
    
    writeData(wb, "Guidelines-Description", stdref.tbl)
    
    
  }
  
  
  
  if (save_excel) {
    save_excel_file = file.path(dirname(raw_csv_file),
                                paste0("Tidy_",
                                       strsplit(basename(raw_csv_file), split =
                                                  "\\.")[[1]][1],
                                       ".xlsx"))
    
    saveWorkbook(wb, file = save_excel_file, overwrite = TRUE)
  }
  
  if (doreturn) {
    df_list = list(
      raw = df,
      long = dfl %>% dplyr::select(-any_of(names(elements_tbl))),
      wide = dfw_c,
      excel = wb
    )
    return(df_list)
  }
  
  
}
