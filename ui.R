
ui <- fluidPage(
  # App title ----
  titlePanel("Cleanup XRF Spectrometer Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Browse dataset ----
      fileInput(inputId = 'data_upload',
                label = "Upload XRF File", 
                multiple=FALSE,
                accept=c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".txt") ),
      
      wellPanel(
        helpText("Tidying Parameters"),
        # helpText("Will not affect the",
        #          "Tidy-Long-Format export."),
        prettyRadioButtons(
          inputId = "lod",
          label = "Set `<LOD` as:",
          choices = c(
            "0" = 0,
            "NA" = NA_integer_,
            "LOD x 0.5" = 0.5
          ),
          inline = TRUE,
        ),
        strong("Missing concentration:"),
        prettyCheckbox(
          inputId = "remove_na",
          label = "Drop element",
          value = TRUE
        ),
        #        wellPanel(
        strong("Subset elements:"),
        prettyCheckbox(inputId = "subset_element", 
                       label = "Yes", 
                       value = FALSE),
       #"Elements to subset:",
       tags$style("#subset_list {font-size:12px;}"),
        textInput("subset_list", 
                  label = NULL, 
                  value = "Pb,Cr,As,Zn,Cd,Cu,Hg,Ni,Mo,Se")
        # fileInput(
        #   inputId = 'subset_upload',
        #   label = NULL,
        #   multiple = FALSE,
        #   accept = c(
        #     "text/csv",
        #     "text/comma-separated-values,text/plain",
        #     ".csv",
        #     ".txt"
        #   )
        # )
        #        )
        
      ), 
      wellPanel(
        helpText("Compare Values with Guidelines"),
        #checkboxInput("guide", "Compare with Guidelines", FALSE),
        pickerInput(
          inputId = "std",
          label = NULL,
          choices = c("None", stdref.tbl$Standard)
        ),
        textOutput("text")
      ), 
      
      
      actionButton("run_tidy", "Run Tidy Function"),
      br(),
      br(),
      downloadButton(
        "download_excel", 
        "Download Data to Excel"
      ),
      helpText("Note: Click the Help tab for information.")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(tabsetPanel(
      id = 'dataset',
      tabPanel("Raw Table", DT::dataTableOutput("rawtable")),
      tabPanel("Tdiy Long Table", DT::dataTableOutput("longtable")),
      tabPanel("Tidy Wide Table", DT::dataTableOutput("widetable")),
      tabPanel(
        title = "Help",
        h2("About"),
        p(
          "This app imports `.csv` files exported from",
          em("Olympus Vanta C-Series Handheld X-Ray Fluorescence Spectrometer"),
          "and produce a tidied-up tables in multiple formats."
        ),
        br(),
        h2("How To Use App"),
        p(
          "- Upload a .csv file exported from the XRF instrument",
          "and click",
          span("Run Tidy Function.", style = "color:grey; font-weight:bold")
        ),
        p(
          "- Use the",
          span("Raw Table", style = "color:grey; font-weight:bold"),
          "tab to view the uploaded file, the",
          span("Tidy Long Table", style = "color:grey; font-weight:bold"),
          "and",
          span("Tidy Wide Table", style = "color:grey; font-weight:bold"),
          "tabs display the tidied table in long and wide formats."
        ),
        p(
          "- The",
          span("Tidying Parameters", style = "color:grey; font-weight:bold"),
          "allow additional controls:",
          "how to deal with `<LOD` values, whether or not `NA` values should be droped from the",
          em("wide format"),
          "tables, and whether or not only a subset of elements should appear in the ",
          em("wide format"),
          "tables. To use the",
          span("Subset elements", style = "color:grey; font-weight:bold"),
          "function, you must supply a",
          strong("list of element symbols separated by a comma"),
          "(e.g., 'Pb,Zn,Hg,As')."
        ),
        p(
          "- To create additoinal Excel sheets that compare sample concentration values ",
          " versus maximum concentration guidelines, select a guideline from the",
          span("drop-down list.", style = "color:grey; font-weight:bold")
        ),
        p(
          "- Click",
          span("Download Data to Excel", style = "color:grey; font-weight:bold"),
          "button to save tidied tables on your computer."
        ),
        br(),
        h2("Description of Excel File"),
        p("The exported excel file will contain 5 sheets."),
        p(
          "-",
          em("Concentrations:"),
          "List of samples with element concentrations."
        ),
        p(
          "-",
          em("Concentrations-and-Error:"),
          "List of samples with element concentrations and 1-standard deviation error (Error1s)."
        ),
        p(
          "-",
          em("Tidy-Long-Format:"),
          "The full data pivoted to long format. This is the master tidied data."
        ),
        p("-", em("Original-Raw:"), "The original uploaded data."),
        p(
          "-",
          em("Tidying-Parameters:"),
          "List of parameters used to tidy the data."
        ),
        p(
          "-",
          em("Concentrations-v-Guidelines:"),
          "Wide-format table showing percent-above or percent-below the guideline maximum values.",
          "Negative values are percent below guideline maximum and positive values are percent above guideline maximum.",
          "Cells with positive values (concentrations that exceed the guideline) are highlighted red."
        ),
        p(
          "-",
          em("Guidelines-Used:"),
          "Description of the selected guideline and the guideline maximum values."
        ),
        br(),
        br(),
        p("Author:", 
          span("Samuel.Araya@usda.gov", style = "color:blue; font-weight:bold"))
      )
      
    ))
)
)