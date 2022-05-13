
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
                  ".csv") ),
      
      wellPanel(
        strong("Optional Tidying Parameters:"),
        # helpText("Will not affect the",
        #          "Tidy-Long-Format export."),
        
        checkboxInput("lod", "Set <LOD = 0", FALSE),
        checkboxInput("remove_na", "Drop elements with missing concentration", TRUE),
        wellPanel(
        checkboxInput("subset_element", "Subset elements", FALSE),
        fileInput(
          inputId = 'subset_upload',
          label = "Upload Elements List",
          multiple = FALSE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        )
        )
        
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
          em("Olympus Vanta C-Series Handheld XRF Spectrometer"),
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
          span("Optional Tidying Parameters", style = "color:grey; font-weight:bold"),
          "allow additional controls on how the",
          em("wide format"),
          "tables are processed. Uploading a",
          strong("single column list of element symbols"),
          "in .csv format is required should you choose to subset the",
          "tables to fewer elements of interest using",
          span("Subset elements", style = "color:grey; font-weight:bold"),
          "option."
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
        )
      )
      
    ))
)
)