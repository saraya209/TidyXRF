server <- function(input, output, session) {
  
  ## Remove columns for display
  cols_remove <- c("Instrument Serial Num", "Method Name",
                  "User Factor Name", "Collimation Status",
                  "Latitude", "Longitude", "Units")
  
  # Reactive value for selected dataset ----
  uploaddatapath <- reactive({
    req(input$data_upload) # waits until upload
    
    ext <- tools::file_ext(input$data_upload$name)
    switch(ext,
           csv = input$data_upload$datapath,
           validate("Invalid file type. Please upload a .csv file")
    )
  })
  
  uploadsubsetpath <- reactive({
    input$subset_upload # waits until upload
    
    ext <- tools::file_ext(input$subset_upload$name)
    switch(ext,
           csv = input$subset_upload$datapath,
           validate("Invalid file type. Please upload a .csv file")
    )
  })
  # Run Button ----
  dataset <- eventReactive(input$run_tidy,{
   # req(input$run_tidy)
    if(input$subset_element){
      subset_file = uploadsubsetpath()
    }else{
      subset_file = NULL
    }
    
    
    tidy_xrf(raw_csv_file = uploaddatapath(),
             subset_file = subset_file,
             handle_lod =  as.numeric(input$lod),
             drop_na_concentration = input$remove_na,
             save_excel = FALSE,
             doreturn = TRUE,
             compare_guide = input$guide)
    
  })
  #output$text <- renderText({ input$std })
  
  # Reactive value for Tidy dataset ----
  
  # Table of raw dataset ----
  output$rawtable <- DT::renderDataTable({
    #
    DT::datatable(dataset()$raw%>% 
                    dplyr::select(-any_of(cols_remove) ),
                  rownames = FALSE,
                  options = list(scrollX = TRUE))
    
    
  })
  # Table of long dataset ----
  output$longtable <- DT::renderDataTable({#
    DT::datatable(
      dataset()$long %>% 
        dplyr::select(-any_of(cols_remove) ),
      rownames = FALSE,
      options = list(scrollX = TRUE)
    )
    
    
  })
  # Table of wide dataset ----
  output$widetable <- DT::renderDataTable({#
    DT::datatable(
      dataset()$wide%>% 
        dplyr::select(-any_of(cols_remove) ),
      rownames = FALSE,
      options = list(scrollX = TRUE)
    )
    
    
  })
  
  # Downloadable csv of selected dataset ----
  output$download_excel <- downloadHandler(
    filename = function() {
      "Tidy_data.xlsx"
    },
    content = function(file) {
    #   my_workbook <- createWorkbook()
    #   
    #   addWorksheet(
    #     wb = my_workbook,
    #     sheetName = "Wide Data"
    #   )
    #   
    #   
    #   writeData(
    #     my_workbook,
    #     sheet = 1,
    #     dataset()$wide
    #   )
    #   
    #   addWorksheet(
    #     wb = my_workbook,
    #     sheetName = "Long Data"
    #   )
    #   writeData(
    #     my_workbook,
    #     sheet = 2,
    #     dataset()$long
    #   )
      
     
      saveWorkbook(dataset()$excel, file)
    }
  )
  
}