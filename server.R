options(shiny.maxRequestSize = 10*1024^2)
library("ICTD")

server <- function(input, output) {
  
  #suppress all the warning message
  options(warn = -1)
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$run_ictd_flag (the action button), so that the output is only
  # updated when the user clicks the button
  ictd_result <- eventReactive(input$run_ictd_flag, {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    inFile <- input$file1
    print('hi')
    
    #if user input data
    if(!is.null(inFile)){   
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(inFile$datapath)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      rownames(df) <- df[, 1]
      df <- df[, -1]
      data.ictd <- df
    }else{  #with default data
      data_bulk = GSE72056_diri_example[[1]]
      data.ictd <- data_bulk[1:4000,1:30]
    }
    
    style <- isolate(input$style)
    withProgress(message = 'Running ICTD...Please wait...', style = style, value = 0.1, {
      Sys.sleep(0.25)
      # Increment the top-level progress indicator
      for(i in 1:10){
        incProgress(0.5)
        Sys.sleep(0.05)
      }
      
    })
    ictd_list2 <- ICTD(data.ictd)
    #prop <- ictd_list2[[1]]
    #data_col_sub <- prop[,1:5]
    return(ictd_list2)
  })
  
  style <- isolate(input$style)
  withProgress(message = 'Running ICTD...', style = style, value = 0.1, {
    Sys.sleep(0.25)
    # Increment the top-level progress indicator
    incProgress(0.5)
  })
  
  output$contents <- renderTable({
    
    prop1 <- ictd_result()[[1]]
    return(prop1[,1:6])
    
  }, rownames = T)
  output$marker <- renderTable({
    mark_list <- ictd_result()[[2]]
  }, rownames = T)
  #download function
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data_', 'ictd_example', '.csv', sep='')
    },
    content = function(con) {
      write.csv(GSE72056_diri_example[[1]][,1:30], con)
    }
  )
  output$downloadResult <- downloadHandler(
    filename = function() {
      paste('ictd_', 'cell_proportion', '.csv', sep='')
    },
    content = function(con) {
      write.csv(ictd_result()[[1]], con)
    }
  )
  output$downloadMarker <- downloadHandler(
    filename = function() {
      paste('ictd_', 'marker_gene', '.csv', sep='')
    },
    content = function(con) {
      write.csv(ictd_result()[[2]], con)
    }
  )
  output$mainImage <-  renderImage({
    
    return(list(
      src = "images/mainpage.png",
      contentType = "image/png",
      width='700',
      alt = "test" ,         
      class="center"
    ))
    
    
  }, deleteFile = FALSE)
  
  #-----------------function part----------
  
  justAdd1 <- function()
  {
    print('test add 1')
    #return(myinput + 1)
    
  }
  
  
  
}



