

library(shiny)


library(datasets)
# Define UI for application that draws a histogram
ui <- navbarPage("ICTD",
        theme = shinythemes::shinytheme("cerulean"),
        tabPanel("Do deconvolute",
            fluidPage(    
            
            # Give the page a title
            titlePanel("Using ICTD to predict the cell type PROPORTION and data specific cell type MARKER GENES"),
            
            # Generate a row with a sidebar
                sidebarLayout(      
                    # Define the sidebar with one input
                    sidebarPanel(
                        #Input: Select a file ----
                        fileInput("file1", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        p('(Maximum allowed 10 MB)'),
                        helpText("Leave empty will run ICTD with default data set."),
                        downloadLink('downloadData', 'Download example data'),
                        hr(),
                        selectInput("dataType", "Tissue type:(coming soon)",
                                    choices=c("Human normal","Human cancer","Human inflammatory","Human brain","Human blood",
                                              "Mouse inflammatory","Mouse cancer")),
                        hr(),
                        selectInput("resu", "Resulution:(coming soon)",
                                    choices=c("Low resolution (canonical cell type marker)","High resolution (lineage relationship)")),
                        hr(),
                        actionButton("run_ictd_flag", "Run ICTD")
                    ),
                    
                    # Create a spot for the barplot
                    mainPanel(
                        #tags$img("mainImage", height=5),
                      tags$ul(
                        tags$li("Maximizing the resolution in identifying cell and sub types that truly exists in a tissue"),
                        tags$li("Identifying the most reliable marker genes for each cell type, which are tissue microenvironment and data set specific"),
                        tags$li("Handling the highly co-presented cell types")
                      ),
                      tags$img(src="images/mainpage1.png", 
                               height=400),

                        tabsetPanel(
                            tabPanel('Cell type proportion', 
                                     downloadLink('downloadResult', 'Download predicted cell type proportion.'),
                                     helpText('(click download result before RUN ICTD)'),
                                     tableOutput("contents")
                            ),
                            tabPanel('Cell type marker genes', 
                                     downloadLink('downloadMarker', 'Download related cell type marker genes.'),
                                     helpText('(click download marker gene before RUN ICTD)'),
                                     tableOutput('marker')
                            )
                            # # Output: Header + table of distribution ----
                            # h4("Print Predicted Cell Proportion")                           tabPanel('Figure', tableOutput(mtcars)),
                            # tableOutput("contents")
                            # #plotOutput("plot")
                        )
                      )
                    
                )
            
            )
        ),
        tabPanel("About",
                 fluidPage(
                    h2("Pipeline of ICTD", style="color: STEELBLUE; font-size: 25px; margin: 0px"),
                    tags$img(src="images/Fig1.png",width=600),  
                    includeMarkdown("about.Rmd")
                 )   
                 
        ),
        tabPanel("Tutorial",
            fluidPage(
                includeMarkdown("instruction.Rmd")
            )
        ),
        tabPanel("FAQ",
                 fluidPage(
                     includeMarkdown("faq.Rmd")
                 )
        ),
        tabPanel("BDR Lab",
                 fluidPage(
                   includeMarkdown("bdr.Rmd")
                 )
        )
)

options(shiny.maxRequestSize = 10*1024^2)

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
    
    
}

#-----------------function part----------

justAdd1 <- function()
{
    print('test add 1')
    #return(myinput + 1)

}

library("ICTD")




#----------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
