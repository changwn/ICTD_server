

library(shiny)


library(datasets)
# Define UI for application that draws a histogram
ui <- navbarPage("ICTD",
        tabPanel("Do deconvolute",
            fluidPage(    
            
            # Give the page a title
            titlePanel("Using ICTD to predict the cell type PROPORTION and data specific cell type MARKER GENES"),
            
            # Generate a row with a sidebar
                sidebarLayout(      
                    
                    # Define the sidebar with one input
                    sidebarPanel(
                        # selectInput("dataType", "RNA-seq or Microarray:", 
                        #             choices=c("RNA-seq","Microarray")),
                        
                        
                        # Input: Select a file ----
                        fileInput("file1", "Choose CSV File",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        
                        helpText("Leave empty will run ICTD default data set from GSE72056."),
                        hr(),
                        
                        actionButton("run_ictd_flag", "Run ICTD")
                    ),
                    
                    # Create a spot for the barplot
                    mainPanel(
                        # Output: Header + table of distribution ----
                        h4("Print Predicted Cell Proportion"),
                        tableOutput("contents")
                        #plotOutput("plot")
                    )
                    
                )
            
            )
        ),
        tabPanel("Instruction",
            fluidPage(
                h3("Instruction of ICTD :)"),
                includeMarkdown("about.Rmd")
            )
        ),
        tabPanel("About",
             fluidPage(
                 p("The novel deconvolution method, namely Inference of Cell Types and Deconvolution (ICTD) ")
                
            )   
            
        )
)

options(shiny.maxRequestSize = 5*1024^2)

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
            data.ictd <- data_bulk[,1:6]
        }
 
        ictd_result <- ICTD(data.ictd)
        prop <- ictd_result[[1]]
        data_col_sub <- prop[,1:5]
    })
    
    output$contents <- renderTable({
        
        ictd_result()

    }, rownames = T)
}

#-----------------function part----------

# ICTD <- function(myinput)
# {
#     return(myinput + 1)
# 
# }

library("ICTD")




#----------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
