

library(shiny)


library(datasets)
# Define UI for application that draws a histogram
ui <- fluidPage(    
    
    # Give the page a title
    titlePanel("Using ICTD to predict the cell type PROPORTION and data specific cell type MARKER GENES"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with one input
        sidebarPanel(
            selectInput("dataType", "RNA-seq or Microarray:", 
                        choices=c("RNA-seq","Microarray")),
            
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            helpText("Leave empty will run ICTD default data set."),
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


server <- function(input, output) {
    
    # Return the requested dataset ----
    # Note that we use eventReactive() here, which depends on
    # input$run_ictd_flag (the action button), so that the output is only
    # updated when the user clicks the button
    # ictd_result <- eventReactive(input$run_ictd_flag, {
    #    # ICTD(input$file1),
    #     runif(5)
    # }, ignoreNULL = FALSE)
    # output$plot <- renderPlot({
    #     hist(ictd_result())
    # })
    
    output$contents <- renderTable({

        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(input$file1)

        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        rownames(df) <- df[, 1]
        df <- df[, -1]
        data.matrix <- df
        ictd_result <- ICTD(data.matrix)
        prop <- ictd_result[[1]]
        data_col_sub <- prop[,1:5]

    }, rownames = T)
}

#-----------------function part----------

# ICTD <- function(myinput)
# {
#     return(myinput + 1)
# 
# }

library("ICTD")

#install dependent pkg
# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("impute")
# BiocManager::install("GO.db")
# BiocManager::install("sva")
# BiocManager::install("preprocessCore")
# library("impute")
# library("GO.db")
# library("sva")
# library("preprocessCore")


# rforge <- "http://r-forge.r-project.org"
# install.packages("estimate", repos=rforge, dependencies=TRUE)
# 
# #install ICTD
# install.packages("devtools")
# devtools::install_github("zy26/ICTD")


#----------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
