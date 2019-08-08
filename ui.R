library(shiny)
library(markdown)
library(shinythemes)
library(datasets)
# Define UI for application that draws a histogram
navbarPage("ICTD",
                 theme = shinytheme("cerulean"),
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
                                helpText("Input CSV file is supposed to have row (gene) names and column (sample) names. Leave empty will run ICTD with default data set."),
                                downloadLink('downloadData', 'Download example data'),
                                hr(),
                                selectInput("dataType", "Tissue type:(coming soon)",
                                            choices=c("Human normal","Human cancer","Human inflammatory","Human brain","Human blood",
                                                      "Mouse inflammatory","Mouse cancer")),
                                hr(),
                                selectInput("reso", "Resolution:(coming soon)",
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