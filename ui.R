library(shiny)
library(markdown)
library(shinythemes)
library(datasets)
library(shinybusy)
library(gplots)
library("RColorBrewer")


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
                                p('(Maximum allowed 50 MB)'),
                                helpText("Input CSV file is supposed to have row (gene) names and column (sample) names. Leave empty will run ICTD with default data set."),
                                downloadLink('downloadData', 'Download example data'),
                                hr(),
                                selectInput("dataType", "Tissue type:",
                                            #choices=c("Human cancer","Human brain","Mouse cancer"),
                                            choices=list(
                                              Human=c('Human Cancer'='human_cancer', 'Human Brain'='human_brain'),
                                              Mouse=c('Mouse Cancer'='mouse_cancer')
                                            ), selectize = FALSE),
                                hr(),
                                p('(Download the ICTD trained cell type marker gene table for above three tissue type.)'),
                                hr(),
                                downloadLink('downloadIM_table', 'Cell type marker gene table for HUMAN CANCER.'),
                                hr(),
                                downloadLink('downloadBRAIN_table', 'Cell type marker gene table for HUMAN BRAIN'),
                                hr(),
                                downloadLink('downloadMOUSE_table', 'Cell type marker gene table for MOUSE CANCER'),
                                hr(),
                                selectInput("reso", "Resolution:(coming soon)",
                                            choices=c("Low resolution (canonical cell type marker)","High resolution (lineage relationship)")),
                                hr(),
                                actionButton("run_ictd_flag", "Run ICTD"),
                                hr(),
                                numericInput(inputId = "sampleX",label = "Choose sample ID for visulization",value = 3)
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
                                         width="100%"),
                                
                                tabsetPanel(
                                  tabPanel('Cell type proportion', 
                                           downloadLink('downloadResult', 'Download predicted cell type proportion.'),
                                           helpText('(click download result before RUN ICTD)'),
                                           #add_busy_spinner(spin = "cube-grid", position = 'full-page'),
                                           add_busy_gif(src = "images/busy13.gif", height = 70, width = 70, position = 'full-page'),
                                           tableOutput("contents"),
                                           plotOutput('plot1'),
                                           plotOutput('plot2')
                                  ),
                                  tabPanel('Cell type marker genes', 
                                           downloadLink('downloadMarker', 'Download related cell type marker genes.'),
                                           helpText('(click download marker gene before RUN ICTD)'),
                                           #tableOutput('marker')
                                           verbatimTextOutput('marker')
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
                            tags$img(src="images/Fig1_new.png",width='100%'),
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