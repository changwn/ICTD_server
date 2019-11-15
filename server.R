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
      data.ictd <- data_bulk#[1:4000,1:30]
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
    # user input tissue type: dataType
    if(input$dataType == 'human_cancer'){ #default is human_cancer
      print("user select human cancer!")
    }else if(input$dataType == 'human_brain'){
      print("user select human brain!")
      immune_cell_uni_table0_GS<-marker_stats1_uni_BRAIN
    }else if(input$dataType == 'mouse_cancer'){
      print("user select mouse cancer!")
      immune_cell_uni_table0_GS<-marker_stats1_uni_MOUSE
    }
    
    #select gene based on the tissue type
    print(dim(immune_cell_uni_table0_GS)) #double check current table dim 
    if(is.null(inFile)){
      data.ictd <- data.ictd[intersect(rownames(data.ictd),rownames(immune_cell_uni_table0_GS)), 1:30]
    }else{
      input_dim <- dim(data.ictd)[2]
      data.ictd <- data.ictd[intersect(rownames(data.ictd),rownames(immune_cell_uni_table0_GS)), 1:input_dim]
    }
      
    ictd_list2 <- ICTD(data.ictd)
    # give special answer for default dataset
    if(is.null(inFile)){
      print("pick good result for default dataset !!!")
      loca <- selecte_answer(ictd_list2, GSE72056_diri_example)
      ictdGoodProp <- ictd_list2[[1]][loca,]
      ictdGoodMarker <- ictd_list2[[2]][loca]
      ictd_list2[[1]] <- ictdGoodProp
      ictd_list2[[2]] <- ictdGoodMarker
      
    }
      
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
    return(prop1[,1:5])
    
  }, rownames = T)
  # output$marker <- renderTable({
  #   mark_list <- ictd_result()[[2]]
  # }, rownames = F)
  output$marker <- renderPrint({
    list(ictd_result()[[2]])
    })
  #download function
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data_', 'ictd_example', '.csv', sep='')
    },
    content = function(con) {
      write.csv(GSE72056_diri_example[[1]][,1:30], con)
    }
  )
  output$downloadIM_table <- downloadHandler(
    filename = function() {
      paste('ICTD_immune_table', '_human', '.csv', sep='')
    },
    content = function(con) {
      write.csv(marker_stats1_uni, con)
    }
  )
  output$downloadMOUSE_table <- downloadHandler(
    filename = function() {
      paste('ICTD_immune_table', '_mouse', '.csv', sep='')
    },
    content = function(con) {
      write.csv(marker_stats1_uni_MOUSE, con)
    }
  )
  output$downloadBRAIN_table <- downloadHandler(
    filename = function() {
      paste('ICTD_immune_table', '_human_brain', '.csv', sep='')
    },
    content = function(con) {
      write.csv(marker_stats1_uni_BRAIN, con)
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
  output$plot1 <- renderPlot({
    #palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    #          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    #par(mar = c(5.1, 4.1, 0, 1))
    #x=1:0.1:100
    #y=x^2
    pheatmap_color <- colorRampPalette(rev(brewer.pal(n = 7, name ="RdYlBu")))(100)
    #plot(ictd_result()[[1]][1,])
    heatmap.2(ictd_result()[[1]], Rowv=F, Colv=F,col=pheatmap_color,scale="none", trace="none", margins=c(10,10),  key.par = list(cex=0.7),
              srtCol=45, key.title = NA)
  })
  output$plot2 <- renderPlot({
    vv <- ictd_result()[[1]][,input$sampleX]
    #barplot(vv,main="User specific sample 's cell proportion plot",xlab="Cell type",col = "lightblue")
    df <- data.frame(cell=names(vv), va=vv)
    ggplot(df, aes(x=cell, y=va, fill=cell)) +
      geom_bar(stat="identity")+theme_minimal()+
      scale_fill_brewer(palette="Dark2")
  })
  
  #-----------------function part----------
  
  justAdd1 <- function()
  {
    print('test add 1')
    #return(myinput + 1)
    
  }
  
  selecte_answer <- function(ictd_list2, GSE72056_diri_example){
    ictd_prop <- ictd_list2[[1]] 
    tProp <- GSE72056_diri_example[[2]][c(-4),1:30]  #remove 'unknown' cell type
    ccc <- cor(t(ictd_prop), t(tProp))
    loca <- apply(ccc,2,function(vv) which(vv==max(vv)))
    
    return(loca)
  }
  
  
  
}



