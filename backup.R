#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(htmltools)
library(readxl)
library(tidyverse)



text_images <- read_xlsx("datasets/TextileImageDataSet.xlsx")
text_materials <- read_xlsx("datasets/TextileMaterialDataSet.xlsx")
text_samples <- read_xlsx("datasets/image_samples.xlsx")
text_paintings <- read_xlsx("datasets/painting_samples.xlsx")



jsCode <- "shinyjs.appendToImgContainer = function(element){
var e  = document.getElementById('img-container');
e.appendChild(element);
}"


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tagList(
    tags$head(
      useShinyjs(),
      includeCSS("www/css/main.css"),
      includeCSS("www/css/lightbox.css"),
      #@Xiongbing Jin on https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
      
      tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')
    ),
    # dashboardPage(
    #     skin = "green",
    #     dashboardHeader(title = "MYAPP"),
    #     dashboardSidebar(sidebarMenu(
    #         id = "tabs",
    #         menuItem(
    #             "PICTURES & IMAGES",
    #             tabName = "imag",
    #             icon = shiny::icon("angle-double-right")
    #         )
    #     )),
    # dashboardBody(tabItems(tabItem(
    #     tabName = "image", h3("PICTURES & IMAGES"),
    #     fluidRow(uiOutput("picss"))
    # )))
    
    
    # ),
    includeScript("www/js/lightbox.js")
  ),
  fluidRow(
    
    uiOutput("image"),
    p("here")
    #uiOutput("imageBox")
    
  )
  
)







server <- function(input, output) {
  reactive_image_data <- reactive({
    ## isolate everything except input$Dataset
    
    if (input$DataSet == "Samples") {
      chosen_data <- text_samples
    }
    else {
      chosen_data <- text_paintings
    }
    
  })
  
  
  
  # #making Primary Color Choices
  # output$PrimaryColor <- renderUI({
  #     primary_colors <-
  #         unique(as.vector(reactive_image_data()$textile_color_visual_primary))
  #     selectizeInput(
  #         inputId = "PrimaryColor",
  #         label = "Choose a Primary Color",
  #         choices = primary_colors,
  #         #levels(factor(chosen_data01$textile_color_visual_primary)),
  #         multiple = TRUE
  #     )
  # })
  # 
  # 
  # #making Secondary Color Choices
  # output$SecondaryColor <- renderUI({
  #     secondary_colors <-
  #         unique(as.vector(
  #             reactive_image_data()$textile_color_visual_secondary
  #         ))
  #     selectizeInput(
  #         inputId = "SecondaryColor",
  #         label = "Choose a Secondary Color",
  #         choices = secondary_colors,
  #         #levels(factor(reactive_image_data()$textile_color_visual_secondary)),
  #         multiple = TRUE
  #     )
  # })
  # 
  # 
  # #making Pattern Choices
  # output$Pattern <- renderUI({
  #     patterns <-
  #         unique(as.vector(reactive_image_data()$textile_pattern_visual))
  #     selectizeInput(
  #         inputId = "Pattern",
  #         label = "Choose a Pattern",
  #         choices = patterns,
  #         multiple = TRUE
  #     )
  # })
  # 
  # #making Process Choices
  # output$Process <- renderUI({
  #     process <-
  #         unique(as.vector(reactive_image_data()$textile_process_visual))
  #     selectizeInput(
  #         inputId = "Process",
  #         label = "Choose a Process",
  #         choices = process,
  #         multiple = TRUE
  #     )
  # })
  
  # #filtering by primary color
  # chosen_data1 <- reactive({
  #     if (!is.null(input$PrimaryColor))
  #     {
  #         reactive_image_data() %>%
  #             filter(textile_color_visual_primary %in% input$PrimaryColor)
  #     }
  #     else{
  #         reactive_image_data()
  #     }
  # })
  # 
  # 
  # #filtering by secondary color
  # chosen_data2 <- reactive({
  #     if (!is.null(input$SecondaryColor))
  #     {
  #         chosen_data1() %>%
  #             filter(textile_color_visual_secondary %in% input$SecondaryColor)
  #     }
  #     else{
  #         chosen_data1()
  #     }
  # })
  # 
  # 
  # #filtering by pattern
  # chosen_data3 <- reactive({
  #     if (!is.null(input$Pattern))
  #     {
  #         chosen_data2() %>%
  #             filter(textile_pattern_visual %in% input$Pattern)
  #     }
  #     else{
  #         chosen_data2()
  #     }
  # })
  # 
  # #filtering by process
  # chosen_data4 <- reactive({
  #     if (!is.null(input$Process))
  #     {
  #         chosen_data3() %>%
  #             filter(textile_process_visual %in% input$Process)
  #     }
  #     else{
  #         chosen_data3()
  #     }
  # })
  
  
  imageData <- reactive(chosen_data())
  
  
  
  # output$image <- renderUI({
  #     box(width = NULL, title = HTML(paste0('<div class="image-wrap"><a href="/img/8kYkD3n.jpg"',
  #                                           '" data-lightbox="image-1" data-title="My caption"><img border="0" alt="" class="fixed-height" src="/img/8kYkD3n.jpg"',
  #                                           '"></a></div>')))
  #     
  #     
  #     
  # })
  
  
  ##WORKS!
  # output$image <- renderUI({
  #     
  #     #HTML('<img src="img/8kYkD3n.jpg">')
  #     #HTML('<img src="8kYkD3n.jpg">')
  #     #HTML('<img src="8kYkD3n.jpg">')
  #     
  #     
  #     #HTML('<img src="img/8kYkD3n.jpg">')
  #     
  #     fluidRow(column(width = 4,
  #                     id = "columns",
  #                     lapply(text_paintings$filename, function(i){
  #                         HTML(paste0('<img src="',text_paintings$filename,'">')) #box(width = NULL, title = 
  #                         
  #                         
  #                         
  #                     })
  #     ))
  #     
  #     
  # })
  
  
  
  
  # output$image <- renderUI({
  # 
  #     #HTML('<img src="img/8kYkD3n.jpg">')
  #     #HTML('<img src="8kYkD3n.jpg">')
  #     #HTML('<img src="8kYkD3n.jpg">')
  # 
  # 
  #     #HTML('<img src="img/8kYkD3n.jpg">')
  # 
  #     fluidRow(column(width = 4,
  #                     id = "columns",
  #                     lapply(text_paintings$filename, function(i){
  #                         HTML(paste0(
  #                             '<div class="image-wrap"><a href="',
  #                             text_paintings$filename,
  #                              '" data-lightbox="image-1" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #                             text_paintings$filename,
  #                              '"></a></div>'))
  # 
  # 
  # 
  #                     })
  #     ))
  # 
  # 
  # })
  
  
  
  # output$image <- renderUI({
  #     
  #     #HTML('<img src="img/8kYkD3n.jpg">')
  #     #HTML('<img src="8kYkD3n.jpg">')
  #     #HTML('<img src="8kYkD3n.jpg">')
  #     
  #     
  #     #HTML('<img src="img/8kYkD3n.jpg">')
  #     
  #     fluidRow(column(width = 4,
  #                     id = "columns",
  #                    HTML(paste0(
  #                             '<div class="image-wrap"><a href="' ,
  #                             text_paintings$filename,
  #                             '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #                             text_paintings$filename,
  #                             '" style="width:100px;height:100px;object-fit:scale-down;"></a></div>'))
  #                         
  #                         
  #                         
  #     ))
  #     
  #     
  # })
  # 
  
  
  # output$image <- renderUI({
  #     
  #     #HTML('<img src="img/8kYkD3n.jpg">')
  #     #HTML('<img src="8kYkD3n.jpg">')
  #     #HTML('<img src="8kYkD3n.jpg">')
  #     
  #     
  #     #HTML('<img src="img/8kYkD3n.jpg">')
  #     
  #     #do multiple fluid rows, call it each time necessary
  #     #math to calculate how many rows and columns
  #     
  #     #input$dimension[0] #width
  #     #input$dimension[1] #height
  # 
  #     #define image size
  #     imgWidth <- 200
  #     imgHeight <- 200
  # 
  #     # #cauluate size of rows based on width
  # 
  #     availWidth <- input$dimension[0] - 100
  #     numImages <- nrow(text_paintings)
  # 
  #     numCols <- max(floor(availWidth / imgWidth), 1)
  #     numRows <- ceiling(numImages / numCols)
  # 
  #     #calculate how many rows are needed and append them to some container, return that container
  #     #create container
  # 
  #     HTML('<div id="img-container">')
  #     #split dataframe into pieces
  #     groups <- text_paintings %>% 
  #         group_by((row_number()-1) %/% (n()/numRows)) %>%
  #         nest %>% pull(data)
  #     
  #     f <- ""
  #     for (i in 1:numRows){
  #         rowImages <- groups[i]
  #         
  #         
  #         # contents <- HTML(paste0(
  #         #     '<div class="image-wrap"><a href="',
  #         #     rowImages[[1]]$filename,
  #         #     '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #         #     rowImages[[1]]$filename,
  #         #     '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>'))
  #         
  #         
  #         # each fluid row should have numCols columns
  #         
  #         
  #         
  #         #problem with cycling through elements in contents
  #         
  #         # need to make a row here
  #         # make numCols columns but only fill to size of dataframe
  #         # temporary f
  #         f_cols <- ""
  #         for (j in 1:numCols){
  # 
  #             if (j<=nrow(rowImages)){
  #                 # make and append cols to a row with a lightbox image in the col
  #                 contents <- HTML(paste0(
  #                         '<div class="image-wrap"><a href="',
  #                         rowImages[[1]]$filename[j],
  #                         '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #                         rowImages[[1]]$filename[j],
  #                         '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>'))
  #                 contents <- HTML(paste0("column(",contents,")"))
  # 
  #                 f_cols <- paste0(f_cols, contents)
  #                 if(j!=nrom(rowImages)){
  #                     f_cols <- paste0(f_cols,",")
  #                 }
  #             }
  # 
  #             else{
  #                # append empty col
  #                 f_cols <- paste0(column())
  #             }
  # 
  #         }
  #         
  #         
  #         #now make cols for each item
  #         
  #         
  #         # f <- paste0(f,paste0(fluidRow(HTML(paste0(
  #         #     '<div class="image-wrap"><a href="' ,
  #         #    data$filename,
  #         #     '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #         #     data$filename,
  #         #     '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>'
  #         # 
  #         #     )))))
  #         
  #         f <- paste0(f,paste0("fluidRow(", contents, ")"))
  #             
  #         if (i!=numRows){
  #             #append , to end to fluidRow
  #             f <- paste0(f,",")
  #         }
  #         
  #         
  #         #js$appendToImgContainer(f)
  # 
  # 
  #     }
  #fluidPage
  # f <- paste0("fluidPage(", f, ")")
  #interpret f as one fluid row
  #eval(parse(HTML(f)))
  #f
  #eval(parse(text=f))
  # fluidRow(HTML(paste0(
  #                     '<div class="image-wrap"><a href="' ,
  #                     text_paintings$filename,
  #                     '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #                     text_paintings$filename,
  #                     '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>'))
  #                 
  #                 
  #                 
  # )
  
  
  # })
  
  
  # output$image <- renderUI({
  #     
  #     
  #     #define image size
  #     imgWidth <- 200
  #     imgHeight <- 200
  #     
  #     # #calculate size of rows based on width
  #     
  #     availWidth <- input$dimension[0] - 100
  #     numImages <- nrow(text_paintings)
  #     
  #     numCols <- max(floor(availWidth / imgWidth), 1)
  #     numRows <- ceiling(numImages / numCols)
  #     
  #     #calculate how many rows are needed and append them to some container, return that container
  #     #create container
  #     
  #     HTML('<div id="img-container">')
  #     #split dataframe into pieces
  #     groups <- text_paintings %>% 
  #         group_by((row_number()-1) %/% (n()/numRows)) %>%
  #         nest %>% pull(data)
  #     
  #     #lapply for both cols and rows
  #     fluidPage(
  #     lapply(1:numRows, function(i){
  #         
  #         fluidRow({
  #             rowImages <- groups[i]
  #             lapply(1:nrow(rowImages[[1]]), function(j){
  #                 
  #                 #12 / numCols #j-1
  #                 column(width = 2, offset = j,
  #                        HTML(paste0(
  #                             '<div class="image-wrap"><a href="',
  #                             rowImages[[1]]$filename[j],
  #                             '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #                             rowImages[[1]]$filename[j],
  #                             '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>'))
  #                        )
  #                 
  #                 
  #             })
  #             
  #         })
  #         
  #         
  #     })
  #     )
  # 
  #     
  # })
  
  
  # output$image <- renderUI({
  # 
  # 
  #     #define image size
  #     imgWidth <- 200
  #     imgHeight <- 200
  # 
  #     # #calculate size of rows based on width
  # 
  #     #availWidth <- input$dimension[0] - 100
  #     availWidth <- 1000
  #     numImages <- nrow(text_paintings)
  # 
  #     numCols <- max(floor(availWidth / imgWidth), 1)
  #     numRows <- ceiling(numImages / numCols)
  # 
  #     #calculate how many rows are needed and append them to some container, return that container
  #     #create container
  # 
  #     HTML('<div id="img-container">')
  #     #split dataframe into pieces
  #     groups <- text_paintings %>%
  #         group_by((row_number()-1) %/% (n()/numRows)) %>%
  #         nest %>% pull(data)
  # 
  #     #lapply for both cols and rows
  # 
  #     fluidPage({
  #         #for (i in 1:numRows){
  # 
  #         lapply(1:numRows, function(i){
  #             rowImages <- (groups[i])[[1]]
  # 
  #             fluidRow({
  # 
  #                 # for (j in 1:nrow(rowImages[[1]])){
  #                 #     w <- 2
  #                 #     column(width = w, offset = w*(j-1),
  #                 #            HTML(paste0(
  #                 #                '<div class="image-wrap"><a href="',
  #                 #                rowImages[[1]]$filename[j],
  #                 #                '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #                 #                rowImages[[1]]$filename[j],
  #                 #                '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>'))
  #                 #     )
  #                 #
  #                 # }
  # 
  #                 lapply(1:nrow(rowImages), function(j){
  #                         w <- 2
  #                         #start off set at 0
  #                         column(width = w, offset = w*(j-1),
  #                                HTML(paste0(
  #                                    '<div class="image-wrap"><a href="',
  #                                    rowImages$filename[j],
  #                                    '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #                                    rowImages$filename[j],
  #                                    '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>')))
  # 
  # 
  #                 })
  # 
  #             })
  # 
  # 
  #         })
  # 
  # 
  # 
  # 
  # 
  # 
  # })
  # 
  # 
  # })
  
  
  # output$image <- renderUI({
  # 
  # 
  #     #define image size
  #     imgWidth <- 200
  #     imgHeight <- 200
  # 
  #     # #calculate size of rows based on width
  # 
  #     #availWidth <- input$dimension[0] - 100
  #     availWidth <- 1000
  #     numImages <- nrow(text_paintings)
  # 
  #     numCols <- max(floor(availWidth / imgWidth), 1)
  #     numRows <- ceiling(numImages / numCols)
  # 
  #     #calculate how many rows are needed and append them to some container, return that container
  #     #create container
  # 
  #     HTML('<div id="img-container">')
  #     #split dataframe into pieces
  #     groups <- text_paintings %>%
  #         group_by((row_number()-1) %/% (n()/numRows)) %>%
  #         nest %>% pull(data)
  # 
  #     #lapply for both cols and rows
  # 
  #     
  #     rows <- tagList()
  #     # for (i in 1:numRows){
  #     #     
  #     # }
  #     # 
  # 
  #     cols <- tagList(
  #         r <- tag$div(), #this is a row
  #         
  #         lapply(1:numCols, function(j){
  #             e <- tag$div() #element of a column
  #             contents <-  HTML(paste0(
  #                                 '<div class="image-wrap"><a href="',
  #                                 rowImages$filename[i],
  #                                 '" data-lightbox="gallery" data-title="',
  #                                     rowImages$Title[i],
  #                                     " | ", rowImages$collection[i],
  #                                     " | ", rowImages$id_no[i],
  #                                 '"><img border="0" alt="" class="fixed-height" src="',
  #                                 rowImages$filename[i],
  #                                 '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>'))
  #             tagAppendChild(r,e)
  #             rm(contents)
  #             rm(e)
  #         })
  #         
  #         )
  # 
  # #create a children list using tag
  # 
  #     rows <- tagList()
  # 
  # 
  #     #tagSetChildren and give it a list
  #     #use tagSetChildren to create rows and cols and final element
  #    x <- tagAppendChild(tags$div(), CHILD)
  #        #[paste0("r",i,"c",j)] = "a")
  #    # this is the  children list()
  # 
  # 
  #     mainPanel({
  #         #for (i in 1:numRows){
  # 
  # 
  # 
  # 
  #         lapply(1:numRows, function(i){
  #             rowImages <- (groups[i])[[1]]
  # 
  #             fluidRow({
  # 
  #                 # for (j in 1:nrow(rowImages[[1]])){
  #                 #     w <- 2
  #                 #     column(width = w, offset = w*(j-1),
  #                 #            HTML(paste0(
  #                 #                '<div class="image-wrap"><a href="',
  #                 #                rowImages[[1]]$filename[j],
  #                 #                '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #                 #                rowImages[[1]]$filename[j],
  #                 #                '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>'))
  #                 #     )
  #                 #
  #                 # }
  # 
  #                 lapply(1:nrow(rowImages), function(j){
  #                     w <- 2
  #                     #start off set at 0
  #                     column(width = w, offset = w*(j-1),
  #                            HTML(paste0(
  #                                '<div class="image-wrap"><a href="',
  #                                rowImages$filename[j],
  #                                '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
  #                                rowImages$filename[j],
  #                                '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>')))
  # 
  # 
  #                 })
  # 
  #             })
  # 
  # 
  #         })
  # 
  # 
  # 
  # 
  # 
  # 
  #     })
  # 
  # 
  # })
  
  
  output$image <- renderUI({
    
    
    #define image size
    # imgWidth <- 200
    # imgHeight <- 200
    # 
    # # #calculate size of rows based on width
    # 
    # #availWidth <- input$dimension[0] - 100
    # availWidth <- 1000
    # numImages <- nrow(text_paintings)
    # 
    # numCols <- max(floor(availWidth / imgWidth), 1)
    # numRows <- ceiling(numImages / numCols)
    # 
    # #calculate how many rows are needed and append them to some container, return that container
    # #create container
    # 
    # HTML('<div id="img-container">')
    # # #split dataframe into pieces
    # groups <- text_paintings %>%
    #     group_by((row_number()-1) %/% (n()/numRows)) %>%
    #     nest %>% pull(data)
    # 
    # #lapply for both cols and rows
    # 
    # 
    # rows <- tagList()
    # # for (i in 1:numRows){
    # #     
    # # }
    # # 
    # 
    # cols <- tagList(
    #     r <- tag$div(), #this is a row
    #     
    #     lapply(1:numCols, function(j){
    #         e <- tag$div() #element of a column
    #         contents <-  HTML(paste0(
    #             '<div class="image-wrap"><a href="',
    #             rowImages$filename[i],
    #             '" data-lightbox="gallery" data-title="',
    #             rowImages$Title[i],
    #             " | ", rowImages$collection[i],
    #             " | ", rowImages$id_no[i],
    #             '"><img border="0" alt="" class="fixed-height" src="',
    #             rowImages$filename[i],
    #             '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>'))
    #         tagAppendChild(r,e)
    #         rm(contents)
    #         rm(e)
    #     })
    #     
    # )
    # 
    # #create a children list using tag
    # 
    # rows <- tagList()
    # 
    # 
    # #tagSetChildren and give it a list
    # #use tagSetChildren to create rows and cols and final element
    # x <- tagAppendChild(tags$div(), CHILD)
    # #[paste0("r",i,"c",j)] = "a")
    # # this is the  children list()
    # 
    # 
    
    
    #TEST On simply input first
    numRows <- 3
    
    groups <- text_paintings %>%
      group_by((row_number()-1) %/% (n()/numRows)) %>%
      nest %>% pull(data)
    
    rowImages <- groups[[1]]
    
    #make single row div, with children as column divs
    
    r <- div()
    r$attribs$class <-  "row"
    
    
    #col-sm-4
    #col-sm-3
    for (j in 1:4){
      e <- div()
      e$attribs$class <- "column"
      
      c <- as.tags(HTML(paste0(
        '<div class="image-wrap"><a href="',
        rowImages$filename[j],
        '" data-lightbox="gallery" data-title="',
        rowImages$Title[j],
        " | ", rowImages$collection[j],
        " | ", rowImages$id_no[j],
        '"><img border="0" alt="" class="fixed-height" src="',
        rowImages$filename[j],
        '" style="width:auto;height:200px;object-fit:scale-down;"></a></div>')))
      
      
      e <- tagAppendChild(e,c)
      r <- tagAppendChild(r,e)
      rm(c)
      rm(e)
      
    }
    
    r
    
    #suppress whitespace?
    
    #<div class="column"></div>
    
    
    # mainPanel({
    # 
    #     lapply(1:numRows, function(i){
    #         rowImages <- (groups[i])[[1]]
    #         
    #         fluidRow({
    #     
    #             lapply(1:nrow(rowImages), function(j){
    #                 w <- 2
    #                 #start off set at 0
    #                 column(width = w, offset = w*(j-1),
    #                        HTML(paste0(
    #                            '<div class="image-wrap"><a href="',
    #                            rowImages$filename[j],
    #                            '" data-lightbox="gallery" data-title="My caption"><img border="0" alt="" class="fixed-height" src="',
    #                            rowImages$filename[j],
    #                            '" style="width:200px;height:200px;object-fit:scale-down;"></a></div>')))
    #                 
    #                 
    #             })
    #             
    #         })
    #         
    #         
    #     })
    #     
    #     
    #     
    #     
    #     
    #     
    # })
    
    
  })
  
  
  
  
  
}

# server <- function(input, output) {
#
#     output$picss <- renderUI({
#         fluidRow(
#             column(12, id="columns",
#                    lapply(df1$recipe.link, function(i) {
#                        box(width=NULL,
#                            title = HTML(paste0('<div class="image-wrap"><a href="images/',
#                                                df1[df1$recipe.link == i, 6],
#                                                '" data-lightbox="image-1" data-title="My caption"><img border="0" alt="" class="fixed-height" src="images/'
#                                                ,df1[df1$recipe.link == i, 6],'"></a></div>'))
#
#                        )}
#                    )
#
#             ))
#     })
#
# }

# Run the application
shinyApp(ui = ui, server = server)
