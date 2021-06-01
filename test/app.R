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
library(shinythemes)
library(shinyPagerUI)


#Define constant page length (number of rows)
#PAGE_ROWS <- 10
PAGE_ROWS <- 5

#refrence with constants['desired input']
constants <- c(
  
  'ROWS_PER_PAGE' = 5,
  'ROW_SIZE' = 4
)



text_images <- read_xlsx("datasets/TextileImageDataSet.xlsx")
text_materials <- read_xlsx("datasets/TextileMaterialDataSet.xlsx")
text_samples <- read_xlsx("datasets/image_samples.xlsx")
text_paintings <- read_xlsx("datasets/painting_samples.xlsx")

joined <- full_join(text_samples,text_paintings)


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

    includeScript("www/js/lightbox.js")
),


theme=shinytheme("sandstone"),
#button to show a dropdown menu of textiles

titlePanel(title = "Dutch Textile Trade: The Material and Pictorial Record"),
p("The Dutch East and West India companies traded items of varying origins,
    qualities, and uses across continents. The exploration of their ship inventories 
    provides many insights into different cultures and customs of the time. Of particular 
    interest is the vast array of traded textiles of different colors, patterns, and 
    processes aboard these Dutch ships. This application allows you to explore both the 
    textiles aboard Dutch West India Company (WIC) and Dutch East India Company (VOC) vessels 
    through archival documents and textiles depicted in artwork belonging to the Rijksmuseum. 
    Presenting a glimpse into the extensive variety of textiles and into the fashions of the 1700s, 
      this app allows the user to draw meaningful comparisons between the “Material” and “Pictorial” archives. "),
sidebarPanel(
    h3("The Pictoral Record"),
    radioButtons(inputId = "DataSet",
                 label = "Choose an Image Type",
                 choices = c("Samples", "Paintings", "Both")),
    uiOutput(outputId = "PrimaryColor"),
    uiOutput(outputId = "SecondaryColor"),
    uiOutput(outputId = "Pattern"),
    uiOutput(outputId = "Process"),
    p("Click on an Image to view information:"),
    htmlOutput(outputId = "ImageInfo"),
    uiOutput(outputId = 'Pager')
    #pageruiInput('pager', page_current = 1, pages_total = 1),
    ),


mainPanel(
  
    
        
        h3("Investigating the Pictorial Record"),
        
        tabsetPanel(tabPanel(title = "Images",
                             uiOutput("image")))
                    
                    
                    
                    
                             #plotOutput(outputId = "ImageSelection",
                                        #click = "plot_click")),
                    #tabPanel(title = "Table",
                             #dataTableOutput(outputId = "DataTable")))),


# 
# fluidRow(
#     
#     #uiOutput("image"),
#     p("here")
#     #uiOutput("imageBox")
# 
#     )

)
)






server <- function(input, output) {
    reactive_image_data <- reactive({
        ## isolate everything except input$Dataset
        
        if (input$DataSet == "Both"){
          chosen_data <- joined
        }
        else if (input$DataSet == "Samples") {
            chosen_data <- text_samples
        }
        else {
            chosen_data <- text_paintings
        }
        
        
        chosen_data
    })
    
    
    
    # #making Primary Color Choices
    output$PrimaryColor <- renderUI({
        primary_colors <-
            unique(as.vector(reactive_image_data()$textile_color_visual_primary))
        
        selectizeInput(
            inputId = "PrimaryColor",
            label = "Choose a Primary Color",
            choices = primary_colors,
            #levels(factor(chosen_data01$textile_color_visual_primary)),
            multiple = TRUE
        )
    })
    #
    #
    #making Secondary Color Choices
    output$SecondaryColor <- renderUI({
        list <- c()
        
        pre_unique <- unique(as.vector(
                     reactive_image_data()$textile_color_visual_secondary))
        
        numRows = length(pre_unique)

        for (i in 1:numRows){

            str <- pre_unique[i]
            strlist <- strsplit(str, ", ")
            
            for (j in 1:length(strlist)){
                list <- append(list,strlist[[1]][j])
            }

        }
        
        secondary_colors <-
            unique(as.vector(
                list
            ))
        
        selectizeInput(
            inputId = "SecondaryColor",
            label = "Choose a Secondary Color",
            choices = secondary_colors,
            #levels(factor(reactive_image_data()$textile_color_visual_secondary)),
            multiple = TRUE
        )
    })
    #
    #
    # #making Pattern Choices
    output$Pattern <- renderUI({
        patterns <-
            unique(as.vector(reactive_image_data()$textile_pattern_visual))
        selectizeInput(
            inputId = "Pattern",
            label = "Choose a Pattern",
            choices = patterns,
            multiple = TRUE
        )
    })
    #
    # #making Process Choices
    output$Process <- renderUI({
        process <-
            unique(as.vector(reactive_image_data()$textile_process_visual))
        selectizeInput(
            inputId = "Process",
            label = "Choose a Process",
            choices = process,
            multiple = TRUE
        )
    })
    
    
    
    
    
    
    # total_pages <- reactive({
    #   
    #   
    #   
    # 
    # })
    # 
    # current_page <- reactive({
    #   
    #   
    #   
    #   
    # })
    # 
    # 
    
    ##update this in the image portion
    output$Pager <- renderUI({

      
      numPages <- rows_and_pages()[2]#Pager()[2]
      currPage <- rows_and_pages()[3] #Pager()[1]

      if (is.na(numPages) || is.null(numPages)){
        numPages <- 1 
      }
      
      if (is.na(currPage) || is.null(currPage)){
       currPage <- 1 
      }
      
      
      if (as.numeric(currPage) > numPages){
        
       currPage = numPages
       
      }
      
      pageruiInput('pager', page_current = currPage, pages_total = numPages) #page_current = currPage



    })
    
    
    
    
    
    
    
    
    
    
    
    

    # #filtering by primary color
    chosen_data1 <- reactive({
        if (!is.null(input$PrimaryColor))
        {
            reactive_image_data() %>%
                filter(textile_color_visual_primary %in% input$PrimaryColor)
        }
        else{
            reactive_image_data()
        }
    })
    #
    #
    # #filtering by secondary color
    chosen_data2 <- reactive({
        if (!is.null(input$SecondaryColor))
        {
            chosen_data1() %>%
                filter(str_detect(textile_color_visual_secondary,input$SecondaryColor))
                #filter(textile_color_visual_secondary %in% input$SecondaryColor)
        }
        else{
            chosen_data1()
        }
    })
    #
    #
    # #filtering by pattern
    chosen_data3 <- reactive({
        if (!is.null(input$Pattern))
        {
            chosen_data2() %>%
                filter(textile_pattern_visual %in% input$Pattern)
        }
        else{
            chosen_data2()
        }
    })

    # #filtering by process
    chosen_data4 <- reactive({
        if (!is.null(input$Process))
        {
            chosen_data3() %>%
                filter(textile_process_visual %in% input$Process)
        }
        else{
            chosen_data3()
        }
    })
    

    imageData <- reactive(chosen_data4())
    pager_state <- reactive({input$pager})
    
    
    rows_and_pages <- reactive({

      # [number of rows, number of pages, current page]

      data <- chosen_data4()

      numRows <- ceiling(nrow(data)/4)
      numPages <- ceiling(numRows/constants['ROWS_PER_PAGE'])

      reactive(updatePageruiInput(session, 'pager', pages_total = numPages))
      
      c(numRows,numPages,pager_state()[1])

    })
    
    #convert this
    
    
    
    pages <- reactive({
      
      ROW_SIZE <- 4
      
      data <- chosen_data4()
      
      numRows <- ceiling(nrow(data)/ROW_SIZE)
      numPages <- ceiling(numRows/constants['ROWS_PER_PAGE'])
      
      reactive(updatePageruiInput(session,'pager',pages_total = numPages))
      
      
      currPage <- pager_state()[[1]]

      row_size <- constants["ROW_SIZE"]
      rows_page <- constants['ROWS_PER_PAGE']
      

      
      #just group by page size to get page
      groups <- data %>%
        group_by((row_number()-1) %/% (n()/numPages)) %>%
        nest %>% pull(data)
      
      
      #later just divide by how many rows per page to get the rows in the given page
      
      pageList <- c()
      
      for (i in 1:numPages){
        
       page <- groups[[i]]
      
       pageList <- append(pageList, list(page))
        
      }
      
    
      pageList
      
      
    })
    
    
    #rows = pages()[[input$pager$page_current]]
    
    #STOP this, just do the orignal implementation but have bounds based on current page
    
    
    
    create_row <- function(data){
      
      r <- div()
      #no-gutters
      r$attribs$class <-  "row gy-5"
      
      
      #col-sm-4
      #col-sm-3
      for (j in 1:length(data$filename)){
        e <- div()
        filename <- data$filename[j]
        #calculate column class here
        e$attribs$class <- "col-sm-3 gx-1"
        
        title <- ""
        if (is.na(data$textile_name[j])){
          
          title <- HTML(paste0(data$Title[j],
                               " | ", data$collection[j],
                               " | ", data$id_no[j]))
          
        }
        else{
          
          title <- HTML(paste0(data$textile_name[j],
                               " | ", data$collection[j],
                               " | ", data$id_no[j]))
          
        }
        
        
        #change image to relative from www that is, img/, not /img/
        
        
        #make image size adjustable by having height (size) input
        c <- as.tags(HTML(paste0(
          '<div class="image-wrap"><a href="',
          filename,
          '" data-lightbox="gallery" data-title="',
          title,
          # rowImages$Title[j],
          # " | ", rowImages$collection[j],
          # " | ", rowImages$id_no[j],
          '"><img border="0" alt="" class="fixed-height" src="',
          filename,
          '" style="width:90%;height:85%;object-fit:cover;"></a></div>')))
        
        
        e <- tagAppendChild(e,c)
        r <- tagAppendChild(r,e)
        rm(c)
        rm(e)
        
      }
      
      
      return (r)
      
      
    }
    
    
    output$image <- renderUI({
        
    
      #need to fix this paging....
      
      req(pager_state()[[1]])
      
      
      currPage <- pager_state()[[1]]
      # if (exists('pager_state')){
      #   currPage <- pager_state()[[1]]
      # }
      
      
      #page <- pages()[currPage]
      
      page <- pages()[[currPage]]
      
      #need to convert page into rows
      rows <- page %>%
        group_by((row_number()-1) %/% (n()/constants['ROWS_PER_PAGE'])) %>%
        nest %>% pull(data)
      
      
      v <- div()
      
      for (i in 1:length(rows)){
        
        row <- rows[[i]]
        v <- tagAppendChild(v, create_row(row))
        
        
      }
      
      v
      
      
      
      
      
      
      
      
      
      
        # 
        # 
        # 
        # data <- chosen_data4()
        # 
        # 
        # 
        # #Here add a function to do this in pages of a certain size
        # 
        # # 
        # # numRows <- ceiling(nrow(data)/4)
        # # numPages <- ceiling(numRows/PAGE_ROWS)
        # 
        # 
        # numRows <- rows_and_pages()[1]
        # numPages <- rows_and_pages()[2]
        # currPage <- rows_and_pages()[3]
        # 
        # #session #page_current = currPage
        # #reactive(updatePageruiInput(session, 'pager', pages_total = numPages))
        # 
        # groups <- data %>%
        #     group_by((row_number()-1) %/% (n()/numRows)) %>%
        #     nest %>% pull(data)
        # 
        # ##for current page, 
        # ##get range of current page
        # 
        # lower_bound <- (currPage)*PAGE_ROWS
        # upper_bound <- min(numRows,lower_bound + PAGE_ROWS) -1
        # 
        # ##then take the min of upper bound and number of rows
        # 
        # 
        # v <- div()
        # 
        # #get current page
        # #currentPage <- pages()[currPage]
        # 
        # 
        # #just keep doing what im doing but use current page to change i bounds
        # 
        # for (i in lower_bound:upper_bound){
        # 
        # 
        # #for (i in 1:length(currentPage)){
        # #for (i in lower_bound:min(upper_bound,numRows)){
        # #for (i in 1:numRows){
        #     
        # 
        # #rowImages <- groups[[i]]
        # rowImages <- groups[[i]]
        # 
        # #make single row div, with children as column divs
        # 
        # 
        # r <- div()
        # #no-gutters
        # r$attribs$class <-  "row gy-5"
        # 
        # 
        # #col-sm-4
        # #col-sm-3
        # for (j in 1:nrow(rowImages)){
        #     e <- div()
        #     #calculate column class here
        #     e$attribs$class <- "col-sm-3 gx-1"
        #     
        #     title <- ""
        #     if (is.na(rowImages$textile_name[j])){
        #     
        #         title <- HTML(paste0(rowImages$Title[j],
        #                              " | ", rowImages$collection[j],
        #                              " | ", rowImages$id_no[j]))
        #     
        #     }
        #     else{
        #         
        #         title <- HTML(paste0(rowImages$textile_name[j],
        #                              " | ", rowImages$collection[j],
        #                              " | ", rowImages$id_no[j]))
        #         
        #     }
        #     
        #     
        #     #change image to relative from www that is, img/, not /img/
        #     
        #     
        #     #make image size adjustable by having height (size) input
        #     c <- as.tags(HTML(paste0(
        #                     '<div class="image-wrap"><a href="',
        #                     rowImages$filename[j],
        #                     '" data-lightbox="gallery" data-title="',
        #                     title,
        #                     # rowImages$Title[j],
        #                     # " | ", rowImages$collection[j],
        #                     # " | ", rowImages$id_no[j],
        #                     '"><img border="0" alt="" class="fixed-height" src="',
        #                     rowImages$filename[j],
        #                     '" style="width:90%;height:85%;object-fit:cover;"></a></div>')))
        # 
        #     
        #     e <- tagAppendChild(e,c)
        #     r <- tagAppendChild(r,e)
        #     rm(c)
        #     rm(e)
        #     
        # }
        # 
        # v <- tagAppendChild(v,r)
        # 
        # }
        # 
        # v

        
       
  
    })
    
    
  
    
    
    
    
}


# Run the application
shinyApp(ui = ui, server = server)
