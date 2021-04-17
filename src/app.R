library(shiny)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(readr)
library(readxl)
library(grid)
library(gridExtra)
library(jpeg)

source("functions.R")

##a
#Move working directory if in src
# wd <- getwd() %>% toString()
# if (substring(wd,nchar(wd)-13) != "textile_images"){
#   setwd("../")
#   wd <- getwd() %>% toString()
# }

wd <- update_wd()

# if (substring(wd,nchar(wd)-2) == "src"){
#   setwd("../")
#   wd <- getwd() %>% toString()
# }


#Reading in the data 
wic_voc_path <- file.path("datasets/WIC_VOC_Cleaned.csv")
wic_voc <- read_csv(wic_voc_path)


###REMOVE AND ADD TO CLEAN FILE
#Creating a subset and applying additional cleaning to data
wic_voc_filtered <- wic_voc
wic_voc_filtered$textile_quality_arch <- as.character(wic_voc$textile_quality_arch)
wic_voc_filtered$textile_quality_arch[wic_voc$textile_quality_arch == "fine"] <-"fijn (fine)"




#### GROUP 3 CODE ####

#reading in Textile Images dataset 
text_images <- read_xlsx("datasets/TextileImageDataSet.xlsx")

#reading in Textile Materials dataset 
text_materials <- read_xlsx("datasets/TextileMaterialDataSet.xlsx")


#cleaning text_materials 
# text_samples <- 
#   text_materials %>%
#   mutate(textile_pattern_visual = 
#            str_replace_all(textile_pattern_visual, c("stripe" = "striped", "none" = NA)),
#          image_ID = str_remove_all(image_url, "https://imgur.com/"),
#          Title = NA, 
#          Artist = NA) %>%
#   rename(Date = orig_date)
# 
# #cleaning text_images 
# text_paintings <- 
#   text_images %>%
#   mutate(image_ID = str_remove_all(imgur_url, "https://imgur.com/")) %>%
#   rename(catalogue_url = source_url)


text_samples <- read_xlsx("datasets/image_samples.xlsx")
text_paintings <- read_xlsx("datasets/painting_samples.xlsx")




ui <- fluidPage(
  
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
    h3("The Material Record"),
    selectInput(
      inputId = "CompanyName",
      label = "Company:",
      choices =  c("Both WIC & VOC", unique(wic_voc$company))),
    selectInput(inputId = "Color",
                label = "Color:",
                choices =  c("All textiles", unique(wic_voc$color_category))),
    selectInput(inputId = "TextileQuality",
                label = "Textile Quality:",
                choices =  c("All textile qualities", unique(wic_voc$textile_quality_arch))),
    selectInput(inputId = "process",
                label = "Process:",
                choices =  c("All textile processes", unique(wic_voc$textile_process_arch))),
    selectInput(inputId = "pattern",
                label = "Pattern:",
                choices =  c("All textile patterns", unique(wic_voc$textile_pattern_arch))),
    selectInput(inputId = "fiber",
                label = "Fiber:",
                choices =  c("All textile fibers", unique(wic_voc$textile_fiber_arch))),
    downloadButton("downloadData", "Download"),
    h3("The Pictoral Record"),
    radioButtons(inputId = "DataSet",
                 label = "Choose an Image Type",
                 choices = c("Samples", "Paintings")),
    uiOutput(outputId = "PrimaryColor"),
    uiOutput(outputId = "SecondaryColor"),
    uiOutput(outputId = "Pattern"),
    uiOutput(outputId = "Process"),
    p("Click on an Image to view information:"),
    htmlOutput(outputId = "ImageInfo")),
  
  
  mainPanel(
    h3("Investigating the Material Record"),
    p("Introduction"),
    tabsetPanel(
      tabPanel(title = "Applicable Textile(s)",
               tableOutput(outputId = "textilename_table")),
      tabPanel(title = "Plot",
               plotOutput(outputId = "Graph")),
      h3("Investigating the Pictorial Record"),
      tabsetPanel(tabPanel(title = "Images",
                           plotOutput(outputId = "ImageSelection",
                                      click = "plot_click")),
                  tabPanel(title = "Table",
                           dataTableOutput(outputId = "DataTable")))))
  
)




server <- function(input, output, session) {
  
  #Application of the action button
  # observeEvent(input$button1, {
  
  output$Graph <- renderPlot({
    
    #filter the data to only include the selected textile
    #wic_voc_filtered <- react_data()
    
    
    if(input$Color != "All textiles"){wic_voc_filtered <- wic_voc_filtered%>%
      filter(color_category == input$Color)}

    if(input$CompanyName != "Both WIC & VOC"){wic_voc_filtered <- wic_voc_filtered %>%
      filter(company == input$CompanyName)}

    if(input$TextileQuality != "All textile qualities"){wic_voc_filtered <- wic_voc_filtered %>%
      filter(textile_quality_arch == input$TextileQuality)}

    if(input$process != "All textile processes"){wic_voc_filtered <- wic_voc_filtered %>%
      filter(textile_process_arch == input$process)}

    if(input$pattern != "All textile patterns"){wic_voc_filtered <- wic_voc_filtered %>%
      filter(textile_pattern_arch == input$pattern)}

    if(input$fiber != "All textile fibers"){wic_voc_filtered <- wic_voc_filtered %>%
      filter(textile_fiber_arch == input$fiber)}

    #group the data by destination country
    wic_voc_grouped <- wic_voc_filtered %>%
      group_by(dest_loc_region)
    
    #draw plot
    wic_voc_grouped%>%
      ggplot(aes(x = textile_name,
                 y= textile_quantity))+
      geom_bar(stat="identity")+
      ggtitle("Total Textile Quantity of Applicable Textiles")+
      ylab("Total Textile Quantity") +
      xlab("Applicable Textiles") +
      coord_flip() +
      theme_minimal()
    
    # })
  })
  
  output$textilename_table <- renderTable({
    
    #filter the data to only include the selected textile
    if(input$Color != "All textiles"){wic_voc_filtered <- wic_voc_filtered%>%
      filter(color_category == input$Color)}
    
    if(input$CompanyName != "Both WIC & VOC"){wic_voc_filtered <- wic_voc_filtered %>%
      filter(company == input$CompanyName)}
    
    if(input$TextileQuality != "All textile qualities"){wic_voc_filtered <- wic_voc_filtered %>%
      filter(textile_quality_arch == input$TextileQuality)}
    
    if(input$process != "All textile processes"){wic_voc_filtered <- wic_voc_filtered %>%
      filter(textile_process_arch == input$process)}
    
    if(input$pattern != "All textile patterns"){wic_voc_filtered <- wic_voc_filtered %>%
      filter(textile_pattern_arch == input$pattern)}
    
    if(input$fiber != "All textile fibers"){wic_voc_filtered <- wic_voc_filtered %>%
      filter(textile_fiber_arch == input$fiber)}
    
    textile_name_list <- unique(wic_voc_filtered$textile_name)

    
    
    return(
      if(length(textile_name_list > 20))
      {
        return(
          textile_name_list[1:20]
        )
      }
      else{
        
        return(
          textile_name_list[1:length(textile_name_list)]
          
        )}
    )
  })
  
  
  react_data <- reactive({
    wic_voc_filtered[,c("textile_name", "color_category",
                        "company", "textile_quality_arch",
                        "textile_process_arch", "textile_pattern_arch", "textile_fiber_arch")]
  })
  
  output$user_selected_dt <- renderTable({
    react_data()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("wic_voc_filtered", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(react_data(), file)
      
      
    }
  )
  
  
  ##### GROUP 3 WORK #####
  
  #making a reactive data frame for chosen data sets 
  reactive_image_data <- reactive({
    ## isolate everything except input$Dataset
    
    if (input$DataSet == "Samples") {chosen_data <- text_samples}
    else {chosen_data <- text_paintings}})
  
  
  
  #making Primary Color Choices
  output$PrimaryColor <- renderUI({
    primary_colors <- unique(as.vector(reactive_image_data()$textile_color_visual_primary))
    selectizeInput(inputId = "PrimaryColor", 
                   label = "Choose a Primary Color", 
                   choices = primary_colors,
                   #levels(factor(chosen_data01$textile_color_visual_primary)),
                   multiple = TRUE)})
  
  
  #making Secondary Color Choices 
  output$SecondaryColor <- renderUI({
    secondary_colors <- unique(as.vector(reactive_image_data()$textile_color_visual_secondary))
    selectizeInput(inputId = "SecondaryColor", 
                   label = "Choose a Secondary Color", 
                   choices = secondary_colors,
                   #levels(factor(reactive_image_data()$textile_color_visual_secondary)),
                   multiple = TRUE)})
  
  
  #making Pattern Choices
  output$Pattern <- renderUI({
    patterns <- unique(as.vector(reactive_image_data()$textile_pattern_visual))
    selectizeInput(inputId = "Pattern", 
                   label = "Choose a Pattern", 
                   choices = patterns,
                   multiple = TRUE)})
  
  #making Process Choices
  output$Process <- renderUI({
    process <- unique(as.vector(reactive_image_data()$textile_process_visual))
    selectizeInput(inputId = "Process", 
                   label = "Choose a Process", 
                   choices = process,
                   multiple = TRUE)})
  
  #filtering by primary color
  chosen_data1 <- reactive({
    
    if (!is.null(input$PrimaryColor))
    {reactive_image_data() %>%
        filter(textile_color_visual_primary %in% input$PrimaryColor)}
    else{reactive_image_data()}
  })
  
  
  #filtering by secondary color
  chosen_data2 <- reactive({
    
    if (!is.null(input$SecondaryColor))
    {chosen_data1() %>%
        filter(textile_color_visual_secondary %in% input$SecondaryColor)}
    else{chosen_data1()}
  })
  
  
  #filtering by pattern
  chosen_data3 <- reactive({
    
    if (!is.null(input$Pattern))
    {chosen_data2() %>%
        filter(textile_pattern_visual %in% input$Pattern)}
    else{chosen_data2()}
  })
  
  #filtering by process
  chosen_data4 <- reactive({
    
    if (!is.null(input$Process))
    {chosen_data3() %>%
        filter(textile_process_visual %in% input$Process)}
    else{chosen_data3()}
  })
  
  
  #outputting images
  #code taken from 
  #https://stackoverflow.com/questions/53386688/how-to-display-multiple-pictures-png-that-are-stored-locally-on-shiny 
  
  output$ImageSelection  <- renderPlot({
    
    wd <- update_wd()
    # filename <- normalizePath(file.path(paste0(chosen_data4()$image_ID, 
    #                                            ".jpg", 
    #                                            sep = "")))
    filename <- normalizePath(paste0(wd,chosen_data4()$filename))
    filename <- filename[file.exists(filename)]
    jpegs = lapply(filename, readJPEG)
    #margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
    asGrobs = lapply(jpegs, rasterGrob)
    #asGrobs = lapply("+", margin)
    p <- grid.arrange(grobs= asGrobs, 
                      nrow = 20, 
                      ncol = 2, 
                      padding = unit(10, "line"))
  }, 
  width = 900, 
  height = 5000,
  res = 20)
  
  
  
  output$ImageInfo <- renderText({
    imagetext <- function(e){
      if (is.null(e)) return(" ")
      paste0("Title: ", chosen_data4()$Title, "<br/>",
             "Artist: ", chosen_data4()$Artist, "<br/>",
             "Date: ", chosen_data4()$Date,"<br/>",
             "Collection: ", chosen_data4()$collection,"<br/>",
             "Catalog URL: ", chosen_data4()$catalogue_url,"<br/>",
             "Colors: ", chosen_data4()$textile_color_visual_primary, chosen_data4()$textile_color_visual_secondary,"<br/>",
             "Pattern: ", chosen_data4()$textile_pattern_visual, "<br/>",
             "Process: ", chosen_data4()$textile_process_visual, "<br/>","<br/>" )
      #Add space below this
    }
    
    
    paste0(imagetext(input$plot_click))
    
  })
  
  
  #outputing Data Table
  output$DataTable <- renderDataTable({ 
    
    chosen_data4()
    
  })
  
  
  
  
}


shinyApp(ui, server)
