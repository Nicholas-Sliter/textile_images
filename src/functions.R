#functions.R holds function files for the textile_images project
#Authored by Nicholas Sliter


library(tidyverse)
library(debkeepr)
library(leaflet)



getColorGroups <- function(col){
  data <- col %>% mutate(colorGroup = ifelse(is.na(textile_color_arch),
                                              "No color indicated",
                                              ifelse(str_detect(textile_color_arch, "gold"),
                                                     "gold",
                                                     ifelse(str_detect(textile_color_arch, "red") | str_detect(textile_color_arch, "scarlet") | str_detect(textile_color_arch, "purple"),
                                                            "red",
                                                            ifelse(str_detect(textile_color_arch, "blue") | str_detect(textile_color_arch, "green"),
                                                                   "blue-green",
                                                                   ifelse(str_detect(textile_color_arch, "white"),
                                                                          "white",
                                                                          ifelse(str_detect(textile_color_arch, "black"),
                                                                                 "black",
                                                                                 ifelse(str_detect(textile_color_arch, "grey"),
                                                                                        "grey",
                                                                                        ifelse(str_detect(textile_color_arch, "yellow"),
                                                                                               "yellow",
                                                                                               ifelse(str_detect(textile_color_arch, "silver"),
                                                                                                      "silver",
                                                                                                      no = "Other"))))))))))
  
  return(data)
}


filter_by_inputs <- function(data,input){
  private_filter_by <- function(data, col, data_col){
    if(exists(col) && exists(data_col) && length(col) != 0){
      data <- data %>%
        filter(data_col %in% col)
    }
    return(data)
    
  }
  
  if(isolate(input$dataSet) != "Both"){
    data <- private_filter_by(data,isolate(input$dataSet),data$company)
  }
  data <- private_filter_by(data,isolate(input$textileName),data$textile_name)
  data <- private_filter_by(data,isolate(input$colors),data$colorGroup)
  data <- private_filter_by(data,isolate(input$patterns),data$textile_pattern_arch)
  data <- private_filter_by(data,isolate(input$process),data$textile_process_arch)
  data <- private_filter_by(data,isolate(input$fibers),data$textile_fiber_arch)
  data <- private_filter_by(data,isolate(input$geography),data$textile_geography_arch)
  data <- private_filter_by(data,isolate(input$qualities),data$textile_quality_arch)
  data <- private_filter_by(data,isolate(input$inferredQualities),data$textile_quality_inferred)
  data <- private_filter_by(data,isolate(input$year),data[[return_yrColname(isolate(input$regionChoice))]])
  
  return(data)
}