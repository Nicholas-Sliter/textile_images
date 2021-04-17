#functions.R holds function files for the textile_images project
#Authored by Nicholas Sliter


library(tidyverse)
library(debkeepr)
library(leaflet)


update_wd <- function(){
  
  wd <- getwd() %>% toString()
  if (substring(wd,nchar(wd)-13) != "textile_images"){
    setwd("../")
    wd <- getwd() %>% toString()
  }
  
  return (wd)
}



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



getColorLists <- function(x, colors = c("white","yellow","red","blue","purple","green","black", "grey", "silver", "gold")){
  ##define color list

  ##a private function that takes in a string and a list of colors and returns a vector of the colors in the list
  identifyColors <- function(string, colors){
    
    l = c()
    if(is.na(string) || length(string) == 0){
      l <- append(l,"No color indicated")
      return(l)
    }
    
    for (c in colors){
      if (str_detect(string, c)){
        l <- append(l,c)
      }
    }
    if (length(l) == 0){
      l <- append(l,"Other")
    }
    
    return(l)
  }
  
  identifyColors_vec <- Vectorize(identifyColors,vectorize.args = "string")
  
  ##Create a empty vector for each element in colorList
  data <- x %>% mutate(colorList = identifyColors_vec(x$textile_color_arch,colors))
                           
                           ##ifelse(is.na(textile_color_arch),
                             ##                "No color indicated"))
  return (data)
}



filter_by_input <- function(data,col,data_col){
  #given a dataset, data, a column to filter by, col, and the col in the data, data_col,
  #return data with data_col filtered by col
    if(exists(col) && exists(data_col) && length(col) != 0){
      data <- data %>%
        filter(data_col %in% col)
    }
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