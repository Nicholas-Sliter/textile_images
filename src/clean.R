library(xlsx)

source("functions.R")
#Move working directory if in src
wd <- getwd() %>% toString()
if (substring(wd,nchar(wd)-2) == "src"){
  setwd("../")
}


#reading in Textile Images dataset 
text_images <- read_xlsx("datasets/TextileImageDataSet.xlsx")

#reading in Textile Materials dataset 
text_materials <- read_xlsx("datasets/TextileMaterialDataSet.xlsx")


#cleaning text_materials 
text_samples <- 
  text_materials %>%
  mutate(textile_pattern_visual = 
           str_replace_all(textile_pattern_visual, c("stripe" = "striped", "none" = NA)),
         image_ID = str_remove_all(image_url, "https://imgur.com/"),
         Title = NA, 
         Artist = NA) %>%
  rename(Date = orig_date)

text_samples <- text_samples %>% 
  mutate(filename = paste0("img/",image_ID,".jpg"))

text_paintings <- 
  text_images %>%
  mutate(image_ID = str_remove_all(imgur_url, "https://imgur.com/")) %>%
  rename(catalogue_url = source_url)

text_paintings <- text_paintings %>% 
  mutate(filename = paste0("img/",image_ID,".jpg"))


if (substring(wd,nchar(wd)-13) == "textile_images"){
  setwd("/datasets")
}
if (substring(wd,nchar(wd)-7) != "datasets"){
  setwd("../datasets")
}
# save these
write.csv(text_samples,"image_samples.csv")
write.csv(text_paintings,"painting_samples.csv")

write.xlsx(text_samples,"image_samples.xlsx")
write.xlsx(text_paintings,"painting_samples.xlsx")

wd <- update_wd()
