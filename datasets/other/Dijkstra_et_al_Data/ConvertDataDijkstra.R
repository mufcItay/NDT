library(tidyr)
library(dplyr)
library(reshape2)
library(foreign)

dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)
expSep = '_'

studyName <- "Dijkstra"
file_list <- list.files('Converted', pattern = ".csv", full.names = TRUE)
data <- do.call("rbind",lapply(file_list,FUN=function(files) {
  read.csv(files,header=T)
}))
data <-data %>%
  rename(cong = iv, rt = dv) %>% 
  mutate(Exp = studyName) %>%
  select(subNum, cong, rt, Exp)
write.csv(data, paste0(studyName,'.csv'))


