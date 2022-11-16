library(weaknull)
library(dplyr)

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)

# read the dataset
data_dir <- 'data'
study_name <- 'Hurme et al_2020'
exps <- list.dirs(data_dir,recursive = FALSE)
read_data_f <- function(dir_name) {
  exp_name <- strsplit(dir_name, .Platform$file.sep)[[1]][2]
  data_fn <- list.files(dir_name, pattern = '*.csv', full.names = TRUE)
  if(exp_name == 'Masking') {
    data <- read.csv(data_fn) %>%
      rename(idv = Subject, iv = Number, dv = correctRT) %>%
      mutate(exp = paste(study_name, exp_name, StimulusColor ,sep = '_'))
    
  } else if(exp_name == 'TMS') {
    data <- read.csv(data_fn) %>%
      rename(idv = Subject, iv = Number.LogLevel5., dv = correctRT.LogLevel5.) %>%
      mutate(exp = paste(study_name, exp_name, StimulusColor.SubTrial. ,sep = '_'))
  } else {print(paste('No such exp name:', exp_name))}
  return (data %>% select(idv, iv, exp, dv))
}
data <- do.call(rbind, lapply(exps, read_data_f))
write.csv(data, paste0(study_name,'.csv'))
