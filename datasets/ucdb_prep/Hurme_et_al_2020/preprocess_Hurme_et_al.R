library(signcon)
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
    # flip number conditions to get a positive RTE effect
    data <- read.csv(data_fn) %>%
      mutate(Number = max(Number) + 1 - Number) %>%
      rename(idv = Subject, iv = Number, dv = correctRT) %>%
      mutate(exp = paste(study_name, exp_name, StimulusColor ,sep = '_'))
    
  } else if(exp_name == 'TMS') {
    # flip number conditions to get a positive RTE effect
    data <- read.csv(data_fn) %>%
      mutate(Number.LogLevel5. = max(Number.LogLevel5.) + 1 - Number.LogLevel5.) %>%
      rename(idv = Subject, iv = Number.LogLevel5., dv = correctRT.LogLevel5.) %>%
      mutate(exp = paste(study_name, exp_name, StimulusColor.SubTrial. ,sep = '_'))
  } else {print(paste('No such exp name:', exp_name))}
  return (data %>% dplyr::select(idv, iv, exp, dv))
}
data <- do.call(rbind, lapply(exps, read_data_f))
write.csv(data, paste0(study_name,'.csv'))

# validate against Fig 2C (Masking) and 3C )TMS in the paper
data %>% group_by(exp, idv, iv) %>% summarise(m = mean(dv)) %>%
  group_by(exp, iv) %>% summarise(m = mean(m)) %>% group_by(exp) %>% summarise(m = diff(m))

data %>% filter(exp == 'Hurme et al_2020_Masking_Blue') %>% 
  group_by(exp, idv, iv) %>% summarise(m = mean(dv)) %>%
  group_by(exp, idv) %>% summarise(m = diff(m)) %>% group_by(exp) %>% summarise(m = mean(m))

data %>% filter(exp == 'Hurme et al_2020_Masking_Blue') %>% 
  group_by(exp, idv, iv) %>% summarise(m = mean(dv)) %>%
  group_by(exp, iv) %>% summarise(m = mean(m)) %>% group_by(exp) %>% summarise(m = diff(m))