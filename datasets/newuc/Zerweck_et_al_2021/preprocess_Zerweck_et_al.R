library(weaknull)
library(dplyr)

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)

# read the dataset
study_name <- 'Zerweck et al_2021'
data_dir <- 'data'
exps <- list.dirs(data_dir,recursive = FALSE)
read_data_f <- function(dir_name) {
  exp_num <- substring(dir_name,  nchar(dir_name), nchar(dir_name))
  if(exp_num == '1') {
    data_fn <- list.files(dir_name, pattern = '*ndirectTask.dat', full.names = TRUE)
    data <- read.csv(data_fn, sep = ' ') %>%
      rename(idv = sub, iv = congruency) %>%
      filter(time_error =="Normal",
        key_error == 'correct') %>%
      mutate(exp = paste(study_name, exp_num, sep = '_'))
  } else if(exp_num == '2') {
    data_fn <- list.files(dir_name, pattern = '*ndirectTask.dat', full.names = TRUE)
    data <- read.csv(data_fn, sep = ' ')
    uc_contrasts <- sort(unique(data$Contrast))[1:2]
    data <- data %>%
      filter(Contrast %in% uc_contrasts,
             TimingError == 0, Task== "RTTaskExp", ResponseTime=="Normal",
             ResponseError=="keypress_correct") %>%
      mutate(exp = paste(study_name, exp_num,Contrast, sep = '_')) %>%
      rename(idv = VP, iv = Congruency, RT = RT.Matlab)
  } else if(exp_num == '3') {
    data_fn <- paste(dir_name, 'data_Exp3', 'om1203_rttask_10.dat', sep = .Platform$file.sep)
    data <- read.csv(data_fn, sep = ' ') %>%
      filter(!endsWith(block_type, 'Prac'),
             time_error=="Normal", key_error=="correct") %>%
      mutate(exp = paste(study_name, exp_num, 'Duration' ,'10' ,sep = '_')) %>%
      rename(idv = sub, iv = congruency)
  }
  
  return (data %>%
            rename(dv = RT) %>%
            select(idv, iv, exp, dv))
}
data <- do.call(rbind, lapply(exps, read_data_f))
write.csv(data, paste0(study_name,'.csv'))
