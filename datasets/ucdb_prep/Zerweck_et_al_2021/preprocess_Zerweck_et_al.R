library(dplyr)

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
    uc_contrasts <- sort(unique(data$Contrast))[1:3]
    data <- data %>%
      filter(Contrast %in% uc_contrasts,
             TimingError == 0, Task== "RTTaskExp", ResponseTime=="Normal",
             ResponseError=="keypress_correct") %>%
      mutate(exp = paste(study_name, exp_num,Contrast, sep = '_')) %>%
      rename(idv = VP, iv = Congruency, RT = RT.Matlab)
  } else if(exp_num == '3') {
    data_fn <- list.files(paste(dir_name, 'data_Exp3',sep = .Platform$file.sep), 
                          pattern = 'om1203_rttask*', full.names = TRUE)
    data_fn_uc <- data_fn[1:4]
    data <- do.call(rbind, lapply(data_fn_uc, 
                                  function(fn) read.csv(fn, sep = ' ') %>% 
                                    mutate(exp = substr(fn[1], nchar(fn[1]) - 5, nchar(fn[1]) - 4))))
    
    # data_fn <- paste(dir_name, 'data_Exp3', 'om1203_rttask_10.dat', sep = .Platform$file.sep)
    data <- data %>%
      filter(!endsWith(block_type, 'Prac'),
             time_error=="Normal", key_error=="correct") %>%
      mutate(exp = paste(study_name, exp_num, 'Duration' ,exp ,sep = '_')) %>%
      rename(idv = sub, iv = congruency)
  }
  
  return (data %>%
            rename(dv = RT) %>%
            dplyr::select(idv, iv, exp, dv))
}
data <- do.call(rbind, lapply(exps, read_data_f))
data <- data[data$exp != 'Zerweck et al_2021_1',]

# validate against results in each section + Figures 4,6
data %>%
  group_by(exp, iv, idv) %>%
  summarise(m = mean(dv)) %>%
  group_by(exp, idv) %>%
  summarise(m = diff(m)) %>%
  group_by(exp) %>%
  summarise(effect = mean(m))

write.csv(data, paste0(study_name,'.csv'))