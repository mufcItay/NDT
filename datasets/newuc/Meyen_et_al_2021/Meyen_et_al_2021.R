library(weaknull)
library(dplyr)

set.seed(121)

get_sim_res <- function(data, n_null_samples = 10^4) {
  dir_sum <- get_directional_effect(data, idv = 'SubNum', dv = 'rt', iv = 'iv') 
  dir_tst <- test_directional_effect(data, idv = 'SubNum', dv = 'rt', iv = 'iv', 
                                     null_dist_samples = n_null_samples)
  dir_p <- 2 * min(1- dir_tst$p, dir_tst$p)
  
  nondir_sum <- get_sign_consistency(data, idv = 'SubNum', dv = 'rt', iv = 'iv') 
  nondir_tst <- test_sign_consistency(data, idv = 'SubNum', dv = 'rt', iv = 'iv', 
                                      null_dist_samples = n_null_samples)
  nondir_p <- nondir_tst$p
  return(list(dir_sum = dir_sum, dir_p = dir_p, nondir_sum = nondir_sum,nondir_p = nondir_p))
}

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)

# read the dataset
data_dir <- 'data'
exps <- list.dirs(data_dir,recursive = FALSE)
read_data_f <- function(dir_name) {
  exp_num <- substring(dir_name,  nchar(dir_name), nchar(dir_name))
  if(exp_num == '1') {
    data_fn <- list.files(dir_name, pattern = '*ndirectTask.dat', full.names = TRUE)
    data <- read.csv(data_fn, sep = ' ') %>%
      rename(SubNum = sub, iv = congruency) %>%
      filter(time_error =="Normal",
        key_error == 'correct') %>%
      mutate(expName = paste('Zwereck', exp_num, sep = '_'))
  } else if(exp_num == '2') {
    data_fn <- list.files(dir_name, pattern = '*ndirectTask.dat', full.names = TRUE)
    data <- read.csv(data_fn, sep = ' ')
    uc_contrasts <- sort(unique(data$Contrast))[1:2]
    data <- data %>%
      filter(Contrast %in% uc_contrasts,
             TimingError == 0, Task== "RTTaskExp", ResponseTime=="Normal",
             ResponseError=="keypress_correct") %>%
      mutate(expName = paste('Zwereck', exp_num, 'SOA' ,Contrast, sep = '_')) %>%
      rename(SubNum = VP, iv = Congruency, RT = RT.Matlab)
  } else if(exp_num == '3') {
    data_fn <- paste(dir_name, 'data_Exp3', 'om1203_rttask_10.dat', sep = .Platform$file.sep)
    data <- read.csv(data_fn, sep = ' ') %>%
      filter(!endsWith(block_type, 'Prac'),
             time_error=="Normal", key_error=="correct") %>%
      mutate(expName = paste('Zwereck', exp_num, 'Duration' ,'10' ,sep = '_')) %>%
      rename(SubNum = sub, iv = congruency)
  }
  else {
    data_fn <- paste(dir_name, 'om1601_rttask_all.dat', sep = .Platform$file.sep)
    data <- read.csv(data_fn, sep = ' ') %>%
      filter(time_error=="Normal", key_error=="correct") %>%
      mutate(expName = 'Meyen_2021') %>%
      rename(SubNum = sub, iv = congruency)
  }
  
  return (data %>%
            rename(rt = RT) %>%
            select(SubNum, iv, expName, rt))
}
data <- do.call(rbind, lapply(exps, read_data_f))
write.csv(data, 'Numberical_Priming.csv')
# res_exp1 <- get_sim_res(data %>% filter(expName == '1'))
# res_exp1
# res_exp2_1 <- get_sim_res(data %>% filter(expName == '2_SOA_20'))
# res_exp2_1
# res_exp2_2 <- get_sim_res(data %>% filter(expName == '2_SOA_29'))
# res_exp2_2
# res_exp3 <- get_sim_res(data %>% filter(expName == '3'))
# res_exp3
# res_exp3 <- get_sim_res(data %>% filter(expName == '4'))
# res_exp3