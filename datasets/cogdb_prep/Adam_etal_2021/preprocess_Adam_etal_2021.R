library(tidyr)
library(dplyr)
library(R.matlab)
library(reshape2)
library(foreign)

dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)
expSep = '_'

get_exp_data <- function(exp, exp_label) {
  studyName <- "1a_Variable_Heterogeneous"
  file_list <- list.files(exp, pattern = ".csv", full.names = TRUE)
  data <- do.call(rbind,lapply(file_list,FUN=function(files) {
    read.csv(files,header=T)
  }))
  return(data %>% filter(rt>=.2) %>% 
           rename(idv = subject, iv = distractor, dv = rt) %>% 
           mutate(exp = paste(exp_label, set_size, sep = '_')))
}
# exp 1a
data_1a <- get_exp_data('1a',"Adam_1a_VHo")
# exp 1d
data_1d <- get_exp_data('1d',"Adam_1a_CHe")
# exp 2a
data_2a <- get_exp_data('2a',"Adam_2a_VHe") %>%
  filter(shape_type == 'heterogeneous')
# exp 2d
data_2d <- get_exp_data('2b',"Adam_2a_CHo") %>%
  filter(shape_type == 'homogeneous')
# exp 3b
data_3b <- get_exp_data('3b',"Adam_3b_GC") %>%
  filter(color_history == 'constant')
data_all <- do.call(rbind,lapply(list(data_1a,data_1d,data_2a, data_2d, data_3b), 
                                 function(d) d %>% dplyr::select(idv, iv, dv, exp)))
write.csv(data_all, 'Adam_et_al_2021.csv')

library(weaknull)
res <- test_directional_effect(data_all %>% filter(exp == "Adam_1a_VHo_3"), 
                        idv = 'idv', iv = 'iv', dv = 'dv')
2 * min(res$p, 1-res$p)