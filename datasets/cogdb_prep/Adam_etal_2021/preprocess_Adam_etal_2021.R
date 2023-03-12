library(tidyr)
library(dplyr)
library(R.matlab)
library(reshape2)
library(foreign)

dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)
expSep = '_'

get_exp_data <- function(exp, exp_label) {
  file_list <- list.files(exp, pattern = ".csv", full.names = TRUE)
  data <- do.call(rbind,lapply(file_list,FUN=function(files) {
    read.csv(files,header=T)
  }))
  return(data %>% filter(rt>=.2) %>% 
           rename(idv = subject, iv = distractor, dv = rt) %>% 
           mutate(exp = paste(exp_label, set_size, sep = '_'),
                  dv = dv * 1000))
}
# exp 1
data_1a <- get_exp_data('1a',"Adam_1a_VHe")
data_1b <- get_exp_data('1b',"Adam_1b_CHe")
data_1c <- get_exp_data('1c',"Adam_1c_VHo")
data_1d <- get_exp_data('1d',"Adam_1d_CHo")

# exp 2a
data_2a_Ho <- get_exp_data('2a',"Adam_2a_VHo") %>%
  filter(shape_type == 'homogeneous')
data_2a_He <- get_exp_data('2a',"Adam_2a_VHe") %>%
  filter(shape_type == 'heterogeneous')

# exp 2b
data_2b_Ho <- get_exp_data('2b',"Adam_2b_CHo") %>%
  filter(shape_type == 'homogeneous')
data_2b_He <- get_exp_data('2b',"Adam_2b_CHe") %>%
  filter(shape_type == 'heterogeneous')

# exp 3a
data_3a_IC <- get_exp_data('3a',"Adam_3a_IC") %>%
  filter(color_history == 'constant')
data_3a_IV <- get_exp_data('3a',"Adam_3a_IV") %>%
  filter(color_history == 'variable')

# exp 3b
data_3b_GC <- get_exp_data('3b',"Adam_3b_GC") %>%
  filter(color_history == 'constant')
data_3b_GV <- get_exp_data('3b',"Adam_3b_GV") %>%
  filter(color_history == 'variable')

data_all <- do.call(rbind,lapply(list(data_1a,data_1b, data_1c, data_1d,
                                      data_2a_He, data_2a_Ho,
                                      data_2b_He, data_2b_Ho,
                                      data_3a_IC, data_3a_IV,
                                      data_3b_GC, data_3b_GV), 
                                 function(d) d %>% dplyr::select(idv, iv, dv, exp)))
write.csv(data_all, 'Adam_et_al_2021.csv')

library(signcon)
res <- data_all %>%
  group_by(exp) %>%
  group_modify(~data.frame(dt = 
    test_directional_effect(.x, idv = 'idv', iv = 'iv', dv = 'dv')[c('statistic','p')])) %>%
  mutate(dt.p = 2 * min(dt.p, 1-dt.p))