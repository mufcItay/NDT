library(weaknull)
library(dplyr)

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)


#read exp3 data
data_exp3 <- read.csv("exp3all.csv", header=T)
#column names
colnames(data_exp3)[5:9] <- c('target_present', 'target_loc', 'pres_time','upright', 'target_exemplar')
#read exp4b data
data_exp4b <- read.csv("exp4ball.csv", header=T)
#column names
colnames(data_exp4b)[4:8] <- c('target_loc', 'pres_time','valid', 'target_cat', 'target_exemplar')


study_name <- 'Stein & van Peelen_2020'

# get exp3 data
exp_num <- '3'
uc_pres_times_exp3 <- c(1,2) # 8, 8(8)
data_exp3 <- data_exp3 %>% 
  filter(pres_time %in% uc_pres_times_exp3) %>%
  rename(idv = subj, iv = upright, dv = loc_accu) %>%
  mutate(exp = paste(study_name, exp_num, pres_time ,sep = '_')) %>% 
  select(idv, iv, exp, dv)

# get exp4b data
uc_pres_times_exp4b <- c(1) # 8
exp_num <- '4b'
data_exp4b <- data_exp4b %>% 
  filter(pres_time %in% uc_pres_times_exp4b) %>%
  rename(idv = subj, iv = valid, dv = loc_accu) %>%
  mutate(exp = paste(study_name, exp_num, pres_time ,sep = '_')) %>% 
  select(idv, iv, exp, dv)

#bind exp data
data <- rbind(data_exp3, data_exp4b)

write.csv(data, paste0(study_name,'.csv'))
