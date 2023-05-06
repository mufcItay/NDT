library(dplyr)
library(tidyr)

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
data_exp3_loc <- data_exp3 %>% 
  filter(pres_time %in% uc_pres_times_exp3, target_present == 1) %>%
  mutate(target_loc = 2 - target_loc) %>% # encoding target location as 1 / 0
  mutate(upright = 2 - upright) %>% # reencoding upright as 1 (upright) / 0 (inverted)
  mutate(response = ifelse(loc_accu, target_loc, 1-target_loc)) %>% # encode response according to accuracy
  rename(idv = subj, iv = upright, iv2 = target_loc, dv = response) %>%
  mutate(exp = paste(study_name, exp_num, 'LOC', pres_time ,sep = '_')) %>% 
  dplyr::select(idv, iv, iv2, exp, dv)

data_exp3_pas <- data_exp3 %>% 
  filter(pres_time %in% uc_pres_times_exp3) %>%
  mutate(target_present = 2 - target_present) %>% # encoding target present /absent as 1 / 0
  mutate(upright = 2 - upright) %>% # reencoding upright as 1 (upright) / 0 (inverted)
  mutate(response = ifelse(pas > 1, 1, 0)) %>% # encode pas as 1 / > 1 
  rename(idv = subj, iv = upright, iv2 = target_present, dv = response) %>%
  mutate(exp = paste(study_name, exp_num, 'PAS', pres_time ,sep = '_')) %>% 
  dplyr::select(idv, iv, iv2, exp, dv)
fas_addition <- data_exp3_pas %>% 
  filter(iv2 == 0) %>% 
  mutate(iv = 1-iv)
# since this is a detection task, there is no difference in iv in target absent trials
data_exp3_pas <- rbind(data_exp3_pas, fas_addition)

uc_pres_times_exp4b <- c(1) # 8
exp_num <- '4b'
data_exp4b <- data_exp4b %>% 
  filter(pres_time %in% uc_pres_times_exp4b) %>%
  mutate(target_loc = target_loc - 1) %>% # encoding target location as 1 / 0
  mutate(valid = 2 - valid) %>% # reencoding valid as 1 (valid) / 0 (invalid)
  mutate(response = ifelse(loc_accu, target_loc, 1-target_loc)) %>%
  rename(idv = subj, iv = valid, iv2 = target_loc, dv = response) %>%
  mutate(exp = paste(study_name, exp_num, pres_time ,sep = '_')) %>% 
  dplyr::select(idv, iv, iv2, exp, dv)

#bind exp data
data <- rbind(data_exp3_loc, data_exp3_pas, 
              data_exp4b)

write.csv(data, paste0(study_name,'.csv'))


