library(dplyr)
library(readxl)
library(tidyr)

study_name <- 'Skora et al_2020'

#read exp1 data
data_exp1 <- read_xlsx(paste0('control - bvyf3', .Platform$file.sep, 'Full_long_incl.RT_02.07_100ms_CONTROL ONLY.xlsx'))
data_exp1 <- data_exp1 %>% 
  filter(subject_exclusions_Control == 0 & subject_exclusions == 0 & awareattrial == 0) %>%
  mutate(iv = ifelse(correct_resp, button_press, !button_press),
         exp = paste(study_name, '1',sep = '_')) %>% 
  rename(idv = subID, dv = button_press) %>%
  dplyr::select(idv, iv, exp, dv)


#read exp2 data
data_exp2 <- read_xlsx(paste0('drmcg', .Platform$file.sep, 'unconscious delay conditioning_Feb2020_full data.xlsx'))
data_exp2 <- data_exp2 %>% 
  filter(exclusions == 0 & trial_aware == 0 & !is.na(accuracy)) %>%
  mutate(go = 1-go, type = type -1) %>%
  rename(idv = pnum, iv = type, dv = go) %>%
  mutate(exp = paste(study_name, '2',sep = '_')) %>% 
  dplyr::select(idv, iv, exp, dv)

data <- rbind(data_exp1, data_exp2)
write.csv(data, 'Skora et al_2020.csv')
