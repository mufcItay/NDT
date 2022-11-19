library(weaknull)
library(dplyr)
library(tidyr)

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
  filter(pres_time %in% uc_pres_times_exp3, target_present == 1) %>%
  mutate(response = ifelse(loc_accu, target_loc, 2-target_loc)) %>%
  rename(idv = subj, iv = upright, iv2 = target_loc, dv = response) %>%
  mutate(exp = paste(study_name, exp_num, pres_time ,sep = '_')) %>% 
  select(idv, iv, iv2, exp, dv)

# get exp4b data
uc_pres_times_exp4b <- c(1) # 8
exp_num <- '4b'
data_exp4b <- data_exp4b %>% 
  filter(pres_time %in% uc_pres_times_exp4b) %>%
  mutate(response = ifelse(loc_accu, target_loc, 3-target_loc)) %>%
  rename(idv = subj, iv = valid, iv2 = target_loc, dv = response) %>%
  mutate(exp = paste(study_name, exp_num, pres_time ,sep = '_')) %>% 
  select(idv, iv, iv2, exp, dv)

#bind exp data
data <- rbind(data_exp3, data_exp4b)

write.csv(data, paste0(study_name,'.csv'))


# validate against the matlab code in OSF and Fig 4d (for exp3) and Fig 5d (for exp 4b) 
df_sdt <- data %>%
  na_if("") %>%
  na.omit %>%
  group_by(exp, iv, idv, iv2, dv, .drop = FALSE) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  complete(exp, iv, idv, iv2, dv,
           fill = list(count = 0))


df_d <- df_sdt %>% group_by(exp, idv, iv, iv2) %>% 
  summarise(rate = 
              ifelse(count[1] == 0, 1/(2 * sum(count)), 
                     ifelse(count[1] == sum(count), (1 - 1 / (2 * sum(count))), 
                            count[1] / sum(count)))) %>%
  group_by(exp, idv, iv) %>% 
  summarise(d = (qnorm(rate[1]) - qnorm(rate[2])) / 2^.5)