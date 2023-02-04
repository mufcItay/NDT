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
  summarise(rate = 1 -  
              ifelse(count[1] == 0, 1/(2 * sum(count)), 
                     ifelse(count[1] == sum(count), (1 - 1 / (2 * sum(count))), 
                            count[1] / sum(count)))) %>%
  group_by(exp, idv, iv) %>% 
  summarise(d = qnorm(rate[2]) - qnorm(rate[1])) %>%
  mutate(d = ifelse(grepl('PAS', exp, fixed = TRUE), d, d / 2^.5)) %>%
  group_by(exp, iv) %>% 
  summarise(md = mean(d, na.rm = TRUE))

org_wd <- getwd()
setwd('..//..//..//')
source('datasets_analysis//definitions_ucdb.R')
setwd(org_wd)
exp1_v1 <- data %>% filter(exp == 'Stein & van Peelen_2020_3_LOC_1',iv ==1)
exp1_v2 <- data %>% filter(exp == 'Stein & van Peelen_2020_3_LOC_1',iv ==0)
v1 <- exp1_v1 %>%
  group_by(idv)%>%
  group_modify(~data.frame(d=get_participant_SDT_d(.x,args = list(iv = 'iv2', dv = 'dv'))/2^.5))
v2 <- exp1_v2 %>%
  group_by(idv)%>%
  group_modify(~data.frame(d=get_participant_SDT_d(.x,args = list(iv = 'iv2', dv = 'dv'))/2^.5))


library(weaknull)
svp_args <- list(iv = 'iv2', dv = 'dv')
summary_f <- function(mat) {
  return (get_participant_SDT_d(mat, svp_args))
}

# exc
min_trials <- 5
exc <- data %>% filter(exp == 'Stein & van Peelen_2020_3_LOC_2') %>%
  mutate(unique_id = paste(exp, idv, sep = '_')) %>%
  group_by(unique_id, iv, iv2,dv) %>%
  summarise(n = n(), .groups = 'drop_last') %>%
  filter(n < min_trials) %>%
  pull(unique_id)
data_SVP2 <- data %>% filter(exp == 'Stein & van Peelen_2020_3_LOC_2') %>%
  filter(! paste(exp, idv, sep = '_') %in% exc)

res_de<-test_directional_effect(data_SVP2,
                        idv = 'idv', dv = c('iv2','dv'), iv = 'iv',
                        summary_function = summary_f,
                        null_dist_samples = 10^5)
res_sc <- test_sign_consistency(data_SVP2,
                        idv = 'idv', dv = c('iv2','dv'), iv = 'iv',
                        summary_function = summary_f,
                        null_dist_samples = 10^5)

test_f <- function(mat) {
  conds <- sort(unique(mat$iv))
  obs <- get_participant_SDT_d(mat[mat$iv == conds[1],], svp_args) - 
    get_participant_SDT_d(mat[mat$iv == conds[2],], svp_args)
  return(perm_test_subject(as.data.frame(mat), obs, get_participant_SDT_d,  summary_f_args =  
                             list(iv = 'iv', dv = 'dv', iv2 = 'iv2')))
}

res_pbt <- data_SVP2 %>% 
  group_by(idv) %>%
  group_modify(~data.frame(p = test_f(.x)))

res_pbt_p <- run_pbt(data_SVP2, test_f)[c('low','high','MAP')]


#################################
summary_f_null <- function(mat) {
  return (get_participant_SDT_d(mat, svp_args))
}
f <- function(ind, d) {
  d <- d %>%  group_by(idv) %>% mutate(iv=sample(iv))
  scores <- get_sign_consistency(d, idv = 'idv', dv = c('iv2','dv'), iv = 'iv',
                                 summary_function = summary_f_null)
  return(scores$consistency_per_id$score)
}
scs <- lapply(1:10,f, d= data_SVP2)
