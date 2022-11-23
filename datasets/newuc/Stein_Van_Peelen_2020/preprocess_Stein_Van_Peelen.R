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
  mutate(target_loc = target_loc - 1) %>% # encoding target location as 1 / 0
  mutate(response = ifelse(loc_accu, target_loc, 1-target_loc)) %>% # encode response according to accuracy
  rename(idv = subj, iv = upright, iv2 = target_loc, dv = response) %>%
  mutate(exp = paste(study_name, exp_num, 'LOC', pres_time ,sep = '_')) %>% 
  dplyr::select(idv, iv, iv2, exp, dv)

data_exp3_pas <- data_exp3 %>% 
  filter(pres_time %in% uc_pres_times_exp3) %>%
  mutate(target_present = 2 - target_present) %>% # encoding target present /absent as 1 / 0
  mutate(response = ifelse(pas == 1, 0, 1)) %>% # encode pas as 1 / > 1 
  rename(idv = subj, iv = upright, iv2 = target_present, dv = response) %>%
  mutate(exp = paste(study_name, exp_num, 'PAS', pres_time ,sep = '_')) %>% 
  dplyr::select(idv, iv, iv2, exp, dv)

# get exp4b data
uc_pres_times_exp4b <- c(1) # 8
exp_num <- '4b'
data_exp4b <- data_exp4b %>% 
  filter(pres_time %in% uc_pres_times_exp4b) %>%
  mutate(target_loc = target_loc - 1) %>% # encoding target location as 1 / 0
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
  summarise(rate = 
              ifelse(count[1] == 0, 1/(2 * sum(count)), 
                     ifelse(count[1] == sum(count), (1 - 1 / (2 * sum(count))), 
                            count[1] / sum(count)))) %>%
  group_by(exp, idv, iv) %>% 
  summarise(d = (qnorm(rate[1]) - qnorm(rate[2])) / 2^.5) %>%
  group_by(exp, idv) %>% 
  summarise(d_diff = -diff(d))


########################################### UTILITY
get_participant_SDT_d <- function(mat, args = list(iv = 'iv', dv = 'dv')) {
  d <- as.data.frame(mat) %>%
    group_by(!!dplyr::sym(args$iv), !!dplyr::sym(args$dv), .drop = FALSE) %>%
    summarise(count = n(), .groups = 'drop_last') %>%
    ungroup() %>%
    complete(!!dplyr::sym(args$iv), !!dplyr::sym(args$dv), fill = list(count = 0)) %>% 
    group_by(!!dplyr::sym(args$iv)) %>% 
    summarise(rate = ifelse(count[1] == 0, 1 / (2*sum(count)), 
                            ifelse(count[1] == sum(count), 1- 1 / (2*sum(count)),
                                   (count[1]) / (sum(count)))), .groups = 'drop_last') %>%
    summarise(d = (qnorm(rate[1]) - qnorm(rate[2])), .groups = 'drop_last') %>%
    pull(d)
  return(d)
}

perm_test_subject <- function(mat, obs, summary_f, summary_f_args = list(iv = 'iv', dv = 'dv'), 
                              n_perm = 10^4, two.sided = TRUE) {
  inner_perm <- function(iteration, mat, summary_f, summary_f_args) {
    n_trials <- nrow(mat)
    mat[,summary_f_args$dv] <- mat[sample(n_trials),summary_f_args$dv]
    return (summary_f(mat, summary_f_args))
  }
  
  if('iv2' %in% summary_f_args) {
    resamp_f_args <- summary_f_args
    resamp_f_args$iv = resamp_f_args$iv2 
    resample_f <- function(iteration, mat) {
      summary_f(mat[sample(nrow(mat), replace = TRUE), ], resamp_f_args)
    }
    conds <- unique(mat[,summary_f_args$iv])
    iv1 <- sapply (1:n_perm, resample_f, mat=mat[mat[,summary_f_args$iv] == conds[1],])
    iv2 <- sapply (1:n_perm, resample_f, mat=mat[mat[,summary_f_args$iv] == conds[1],])
    null_dist <- sample(c(-1,1), n_perm, replace = TRUE) * (iv1 - iv2)  } 
  else {
  }
  p_value <- mean(obs < null_dist, na.rm=TRUE)
  if(two.sided) {p_value <- 2 * min(p_value, 1 - p_value)}
  return (p_value)
}

summary_f <- function(mat) {
  stein_sum_f_args <- list(iv = 'iv2', dv = 'dv')
  return (get_participant_SDT_d(mat, stein_sum_f_args))
}
test_f <- function(mat) {
  stein_sum_f_args <- list(iv = 'iv2', dv = 'dv')
  conds <- unique(mat$iv)
  obs_d_diff <- get_participant_SDT_d(mat[mat$iv == conds[1],], stein_sum_f_args) - 
    get_participant_SDT_d(mat[mat$iv == conds[2],], stein_sum_f_args)
  
  return(perm_test_subject(as.matrix(mat), obs_d_diff, get_participant_SDT_d,  summary_f_args =  
                             list(iv = 'iv', dv = 'dv', iv2 = 'iv2')))
}

########################################### UTILITY

# summary_f(data_exp3_loc[data_exp3_loc$idv == 1 & data_exp3_loc$iv == 1,])
res <- data_exp3_loc %>% group_by(idv) %>% 
  group_modify(~data.frame(p = test_f(.x)))
res <- data_exp4b %>% group_by(idv) %>% 
  group_modify(~data.frame(p = test_f(.x)))
res <- data_exp3_pas %>% group_by(idv) %>% 
  group_modify(~data.frame(p = test_f(.x)))