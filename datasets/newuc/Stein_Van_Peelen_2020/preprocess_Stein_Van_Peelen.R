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

# get exp4b data
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
source('datasets_analysis//utils.R')
setwd(org_wd)
exp1_v1 <- data %>% filter(exp == 'Stein & van Peelen_2020_3_LOC_1',iv ==1)
exp1_v2 <- data %>% filter(exp == 'Stein & van Peelen_2020_3_LOC_1',iv ==0)
v1 <- exp1_v1 %>%
  group_by(idv)%>%
  group_modify(~data.frame(d=get_participant_SDT_d(.x,args = list(iv = 'iv2', dv = 'dv'))/2^.5))
v2 <- exp1_v2 %>%
  group_by(idv)%>%
  group_modify(~data.frame(d=get_participant_SDT_d(.x,args = list(iv = 'iv2', dv = 'dv'))/2^.5))

dfunc <- function(mat, args = list(iv = 'iv', dv = 'dv')) {
  mat <- as.data.frame(mat)
  conds <- sort(unique(mat[,args$iv]))
  calc_rate_nrom <- function(cnd, mat) {
    cnd_dat <- mat[mat[,args$iv] == cnd,]
    cnt <- sum(cnd_dat[,args$dv])
    len <- length(cnd_dat[,args$dv]) 
    rate <- ifelse(cnt == 0, 1 / (2*len), 
                   ifelse(cnt == len, 1- 1 / (2*len), cnt / len))
    return (qnorm(rate))
  }
  rate_norms <- sapply(conds, calc_rate_nrom, mat = mat)
  return (diff(rate_norms))
}
dfunc(exp1_v1%>%filter(idv == 1), args = list(iv = 'iv2', dv = 'dv')) / 2^.5
# 
# ## pas validation
# n_trials <- df_sdt %>% 
#   filter(grepl('PAS', exp, fixed = TRUE), iv2 == 0) %>%
#   group_by(exp, idv) %>%
#   summarise(n = sum(count))
# fa_rates <- df_sdt %>% 
#   filter(grepl('PAS', exp, fixed = TRUE), iv2 == 0, dv == 1) %>%
#   group_by(exp, idv) %>% 
#   summarise(count = sum(count)) %>%
#   left_join(y=n_trials, by=c("exp","idv")) %>%
#   summarise(rate = ifelse(count == 0, 1/(2 * n), 
#                           ifelse(count == n, (1 - 1 / (2 * n)), 
#                                  count / n))) %>% 
#   slice(rep(1:n(), each = 2))
# 
# 
# hit_rates <- df_sdt %>% 
#   filter(grepl('PAS', exp, fixed = TRUE), iv2 == 1 , dv == 1) %>%
#   group_by(exp, idv, iv) %>% 
#   left_join(y=n_trials, by=c("exp","idv")) %>%
#   summarise(rate = ifelse(count == 0, 1/(2 * n), 
#                           ifelse(count == n, (1 - 1 / (2 * n)), 
#                                  count / n)))
# df_d_pas <- hit_rates
# df_d_pas$d <- qnorm(df_d_pas$rate) - qnorm(fa_rates$rate) 
# df_d_pas %>%
#   group_by(exp, iv) %>% 
#   summarise(md = mean(d, na.rm = TRUE))
# 
# 
# 
# #####################
# calc_detect_d <- function(data) {
#   browser()
#   data %>%
#     group_by(iv2, dv, .drop = FALSE) %>%
#     summarise(count = n()) %>%
#     ungroup() %>%
#     complete(iv2, dv, fill = list(count = 0)) %>%
#     group_by(iv2, dv, .drop = FALSE) %>%
#     summarise(count = sum(count)) %>%
#     summarise(total = sum(count), n = count[2]) %>%
#     summarise(ninv_rate = qnorm(ifelse(n == 0, 1/(ifelse(iv2 ==0, 2,1) * total), 
#                                        ifelse(n == total, (1 - 1 / (ifelse(iv2 ==0, 2,1) * total)), 
#                                               n / total))))
# }
# 
# get_sub_data <- function(dat) {
#   return(df_sdt %>% filter(exp == dat$exp, iv == dat$idv, idv == dat$idv))
# }
# a <- sapply(1:nrow(df_d_pas), function(i) calc_detect_d(get_sub_data(df_d_pas[i,])))
# df_one <- df_sdt%>% filter(exp == 'Stein & van Peelen_2020_3_PAS_1', idv == 1)
# rates <- df_one %>%
#   group_by(iv2, dv, .drop = FALSE) %>%
#   summarise(count = sum(count)) %>%
#   summarise(total = sum(count), n = count[2]) %>%
#   summarise(ninv_rate = qnorm(ifelse(n == 0, 1/(2 * total), 
#                           ifelse(n == total, (1 - 1 / (2 * total)), 
#                                  n / total)))) %>%
#   summarise(d = diff(ninv_rate)) %>% 
#   pull(d)
# 
# 
# #########################
# rate_h_fa <- data %>%
#   filter(grepl('PAS', exp, fixed = TRUE)) %>%
#   na_if("") %>%
#   na.omit %>%
#   group_by(exp, iv, idv, iv2, dv, .drop = FALSE) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   complete(exp, idv, iv, iv2, dv,
#            fill = list(count = 0)) %>%
#   group_by(exp, idv, iv, iv2, dv, .drop = FALSE) %>%
#   summarise(count = sum(count)) %>%
#   summarise(total = sum(count), n = count[2])
# rate_h <- rate_h_fa %>%
#   filter(iv2 == 1) %>%
#   summarise(n = sum(n), total = sum(total))
# rate_fa <- rate_h_fa %>%
#   filter(iv2 == 0) %>%
#   group_by(exp,idv) %>%
#   summarise(n = sum(n), total = sum(total)) %>%
#   slice(rep(1:n(), each = 2))
# 
# %>%
#   summarise(ninv_rate = qnorm(ifelse(n == 0, 1/(ifelse(iv2 == 0, 2,1) * total), 
#                                      ifelse(n == total, (1 - 1 / (ifelse(iv2 == 0, 2,1) * total)), 
#                                             n / total)))) %>% 
#   summarise(d = diff(ninv_rate))
# 
