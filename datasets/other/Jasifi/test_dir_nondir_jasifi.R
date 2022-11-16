library(weaknull)
library(dplyr)

set.seed(121)

print_res <- function(title, results) {
  print(paste('+++++', title ,'++++++++'))
  print(paste('Directional p = ', results$dir_p))
  print(paste('Non-Directional p = ', results$nondir_p))
}

get_sim_res <- function(data, dv, f, n_null_samples = 10^6) {
  dir_sum <- get_directional_effect(data, idv = 'SubNum', dv = c('beeps',dv), iv = 'soc_condition',
                                               summary_function = f) 
  dir_tst <- test_directional_effect(data, idv = 'SubNum', dv = c('beeps',dv), iv = 'soc_condition',
                                            summary_function = f, null_dist_samples = n_null_samples)
  dir_p <- 2 * min(1- dir_tst$p, dir_tst$p)
  
  nondir_sum <- get_sign_consistency(data, idv = 'SubNum', dv = c('beeps',dv), iv = 'soc_condition',
                                                summary_function = f) 
  nondir_tst <- test_sign_consistency(data, idv = 'SubNum', dv = c('beeps',dv), iv = 'soc_condition',
                                             summary_function = f, null_dist_samples = n_null_samples)
  nondir_p <- nondir_tst$p
  return(list(dir_sum = dir_sum, dir_p = dir_p, nondir_sum = nondir_sum,nondir_p = nondir_p))
}

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)

# read the dataset
data <- read.csv('jaSifiData.csv')
subjs <- unique(data$subject)
data <- data %>%
  rename(SubNum = subject) %>%
  mutate(SubNum = as.numeric(factor(SubNum)))

# exclude low performing participants (as was done in Battich et al. 2021)
min_perf <- 1 - .35
exc_participants <- data %>%
  filter(beeps == target) %>%
  group_by(SubNum,target) %>%
  summarise(perf = mean(correct)) %>%
  filter(perf <= min_perf) %>%
  pull(SubNum)
# RT outliers (trial level) + low preforming ps (subj level)
data <- data %>%
  filter(rt >= .1, rt <= 3, !SubNum %in% exc_participants)

all_data_cleaned <- data
data <- data %>%
  filter(soc_condition != 'C')

# Preregistered (Main) analysis (resp flashes by beeps X soc_condition - ns interaction (p = .09))
# interaction summary function
f_int <- function(mat) {
  res <- mean(mat[mat[,'beeps'] == 2, 'response']) - 
    mean(mat[mat[,'beeps'] == 1, 'response'])
  return (res)
}
res_fission <- get_sim_res(data %>% filter(target == 1), dv = 'response', f= f_int)
res_fusion <- get_sim_res(data %>% filter(target == 2), dv = 'response', f= f_int)
print_res('Fission Flash Count Analysis', res_fission)
print_res('Fusion Flash Count Analysis', res_fusion)

# Exploratory analysis - RT (significant in the Battich et al. 2021)
# here we use medians (to mimic what we did in the datasets analysis for RT dv)
f_int_rt <- function(mat) {
  res <- median(mat[mat[,'beeps'] == 2, 'rt']) - 
    median(mat[mat[,'beeps'] == 1, 'rt'])
  return (res)
}
res_fission_rt <- get_sim_res(data %>% filter(target == 1), dv = 'rt', f= f_int_rt)
res_fusion_rt <- get_sim_res(data %>% filter(target == 2), dv = 'rt', f= f_int_rt)
print_res('Fission RT Analysis', res_fission_rt)
print_res('Fusion RT Analysis', res_fusion_rt)


############################################################
# control condition analysis
############################################################
ja_c_data <- all_data_cleaned %>% filter(soc_condition != 'I')
i_c_data <- all_data_cleaned %>% filter(soc_condition != 'JA')

res_fission_ja_c <- get_sim_res(ja_c_data %>% filter(target == 1), dv = 'response', f= f_int)
res_fusion_ja_c <- get_sim_res(ja_c_data %>% filter(target == 2), dv = 'response', f= f_int)
res_fission_i_c <- get_sim_res(i_c_data %>% filter(target == 1), dv = 'response', f= f_int)
res_fusion_i_c <- get_sim_res(i_c_data %>% filter(target == 2), dv = 'response', f= f_int)

print_res('Fission Control (JA vs.C) Analysis', res_fission_ja_c)
print_res('Fusion Control (JA vs.C) Analysis', res_fusion_ja_c)
print_res('Fission Control (I vs.C) Analysis', res_fission_i_c)
print_res('Fusion Control (I vs.C) Analysis', res_fusion_i_c)
