library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read the dataset
data <- read.csv('jaSifiData.csv')
subjs <- unique(data$subject)
data <- data %>%
  rename(idv = subject) %>%
  mutate(idv = as.numeric(factor(idv)))

# exclude low performing participants (as was done in Battich et al. 2021)
min_perf <- 1 - .35
exc_participants <- data %>%
  filter(beeps == target) %>%
  group_by(idv,target) %>%
  summarise(perf = mean(correct)) %>%
  filter(perf <= min_perf) %>%
  pull(idv)

# RT outliers (trial level) + low preforming ps (subj level)
data <- data %>%
  filter(rt >= .1, rt <= 3, !idv %in% exc_participants, soc_condition != 'C') %>%
  group_by(idv) %>%
  mutate(first = first(soc_condition), half = 2 - (soc_condition == first))

# get response data, preregistered analysis
data_response <- data %>%
  filter(soc_condition != 'C') %>%
  rename(dv =  response, iv2 = beeps, iv = soc_condition) %>%
  mutate(exp = paste('Battich_etal_2021', 'resp', target, sep = '_')) %>%
  dplyr::select(idv,iv,iv2,dv,exp, order, half)

# get RT data, exploratory analysis
data_rt <- data %>%
  filter(soc_condition != 'C') %>%
  rename(dv =  rt, iv2 = beeps, iv = soc_condition) %>%
  mutate(exp = paste('Battich_etal_2021', 'rt',target, sep = '_')) %>%
  dplyr::select(idv,iv,iv2,dv,exp,order, half)

# bind RT and response data together
data_all <- rbind(data_rt, data_response)
write.csv(data_all, 'Battich_etal_2021.csv')
# data_all$iv <- factor(data_all$iv)
# data_all$iv2 <- factor(data_all$iv2)
#' ####################
#' #' get_diffscore_f
#' #' the function calculates the difference score between two 'iv' conditions
#' #' @param mat a matrix of the condition labels (iv) and responses (dv) for each trial (rows).
#' #' @param args a list of parameter values for each column name and the summary function
#' #' @return the function returns the difference score between two conditions,
#' #' according to the summary function (for an interaction effect, use args$iv = 'iv2')
#' get_diffscore_f <- function(mat, args = list(summary_f = mean, iv = 'iv', dv = 'dv')) {
#'   mat <- as.data.frame(mat) %>% mutate(dv = as.numeric(dv))
#'   values <- mat %>% pull(dplyr::sym(args$iv))
#'   conds <- sort(unique(values))
#'   res <- args$summary_f(mat[values == conds[2],]$dv) - 
#'     args$summary_f(mat[values == conds[1],]$dv)
#'   return(res)
#' }
#' f <- function(mat) {
#'   interaction_args <- list(idv = 'idv', iv = 'iv', iv2 = 'iv2', dv = 'dv', 
#'                            summary_f = mean)
#'   args <- interaction_args
#'   args$iv <- 'iv2'
#'   get_diffscore_f(mat, args)
#' }
#' 
#' library(signcon)
res_direct_e1 <- test_directional_effect(data_response %>%
                                        filter(exp == 'Battich_etal_2021_resp_1'),
                        idv = 'idv', iv = 'iv', dv = c('dv','iv2'), summary_function = f)

#' effect_per_id_e1 <- res_direct_e1$effect_per_id$score
#' 
#' res_direct_e2 <- test_directional_effect(data_response %>% 
#'                                            filter(exp == 'Battich_etal_2021_resp_2'),
#'                                          idv = 'idv', iv = 'iv', dv = c('dv','iv2'), summary_function = f)
#' 
#' effect_per_id_e2 <- res_direct_e2$effect_per_id$score
#' 
#' #rt
#' rt_res_direct_e1 <- test_directional_effect(data_rt %>% 
#'                                            filter(exp == 'Battich_etal_2021_rt_1'),
#'                                          idv = 'idv', iv = 'iv', dv = c('dv','iv2'), summary_function = f)
#' 
#' rt_effect_per_id_e1 <- rt_res_direct_e1$effect_per_id$score
#' 
#' rt_res_direct_e2 <- test_directional_effect(data_rt %>% 
#'                                            filter(exp == 'Battich_etal_2021_rt_2'),
#'                                          idv = 'idv', iv = 'iv', dv = c('dv','iv2'), summary_function = f)
#' 
#' rt_effect_per_id_e2 <- rt_res_direct_e2$effect_per_id$score

# 
# # social condition order
# ord_per_id_e1 <- data %>% 
#   filter(target == 1, soc_condition == 'JA') %>% 
#   group_by(idv) %>%  
#   summarise(ord =first(order)) %>% 
#   pull(ord)
# ord_per_id_e2 <- data %>% 
#   filter(target == 2, soc_condition == 'JA') %>% 
#   group_by(idv) %>%  
#   summarise(ord =first(order)) %>% 
#   pull(ord)
# 
# cor.test(as.numeric(factor(ord_per_id_e1)),effect_per_id_e1)
# cor.test(as.numeric(factor(ord_per_id_e2)),effect_per_id_e2)
# cor.test(as.numeric(factor(ord_per_id_e1)),rt_effect_per_id_e1)
# cor.test(as.numeric(factor(ord_per_id_e2)),rt_effect_per_id_e2)


## test for difference in effects between halves
library(signcon)
compare_halves <- function(data, exp_label) {
  res_1st <- get_directional_effect(data %>% 
                                       filter(exp == exp_label, half == 1),
                                     idv = 'idv', iv = 'iv2', dv ='dv')
  scores_1st = res_1st$effect_per_id$score
  res_2nd <- get_directional_effect(data %>% 
                                       filter(exp == exp_label, half == 2),
                                     idv = 'idv', iv = 'iv2', dv ='dv')
  scores_2nd = res_2nd$effect_per_id$score
  return(t.test(scores_1st, scores_2nd,paired = TRUE))
}
resp1_halves_comp <- compare_halves(data_all, 'Battich_etal_2021_resp_1')
resp2_halves_comp <- compare_halves(data_all, 'Battich_etal_2021_resp_2')
rt1_halves_comp <- compare_halves(data_all, 'Battich_etal_2021_rt_1')
rt2_halves_comp <- compare_halves(data_all, 'Battich_etal_2021_rt_2')



res_direct_e1 <- test_sign_consistency(data_response %>%
                                         filter(exp == 'Battich_etal_2021_rt_1'),
                                       idv = 'idv', iv = 'iv', dv = c('dv','iv2'), summary_function = f)