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
  filter(rt >= .1, rt <= 3, !idv %in% exc_participants, soc_condition != 'C')

data_response <- data %>%
  filter(soc_condition != 'C') %>%
  rename(dv =  response, iv2 = beeps, iv = soc_condition) %>%
  mutate(exp = paste('Battich_etal_2021', 'resp', target, sep = '_')) %>%
  dplyr::select(idv,iv,iv2,dv,exp)

data_rt <- data %>%
  filter(soc_condition != 'C') %>%
  rename(dv =  rt, iv2 = beeps, iv = soc_condition) %>%
  mutate(exp = paste('Battich_etal_2021', 'rt',target, sep = '_')) %>%
  dplyr::select(idv,iv,iv2,dv,exp)

data_all <- rbind(data_rt, data_response)
write.csv(data_all, 'Battich_etal_2021.csv')

library(weaknull)
get_effect_f <- function(mat, args = list(summary_f = mean, iv = 'iv2', dv = 'dv')) {
  mat <- as.data.frame(mat)
  mat$dv <- as.numeric(mat$dv)
  values <- mat %>% pull(dplyr::sym(args$iv))
  conds <- sort(unique(values))
  res <- args$summary_f(mat[values == conds[2],]$dv) - 
    args$summary_f(mat[values == conds[1],]$dv)
  return(res)
}

test_directional_effect(data_all %>% filter(exp == 'Battich_etal_2021_resp_1'), 
                        idv = 'idv', dv = c('iv2','dv'), iv = 'iv', 
                        summary_function = get_effect_f)
test_sign_consistency(data_all %>% filter(exp == 'Battich_etal_2021_resp_1'), 
                      idv = 'idv', dv = c('iv2','dv'), iv = 'iv', 
                        summary_function = get_effect_f)


test_sign_consistency(data_all %>% filter(exp == 'Battich_etal_2021_resp_1', idv != 20), 
                      idv = 'idv', dv = c('iv2','dv'), iv = 'iv', 
                      summary_function = get_effect_f, max_invalid_reps = 3)
