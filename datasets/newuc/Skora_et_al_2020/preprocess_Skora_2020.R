library(weaknull)
library(dplyr)
library(readxl)
library(tidyr)
set.seed(121)

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)


study_name <- 'Skora et al_2020'

#read exp1 data
data_exp1 <- read_xlsx("control - bvyf3\\Full_long_incl.RT_02.07_100ms_CONTROL ONLY.xlsx")
data_exp1 <- data_exp1 %>% 
  filter(subject_exclusions_Control == 0 & subject_exclusions == 0 & awareattrial == 0) %>%
  mutate(iv = ifelse(correct_resp, button_press, !button_press),
         exp = paste(study_name, '1',sep = '_')) %>% 
  rename(idv = subID, dv = button_press) %>%
  dplyr::select(idv, iv, exp, dv)


#read exp2 data
data_exp2 <- read_xlsx("drmcg\\unconscious delay conditioning_Feb2020_full data.xlsx")
data_exp2 <- data_exp2 %>% 
  filter(exclusions == 0 & trial_aware == 0 & !is.na(accuracy)) %>%
  mutate(go = 1-go, type = type -1) %>%
  rename(idv = pnum, iv = type, dv = go) %>%
  mutate(exp = paste(study_name, '2',sep = '_')) %>% 
  dplyr::select(idv, iv, exp, dv)

data <- rbind(data_exp1, data_exp2)
write.csv(data, 'Skora et al_2020.csv')

# to calculate d'
f_norm_rate <- function(var) {
  rate <- (sum(var) + .1) / (length(var) + .1)
  return (qnorm(rate))
}

# validate against d' results at the group level in the paper in both experiments
df_sdt <- data %>%
  na_if("") %>%
  na.omit %>%
  group_by(exp, idv, iv, dv, .drop = FALSE) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  complete(exp, idv, iv, dv,
           fill = list(count = 0))

df_d <- df_sdt %>% group_by(exp, idv, iv) %>% 
  # summarise(rate = (count[1]) / (sum(count))) %>%
  summarise(rate =
              ifelse(count[1] == 0, 1/(2 * sum(count)),
                     ifelse(count[1] == sum(count), (1 - 1 / (2 * sum(count))),
                            count[1] / sum(count)))) %>%
  group_by(exp, idv) %>% 
  summarise(d = (qnorm(rate[1]) - qnorm(rate[2])))

mean(df_d[df_d$exp == 'Skora et al_2020_1' & !is.nan(df_d$d),]$d, na.rm = TRUE)
length(df_d[df_d$exp == 'Skora et al_2020_1' & !is.nan(df_d$d),]$d)
mean(df_d[df_d$exp == 'Skora et al_2020_2' & !is.nan(df_d$d),]$d, na.rm = TRUE)
length(df_d[df_d$exp == 'Skora et al_2020_2' & !is.nan(df_d$d),]$d)

summary_f <- function(mat) {
  cnt <- sum(mat[,'dv'])
  len <- length(mat[,'dv']) 
  rate <- ifelse(cnt == 0, 1 / (2*len), 
                 ifelse(cnt == len, 1- 1 / (2*len), cnt / len))
  return (qnorm(rate))
}


get_directional_effect(data %>% mutate(iv2 = 11) %>%filter(exp == 'Skora et al_2020_1'), 
                       idv = 'idv', iv = 'iv', dv = c('dv','iv2'), 
                       summary_function =summary_f )
test_sign_consistency(data %>% mutate(iv2 = 11) %>%filter(exp == 'Skora et al_2020_1'), 
                       idv = 'idv', iv = 'iv', dv = c('dv','iv2'), 
                       summary_function =summary_f)
