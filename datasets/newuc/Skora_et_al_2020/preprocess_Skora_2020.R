library(weaknull)
library(dplyr)
library(readxl)
library(psycho)
set.seed(121)

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)


study_name <- 'Skora et al_2020'

#read exp1 data
data_exp1 <- read_xlsx("control - bvyf3\\Full_long_incl.RT_02.07_100ms_CONTROL ONLY.xlsx")
data_exp1 <- data_exp1 %>% 
  filter(subject_exclusions_Control == 0 & subject_exclusions == 0 & awareattrial == 0) %>%
  rename(idv = subID, iv = correct_resp, dv = button_press) %>%
  mutate(exp = paste(study_name, '1',sep = '_')) %>% 
  select(idv, iv, exp, dv)


#read exp2 data
data_exp2 <- read_xlsx("drmcg\\unconscious delay conditioning_Feb2020_full data.xlsx")
data_exp2 <- data_exp2 %>% 
  filter(exclusions == 0 & trial_aware == 0 & !is.na(accuracy)) %>%
  mutate(go = 1-go, type = type -1) %>%
  rename(idv = pnum, iv = type, dv = go) %>%
  mutate(exp = paste(study_name, '2',sep = '_')) %>% 
  select(idv, iv, exp, dv)

data <- rbind(data_exp1, data_exp2)
write.csv(data, 'Skora et al_2020.csv')

# to calculate d'
f_norm_rate <- function(var) {
  rate <- (sum(var) + .1) / (length(var) + .1)
  return (qnorm(rate))
}

