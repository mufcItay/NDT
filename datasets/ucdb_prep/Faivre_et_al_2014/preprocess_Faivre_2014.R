rm(list=ls(all=T)); # clear workspace
# Load libraries
library(R.matlab)
library(dplyr)
library(tidyverse)
library(data.table)


IDENTICAL_LBL <- 'identical'
DIFFERENT_LBL <- 'different'


get_Faivre_data <- function(exp = 0, relatedness = 'identical') {
  chancePerformance <- 1/2
  expFiles <- dir(paste0('data\\Exp',exp), full.names = T)
  # recode relatedness to fit with the actual labels in the data
  relatedness <- ifelse(relatedness == IDENTICAL_LBL,'cong', 
                        ifelse(relatedness == DIFFERENT_LBL,'incong', 'INVALID'))
  # read first file, then the rest in a loop
  data <- data.table(do.call(rbind, lapply(expFiles, read.table, header = F)))
  # assign column names
  names(data) <- c('initials','subNum','trialNum','primeCon','primeTargetRelCon','auditoryPrimeUnMasked','visualPrimeUnMasked','auditoryPrimeStim','auditoryTarStim','visualPrimeStim','visualTarStim','tarResponse','tarRT','primeResponse','primeRT' )
  
  # recode subject ID to make it unique
  data <- data %>%
    mutate(subNum = paste(subNum,initials)) %>%
    # reencode trial numbers to exclude only the first 5 trials
    group_by(subNum) %>% mutate(codedTNum = row_number())
  
  #get unconscious trails + remove 5 first trials (were considered training)
  #in experiment 2 the auditory prime was masked, 
  #in the other experiments the conscious/unconscious split was according to the visual prime
  if(exp == 2) {
    data <- subset(data,auditoryPrimeUnMasked ==0 & trialNum >5)
  } else {
    data <- subset(data,visualPrimeUnMasked ==0 & trialNum >5)
  }
    
  # create a 'tarCon' column for target congruency, similar to rel/unrel column
  data <- data %>%
    mutate(tarCon = ifelse(visualTarStim == auditoryTarStim, 'cong','incong'))
  # create an 'objt' column for accuracy in target congruency response
  data <- data %>%
    mutate(objt = ifelse((((tarCon  == 'cong') & (tarResponse == 1)) | ((tarCon == 'incong') & (tarResponse == 0))),1,0))
  # create an 'objp' column for accuracy in prime congruency response
  data <- data %>%
    mutate(objp = ifelse((((primeCon  == 'cong') & (primeResponse == 1)) | ((primeCon == 'incong') & (primeResponse == 0))),1,0))
  
  # remove extreme RT trials
  data <- subset(data, !((tarRT>4000 | tarRT<300)))
  data$primeTargetRelCon <- ifelse(data$primeCon == data$tarCon,'rel','unrel')
  # summarize the accuracy for every subject for both prime and target congruecy responses
  # find out which of the subjects performed above 65%
  sum_subj_unCon <- data %>% group_by(subNum) %>% summarise(accP = mean(objp), accT = mean(objt))
  over_performers_subs <- getOver65PTAccuracy(sum_subj_unCon)
  
  data <- data %>% 
    filter(objt==1, ! subNum %in% over_performers_subs, tarCon == relatedness) %>%
    mutate(exp = paste('Faivre et al.', exp, relatedness, sep = '_')) %>%
    rename(idv = subNum, iv = primeTargetRelCon, dv = tarRT) %>%
    dplyr::select(idv,iv,dv,exp)
  return (data)  
}

getOver65PTAccuracy <- function(data, threshold = 0.65) {
  # - > 65% performance
  high_perf <- subset(data, accP > threshold)
  high_perf_subjects <- unique(high_perf$subNum)
  return (high_perf_subjects)
}
  

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
all_exps <- rbind(get_Faivre_data(1, IDENTICAL_LBL), get_Faivre_data(1, DIFFERENT_LBL),
                  get_Faivre_data(2, IDENTICAL_LBL), get_Faivre_data(2, DIFFERENT_LBL),
                  get_Faivre_data(3, IDENTICAL_LBL), get_Faivre_data(3, DIFFERENT_LBL),
                  get_Faivre_data(4, IDENTICAL_LBL), get_Faivre_data(4, DIFFERENT_LBL))
write.csv(all_exps, file = 'Faivre et al._2014.csv')


a <- all_exps %>%
  mutate(dv=log(dv)) %>%
  group_by(exp) %>%
  group_modify(~data.frame(test_directional_effect(
    .x, idv = 'idv', iv = 'iv', dv = 'dv', 
    summary_function = mean, null_dist_samples = 10000)[c('statistic','p')]))
a$statistic <- exp(a$statistic)
a

all_exps %>%
  mutate(dv=log(dv)) %>%
  group_by(exp, idv, iv) %>% summarise(m = mean(dv)) %>% group_by(exp,idv) %>%
  summarise(m = diff(exp(m))) %>% group_by(exp) %>% summarise(m = mean(m))
