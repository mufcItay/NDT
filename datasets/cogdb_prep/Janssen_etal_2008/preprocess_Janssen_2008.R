library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data<-read.csv('data_for_analysis.csv',header = TRUE)  
data <- data %>% 
  rename(idv = Subject, iv = Relatedness, dv = RT) %>% 
  mutate(exp = 'Janssen_etal_2008') %>%
  dplyr::select(idv, iv, dv, exp)

write.csv(data, 'Janssen_etal_2008.csv')

library(signcon)
p <- test_directional_effect(data, idv = 'idv', dv = 'dv', iv = 'iv')$p
2 * min(p,1-p)
test_sign_consistency(data, idv = 'idv', dv = 'dv', iv = 'iv')$p
