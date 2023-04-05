library(dplyr)

data<-read.csv('data_for_analysis.csv',header = TRUE)  
data <- data %>% 
  rename(idv = Subject, iv = Relatedness, dv = RT) %>% 
  mutate(exp = 'Janssen_etal_2008') %>%
  dplyr::select(idv, iv, dv, exp)

write.csv(data, 'Janssen_etal_2008.csv')