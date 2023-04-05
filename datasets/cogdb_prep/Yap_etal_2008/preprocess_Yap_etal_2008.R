library(dplyr)

data<-read.csv('YapMu.csv',header = TRUE)
data <- data %>% 
  rename(idv = Subject, iv = Procedure, iv2 = word_NW,dv = Target.RT) %>%
  mutate(exp = 'Yap_etal_2008') %>%
  dplyr::select(idv, iv, iv2, dv, exp)

write.csv(data, 'Yap_etal_2008.csv')