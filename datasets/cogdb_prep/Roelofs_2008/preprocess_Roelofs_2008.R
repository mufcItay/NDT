library(dplyr)

data <- read.table('Roelofs_clean.txt', header=T,sep="") %>% 
  filter(AnyError==0) %>% 
  mutate(exp = paste('Roelofs'), VoiceTask.RT = as.numeric(VoiceTask.RT)) %>%
  rename(idv = Subject, iv = Relatedness, iv2 = TypeOfRelation, dv = VoiceTask.RT) %>%
  dplyr::select(idv,iv,iv2, dv,exp)

write.csv(data, 'Roelofs_2008.csv')