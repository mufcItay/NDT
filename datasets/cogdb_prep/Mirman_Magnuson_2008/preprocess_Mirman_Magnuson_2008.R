library(dplyr)
library(tidyverse)
library(foreign)
library(reshape2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data<-read.csv('replication_raw_data_processed.csv',header = TRUE)
data.long <- gather(data, SUBJECT,RT,setdiff(names(data), c('WORD','COND')))
data.long$SUBJECT <- gsub("X","",as.character(data.long$SUBJECT))
data.long$nearness <- 1
data.long$nearness[data.long$COND == 1 | data.long$COND == 2] <- 2

data.long$distance <- 1
data.long$distance[data.long$COND == 1 | data.long$COND == 3] <- 2

data.long <- transform(data.long, distance=factor(distance))
data.long <- transform(data.long, nearness=factor(nearness))

data.long <- data.long[!is.na(data.long$RT),]

data <- data.long %>% 
  rename(idv = SUBJECT, iv = nearness,dv = RT) %>% 
  mutate(exp = 'Mirman_Magnuson_2008') %>%
  dplyr::select(idv, iv, dv, exp)

write.csv(data, 'Mirman_Magnuson_2008.csv')

library(signcon)
p <-test_directional_effect(data, idv = 'idv', dv = 'dv', iv = 'iv')$p
2 * min(p,1-p)
test_sign_consistency(data, idv = 'idv', dv = 'dv', iv = 'iv')$p
