library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data<-read.spss(first(list.files(pattern = '.sav')), to.data.frame =TRUE)
#Variable "Subj" needs to be a factor to perform ezAnova-function
data$Subj<-as.factor(data$Subj)

#Remove all Subjects with an reactiontime beyond 1000ms
data$RT.cleaned<-data$RT
data$RT.cleaned[data$RT>1000]=NA
data.cleaned<-na.omit(data)
data <- data.cleaned %>% 
  rename(idv = Subj, iv = typisch, dv = RT) %>% 
  mutate(exp = 'Estes_etal_2008') %>%
  dplyr::select(idv,iv, dv,exp)

write.csv(data, 'Estes_etal_2008.csv')


library(weaknull)
p <- test_directional_effect(data, idv = 'idv', dv = 'dv', iv = 'iv')$p
2 * min(p,1-p)
ndir_p <- test_sign_consistency(data, idv = 'idv', dv = 'dv', iv = 'iv', null_dist_samples = 10^5)$p
ndir_p
