library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.table('Roelofs_clean.txt', header=T,sep="") %>% 
  filter(AnyError==0) %>% 
  mutate(exp = paste('Roelofs'), VoiceTask.RT = as.numeric(VoiceTask.RT)) %>%
  rename(idv = Subject, iv = Relatedness, iv2 = TypeOfRelation, dv = VoiceTask.RT) %>%
  dplyr::select(idv,iv,iv2, dv,exp)

write.csv(data, 'Roelofs_2008.csv')

library(weaknull)
get_interaction_effect_f <- function(mat, args = list(summary_f = mean, iv = 'iv2', dv = 'dv')) {
  mat <- as.data.frame(mat)
  mat$dv <- as.numeric(mat$dv)
  values <- mat %>% pull(dplyr::sym(args$iv))
  conds <- sort(unique(values))
  res <- args$summary_f(mat[values == conds[2],]$dv) - 
    args$summary_f(mat[values == conds[1],]$dv)
  if(is.na(res)) {
    print(res)
  }
  return(res)
}

# p <- test_directional_effect(data, idv = 'idv', dv = c('iv2','dv'), iv = 'iv', 
#                         summary_function = get_interaction_effect_f)$p
# 2 * min(p,1-p)
test_sign_consistency(data, idv = 'idv', dv = c('iv2','dv'), iv = 'iv', 
                        summary_function = get_interaction_effect_f)$p
