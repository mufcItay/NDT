library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data<-read.csv('YapMu.csv',header = TRUE)
data <- data %>% 
  rename(idv = Subject, iv = Procedure, iv2 = word_NW,dv = Target.RT) %>%
  mutate(exp = 'Yap_etal_2008') %>%
  dplyr::select(idv, iv, iv2, dv, exp)

write.csv(data, 'Yap_etal_2008.csv')

library(signcon)
get_effect_f <- function(mat, args = list(summary_f = mean, iv = 'iv2', dv = 'dv')) {
  mat <- as.data.frame(mat)
  mat$dv <- as.numeric(mat$dv)
  values <- mat %>% pull(dplyr::sym(args$iv))
  conds <- sort(unique(values))
  res <- args$summary_f(mat[values == conds[2],]$dv) - 
    args$summary_f(mat[values == conds[1],]$dv)
  return(res)
}

p <- test_directional_effect(data, idv = 'idv', dv = c('iv2','dv'), iv = 'iv', 
                        summary_function = get_effect_f)$p
2 * min(p,1-p)
test_sign_consistency(data, idv = 'idv', dv = c('iv2','dv'), iv = 'iv', 
                        summary_function = get_effect_f)$p
