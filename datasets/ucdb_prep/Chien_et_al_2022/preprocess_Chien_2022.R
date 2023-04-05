library(dplyr)
library(readxl)

read_full_sheet <- function(fn) {
  sheets <- excel_sheets(fn)
  exp_name <- substr(fn,4,5)
  # we code iv = oddity(condition) because conditions are (cold-cold, cold-warm, warm-warm, warm-cold)
  dat <- read_excel(fn, sheet = sheets[length(sheets)]) %>%
    mutate(exp = exp_name, iv = condition %% 2, rt = rt * 1000) %>%
    rename(idv = no., dv = rt)
    
  # from the excel sheets, the included participants were manually extracted
  exp2a_inc <- c('s1','s12','s13','s14','s15','s16','s18','s20','s21','s22','s24','s25','s28','s29','s3','s30','s32','s33','s35','s37','s5','s6','s8','s9')
  exp2b_inc <- c('s15', 's18', 's19', 's22', 's23', 's24', 's25', 's28', 's29', 's31', 's32', 's33', 's35', 's36', 's40', 's42', 's43', 's44', 's45', 's47', 's49', 's60 ', 's62', 's64')
  exp2c_inc <- c('s1', 's12', 's13', 's14', 's15', 's16', 's18', 's20', 's21', 's22', 's24', 's25', 's28', 's29', 's3', 's30', 's32', 's33', 's35', 's37', 's5', 's6', 's8', 's9')
  
  if (exp_name == '2A') {dat <- dat %>% filter(sub. %in% exp2a_inc)} else
    if (exp_name == '2B') {dat <- dat %>% filter(sub. %in% exp2b_inc)} else
      if (exp_name == '2C') {dat <- dat %>% filter(sub. %in% exp2c_inc)}
  return(dat %>% 
           mutate(exp = paste('Chien_et_al.','2022',exp, sep = '_')) %>% 
           dplyr::select(idv,iv,dv,exp))
}
study_name <- 'Chien_et_al_2022'
data_files <- list.files(pattern = '.xlsx')
agg_data <- do.call(rbind,lapply(data_files, read_full_sheet))

write.csv(agg_data, paste0(study_name,'.csv'))
