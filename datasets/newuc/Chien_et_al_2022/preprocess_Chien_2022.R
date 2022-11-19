library(weaknull)
library(dplyr)
library(readxl)

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)

read_full_sheet <- function(fn) {
  sheets <- excel_sheets(fn)
  exp_name <- substr(fn,4,5)
  # we code iv = oddity(condition) because conditions are (cold-cold, cold-warm, warm-warm, warm-cold)
  dat <- read_excel(fn, sheet = sheets[length(sheets)]) %>%
    mutate(exp = exp_name, iv = condition %% 2) %>%
    rename(idv = sub., dv = rt)
  
  # from the excel sheets, the included participants were manually extractes
  exp2a_inc <- c('s1','s12','s13','s14','s15','s16','s18','s20','s21','s22','s24','s25','s28','s29','s3','s30','s32','s33','s35','s37','s5','s6','s8','s9')
  exp2b_inc <- c('s15', 's18', 's19', 's22', 's23', 's24', 's25', 's28', 's29', 's31', 's32', 's33', 's35', 's36', 's40', 's42', 's43', 's44', 's45', 's47', 's49', 's60 ', 's62', 's64')
  exp2c_inc <- c('s1', 's12', 's13', 's14', 's15', 's16', 's18', 's20', 's21', 's22', 's24', 's25', 's28', 's29', 's3', 's30', 's32', 's33', 's35', 's37', 's5', 's6', 's8', 's9')
  
  if (exp_name == '2A') {dat <- dat %>% filter(idv %in% exp2a_inc)} else
    if (exp_name == '2B') {dat <- dat %>% filter(idv %in% exp2b_inc)} else
      if (exp_name == '2C') {dat <- dat %>% filter(idv %in% exp2c_inc)}
  return(dat)
}

study_name <- 'Chien_et_al_2022'
data_files <- list.files(pattern = '.xlsx')
agg_data <- do.call(rbind,lapply(data_files, read_full_sheet))

write.csv(agg_data, paste0(study_name,'.csv'))

get_2a <- get_directional_effect(agg_data[agg_data$exp == '2A',], 'idv','dv','iv', 
                                 summary_function = mean)

dir_2a <- test_directional_effect(agg_data[agg_data$exp == '2A',], 'idv','dv','iv', 
                               summary_function = mean)
nondir_2a <- test_sign_consistency(agg_data[agg_data$exp == '2A',], 'idv','dv','iv', 
                               summary_function = mean)
#2b
dir_2b <- test_directional_effect(agg_data[agg_data$exp == '2B',], 'idv','dv','iv', 
                                  summary_function = mean)
nondir_2b <- test_sign_consistency(agg_data[agg_data$exp == '2B',], 'idv','dv','iv', 
                                   summary_function = mean)
#2c
dir_2c <- test_directional_effect(agg_data[agg_data$exp == '2C',], 'idv','dv','iv', 
                                  summary_function = mean)
nondir_2c <- test_sign_consistency(agg_data[agg_data$exp == '2C',], 'idv','dv','iv', 
                                   summary_function = mean)

# validate against the 'Sheet2' sheet of the data - note that we collapse across cong and incong levels
agg_data[agg_data$exp == '2C',] %>% group_by(idv, condition, iv) %>% summarise(m = mean(dv)) %>%
  group_by(idv, iv) %>% summarise(m = mean(m))
