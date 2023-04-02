library(dplyr)
library(osfr)

dir_path <- dirname(rstudioapi::getSourceEditorContext()$path)

# get the latest Confidence Database zip
osf_dir_name <- 'Confidence Database'
osf_repo <- 'osf.io/s46pr/' 
osf_files <- osf_ls_files(osf_retrieve_node(osf_repo), type = 'folder')
cdb_osf_file <- osf_files[osf_files$name == osf_dir_name,]
osf_download(cdb_osf_file, path = dir_path, conflicts = 'overwrite')
fileConn<-file(paste(dir_path, osf_dir_name, paste0(Sys.Date(),".txt"), 
                     sep = .Platform$file.sep),)
writeLines(c('Downloaded the confidence database at the date specified in the file name'), fileConn)
close(fileConn)


dataset_files <- list.files(path = paste(dir_path, osf_dir_name, sep = .Platform$file.sep),
           pattern = '.csv', full.names = TRUE)

preprocess_dataset <- function(fn, db_name, expected_cols) {
  df <- read.csv(fn)
  if(! all(expected_cols %in% names(df))) {
    return()
  }
  df <- df[,expected_cols] %>% drop_na()
  write.csv(df, paste(dir_path, db_name, basename(fn), sep = .Platform$file.sep))
}

# AUC DB
exp_cols_auc <- c('Subj_idx', 'Accuracy','Response', 'Confidence')
auc_db_name <- 'AUCDB'
if (!dir.exists(paste(dir_path, auc_db_name, sep = .Platform$file.sep))) {
  dir.create(paste(dir_path, auc_db_name , sep=.Platform$file.sep))
}
sapply(dataset_files, preprocess_dataset, db_name = auc_db_name, expected_cols = exp_cols_auc)

# Confidence DB
exp_cols_confidence <- c('Subj_idx','Response', 'Confidence')
conf_db_name <- 'CONFDB'
if (!dir.exists(paste(dir_path, conf_db_name , sep = .Platform$file.sep))) {
  dir.create(paste(dir_path, conf_db_name , sep =.Platform$file.sep))
}
sapply(dataset_files, preprocess_dataset,  db_name = conf_db_name, expected_cols = exp_cols_confidence)
