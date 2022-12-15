library(dplyr)
library(weaknull)
source('datasets_analysis\\definitions.R')
source('datasets_analysis\\utils.R')
# UCID_analysis_conf
# AUC_analysis_conf
# confidence_analysis_conf
# cogdb_analysis_conf
conf <- init_analysis(AUC_analysis_lbl)
conf@db_output_fn <- 'results\\AUC_DB_OLD.csv'

run_analysis(conf)
