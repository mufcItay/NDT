library(dplyr)
library(weaknull)
source('datasets_analysis\\definitions.R')
source('datasets_analysis\\utils.R')
conf <- init_analysis(cogdb_analysis_lbl)
conf@results_fn <-  "results\\cogdb_Results_Mean_NEW_Replication.csv"
run_analysis(conf)

