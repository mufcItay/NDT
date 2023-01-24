library(dplyr)
library(weaknull)
source('datasets_analysis\\definitions.R')
source('datasets_analysis\\utils.R')
conf <- init_analysis(cogdb_analysis_lbl)
conf@results_fn <-  "results\\cogdb_Results_Means_NEW.csv"
run_analysis(conf)
