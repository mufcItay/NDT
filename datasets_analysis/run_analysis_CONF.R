library(dplyr)
library(weaknull)
source('datasets_analysis\\utils.R')
source('datasets_analysis\\definitions.R')
conf <- init_analysis(confidence_analysis_lbl)
conf@results_fn <-  "results\\Confidence_Results_Replication.csv"
run_analysis(conf)