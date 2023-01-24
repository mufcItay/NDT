library(dplyr)
library(weaknull)
source('datasets_analysis\\utils.R')
source('datasets_analysis\\definitions.R')
conf <- init_analysis(UCDB_analysis_lbl)
conf@results_fn <-  "results\\UCDB_Results_MEANS.csv"
conf@summary_f <- mean
run_analysis(conf)
