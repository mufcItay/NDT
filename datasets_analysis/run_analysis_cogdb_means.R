library(dplyr)
library(weaknull)
source('datasets_analysis\\definitions.R')
source('datasets_analysis\\utils.R')
conf <- init_analysis(UCDB_analysis_lbl)
conf@results_fn <-  "results\\UCDB_Results_Test.csv"
run_analysis(conf)

