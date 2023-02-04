library(dplyr)
library(weaknull)
source('datasets_analysis\\definitions.R')
source('datasets_analysis\\utils.R')
conf <- init_analysis(UCDB_analysis_lbl)
conf@results_fn <-  "results\\Unconscious Processing_UPDATEDEXC_Imporved.csv"
run_analysis(conf)

