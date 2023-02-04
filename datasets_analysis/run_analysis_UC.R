library(dplyr)
library(weaknull)
source('datasets_analysis\\definitions.R')
source('datasets_analysis\\utils.R')
conf <- init_analysis(Unconscious_Processing_analysis_lbl)
conf@results_fn <-  "results\\Unconscious Processing_Results.csv"
run_analysis(conf)

