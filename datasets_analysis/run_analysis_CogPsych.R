library(dplyr)
library(weaknull)
source('datasets_analysis\\definitions.R')
source('datasets_analysis\\utils.R')
conf <- init_analysis(Cognitive_Psychology_analysis_lbl)
conf@results_fn <-  "results\\Cognitive Psychology_Results.csv"
run_analysis(conf)