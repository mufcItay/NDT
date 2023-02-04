library(dplyr)
library(weaknull)
source('datasets_analysis\\utils.R')
source('datasets_analysis\\definitions.R')
conf <- init_analysis(Metacognitive_Sensitivity_analysis_lbl)
conf@results_fn <- "results\\Metacognitive Sensitivity_Results.csv"
run_analysis(conf)
