source('datasets_analysis\\definitions.R')
source('datasets_analysis\\utils.R')

for (analysis_ind in 1: length(all_analysis_types)) {
  analysis <- all_analysis_types[analysis_ind]
  conf <- init_analysis(analysis)
  run_analysis(conf)  
}