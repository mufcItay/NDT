source(paste('datasets_analysis', 'definitions.R',
             sep = .Platform$file.sep))
source(paste('datasets_analysis', 'utils.R',
             sep = .Platform$file.sep))

for (analysis_ind in 1: length(all_analysis_types)) {
  analysis <- all_analysis_types[analysis_ind]
  conf <- init_analysis(analysis)
  run_analysis(conf)  
}