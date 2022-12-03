library(ggplot2)
library(tidyr)
source('datasets_analysis\\quid.R')
source('datasets_analysis\\pbt.R')

# retrieves the database to analyze (including all individual experiments)
get_input_df <- function(analysis_conf) {
  # if db file already exists
  if(file.exists(analysis_conf@db_output_fn)) {
    print('reading the database from hard-disk, delete the file if you want to recreate it on the next run')
    print(paste('DB file path:', analysis_conf@db_output_fn))
    return(read.csv(analysis_conf@db_output_fn))
  }
  
  is_dir_input <- file.info(analysis_conf@input_source)$isdir
  if (is.na(is_dir_input)) {return(NA)}
  # if input file is an existing file
  if(!is_dir_input) {
    print('reading the database from a file on hard-disk, delete the file if you want to recreate it on the next run')
    print(paste('DB file path:', analysis_conf@input_source))
    
    return(read.csv(analysis_conf@input_source))
  }
  # load the files and run preprocessing functions
  file_list <- list.files(analysis_conf@input_source, pattern = ".csv", full.names = TRUE)
  dfs <- lapply(file_list,function(fn) {
    df = read.csv(fn)
    print(paste('reading file:', fn, '; removing na and nan rows'))
    df <- df[!rowSums(is.na(df[names(df)])), ]
    df <- df[!rowSums(is.nan(as.matrix(df))), ]
    df <- analysis_conf@preprocess_f(df, basename(fn))
    
    return (df)
  })
  df_all <- do.call(rbind, dfs)
  return(df_all)
}


# retrieves the database to analyze (including all individual experiments)
run_analysis <-function(analysis_conf) {
  set.seed(analysis_conf@seed)
  dfs <- get_input_df(analysis_conf)
  analysis_fs <- analysis_conf@sum_fs(analysis_conf, unique(dfs$exp))
  # get a dataframe of the results 
  # (for the RT analysis we use Median as summary_function, and for the AUC analysis we use get_AUC)
  res <- dfs %>%
    group_by(exp) %>%
    group_modify(~data.frame(
      non_directional = test_sign_consistency(.x,'idv', c('dv','iv2'), 'iv',
                                   null_dist_samples = analysis_conf@n_samp,
                                   summary_function = analysis_fs[[.y[[1]]]]$summary)[c('statistic','p')],
                             directional_effect = test_directional_effect(.x,'idv', c('dv','iv2'), 'iv',
                                    null_dist_samples = analysis_conf@n_samp,
                                    summary_function = analysis_fs[[.y[[1]]]]$summary)[c('statistic','p')],
                             quid = run_quid(.x)[c('pos_bf')],
      pbt = run_pbt(.x, analysis_fs[[.y[[1]]]]$test)[c('low','high')]))
  
  # adjust p-values of the directional test
  if('directional_effect.p' %in% names(res)){
    
  }
  res <- res %>% mutate(directional_effect.p = 2*min(directional_effect.p, 1-directional_effect.p))
  
  res_dir <- dirname(analysis_conf@results_fn)
  if(!dir.exists(res_dir)) {
    dir.create(res_dir, recursive = TRUE)
  }
  
  # write results to file
  write.csv(res, analysis_conf@results_fn)  
}
