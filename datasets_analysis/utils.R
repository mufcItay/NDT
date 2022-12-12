library(tidyr)
source('datasets_analysis\\quid.R')
source('datasets_analysis\\pbt.R')

#' get_input_df
#' The function reads all datasets according to the 'analysis_conf' parameter to generate
#' an aggregated dataframe for the analysis of all datasets 
#' @param analysis_conf a configuration class with the functions needed to read and
#' preporocess the relevant datasets and
#' @return an aggregated dataframe with all datasets.
#' The shape of the dataframe is (#Experiments X #Participants X #Trials) X (exp,idv,iv,iv2,dv),
#' where exp is the identifier of the experiment (and dataset), idv is the identifier 
#' of each participant, iv is the experimental condition, iv2 is an optional column with
#' another parameter in the dataset, and dv is the dependent measure
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


#' run_analysis
#' The function reads all datasets and analyze them according to the 'analysis_conf' parameter
#' The output of the function is a csv file describing all of the analysis results
#' @param analysis_conf a configuration class with the functions needed to read, preporocess,
#' and analyze the relevant datasets according to all relevant tests
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
    res <- res %>% mutate(directional_effect.p = 2*min(directional_effect.p, 1-directional_effect.p))
  }
  
  res_dir <- dirname(analysis_conf@results_fn)
  if(!dir.exists(res_dir)) {
    dir.create(res_dir, recursive = TRUE)
  }
  
  # write results to file
  write.csv(res, analysis_conf@results_fn)  
}

#' perm_test_subject
#' The function runs a permutations test per subject using a given summary function, for
#' the difference between two conditions
#' @param mat a matrix of the shape (#Trials) X (iv, dv), where 'iv' indicates the experimental
#' condition of each trial and 'dv' indicates the dependent measure in the same trial.
#' @param obs the observed difference score between conditions
#' @param summary_f the summary function to use when summarizing the data in each condition.
#' The function should be implemented in the form of (mat') -> (result), assuming that mat' is
#' a subset of the trials in mat, from a specific condition 
#' @param summary_f_args a list of parameter names in 'mat' to be used within the summary_f
#' function. The list should include a indepdent variable ('iv') and a depdent measure ('dv)
#' @param n_perm the number of permutations to use in the permutations test
#' @param two.sided a boolean parameter determining whether we use a two-sided permutations test.
#' If the parameter is set to FALSE, a one-sided test would be used
#' @return the function returns the test's p-value
perm_test_subject <- function(mat, obs, summary_f, summary_f_args = list(iv = 'iv', dv = 'dv'), 
                              n_perm = 10^4, two.sided = TRUE) {
  if('iv2' %in% summary_f_args) {
    resamp_f_args <- summary_f_args
    resamp_f_args$iv = resamp_f_args$iv2
    
    n_trials <- nrow(mat)
    conds <- unique(mat$iv)
    inner_perm <- function(iteration, mat, summary_f, summary_f_args) {
      mat[,summary_f_args$iv] <- mat[sample(n_trials),summary_f_args$iv]
      return (summary_f(mat[mat$iv == conds[1],], resamp_f_args) -
                summary_f(mat[mat$iv == conds[2],], resamp_f_args))
    }
    null_dist <- sapply(1:n_perm, inner_perm, mat = mat, 
                        summary_f = summary_f, summary_f_args = summary_f_args)
  } else {
    inner_perm <- function(iteration, mat, summary_f, summary_f_args) {
      n_trials <- nrow(mat)
      mat[,summary_f_args$dv] <- mat[sample(n_trials),summary_f_args$dv]
      return (summary_f(mat, summary_f_args))
    }
    null_dist <- sapply(1:n_perm, inner_perm, mat = mat, 
                        summary_f = summary_f, summary_f_args = summary_f_args)
  }
  p_value <- mean(obs < null_dist, na.rm=TRUE)
  if(two.sided) {p_value <- 2 * min(p_value, 1 - p_value)}
  return (p_value)
}

#' get_diff_effect
#' The function calculates the difference score between the experimental conditions
#' @param mat a matrix of the shape (#Trials) X (iv, dv), where 'iv' indicates the experimental
#' condition of each trial and 'dv' indicates the dependent measure in the same trial.
#' @param summary_f_args a list, including (1) a summary function 'summary_f' which is 
#' the summary function to use when summarizing the data in each condition.
#' The function should be implemented in the form of (mat') -> (result), assuming that mat' is
#' a subset of the trials in mat, from a specific condition of parameter names in 'mat' to be used within the summary_f
#' function (2) parameter names including ('iv') and a depdent measure ('dv)
#' @return the difference score between the experimental conditions
get_diff_effect <- function(mat, args = list(summary_f = mean, iv = 'iv', dv = 'dv')) {
  return (as.data.frame(mat) %>% 
            group_by(!!dplyr::sym(args$iv)) %>% 
            summarise(val = args$summary_f(!!dplyr::sym(args$dv))) %>% 
            summarise(effect = diff(val)) %>% 
            pull(effect))
}  

