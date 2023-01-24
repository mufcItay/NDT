library(pracma)
source('datasets_analysis\\definitions.R')
#' get_AUC
#' A summary function for the calculation of AUC

#' @param mat a matrix of the accuracy and confidence scores (columns) for each trial (rows).
#' The accuracy column is coded as 'iv2', and the confidence column is coded as 'dv'.
#' NA is returned if the matrix misses one category of accuracy scores or if the matrix dimensions
#' are not as expected
#' @return the calculated AUC measure
get_AUC <- function(mat, summary_f_args = list(iv = 'iv2', dv = 'dv')){
  mat <- as.data.frame(mat)
  if(length(colnames(mat)) <2) {
    # not enough trials overall
    browser()
    print(mat)
    return(NA)
  }

  accuracy <- mat[,summary_f_args$iv]
  if(length(unique(accuracy)) < 2) {
    # not enough accuracy levels
    return(NA)
  }
  confidence <- mat[,summary_f_args$dv]
  accuracy <- accuracy[order(confidence, decreasing=TRUE)]
  AUC <- trapz(cumsum(!accuracy)/sum(!accuracy),
               cumsum(accuracy)/sum(accuracy))
  return(AUC)
}

#' preprocess_dfs_AUC
#' The function preprocesses a single dataset to fit with the AUC analysis.
#' Datasets for which #Responses != 2, or datasets that do not include an 'Accuracy' column are not included. 
#' participants with less than ten observations in each condition are excluded.
#' @param df a dataframe with the shape (#Participants X #Trials) X (Subj_idx, Accuracy, Confidence, Response)
#' @param ds_name the name of the data frame to preprocess
#' @return a preprocessed dataframe with the shape (#Participants X #Trials) X (idv, iv, iv2, dv),
#' where idv is the identifier of participants, iv is the response category, iv2 is
#' the accuracy score, and dv is the confidence score
preprocess_dfs_AUC <- function(df, ds_name) {
  if((!'Accuracy' %in% names(df)) || length(unique(df$Response)) != 2) {
    return (data.frame())
  }
  # the 'name' of the effect to analyze
  df$exp <- rep(ds_name, nrow(df))
  # rename and select the relevant columns
  df <- df %>% 
    rename(idv = Subj_idx, iv2 = Accuracy, dv = Confidence, iv = Response) %>% 
    dplyr::select(exp, idv, dv, iv2, iv) %>%
    drop_na()
  #exclude subjects from experiments if the have too few trials in each cell
  exclusions <- df %>% 
    mutate(iv = factor(iv), iv2 = factor(iv2)) %>% 
    group_by(idv, iv, iv2) %>% 
    count(idv, name = "n", .drop = F) %>% 
    filter (n < 5) %>%
    pull(idv)
  if(length(exclusions)) { df <- df %>% filter(! idv %in% exclusions) }
  return(df)
}

#' get_sum_fs_AUC
#' The function sets the relevant summary and test functions for each dataset
#' @param analysis_conf the general analysis condfiguration class
#' @param experiments the name of the experiments to set summary and test functions for
#' @return a list of functions to use as summary and test functions for each dataset
get_sum_fs_AUC <- function(analysis_conf, experiments) {
  map_f_to_exp <- function(exp_name) {
    summary_f <- function(mat) {
      return (analysis_conf@summary_f(mat))
    }
    test_f <- function(mat) {
      conds <- sort(unique(mat$iv))
      mat_cond_1 <- mat[mat$iv == conds[1],c('iv2','dv')]
      mat_cond_2 <- mat[mat$iv == conds[2],c('iv2','dv')]
      obs <- analysis_conf@summary_f(mat_cond_1) -
        analysis_conf@summary_f(mat_cond_2)
      return(perm_test_subject(as.data.frame(mat), obs, summary_f = analysis_conf@summary_f,
                               summary_f_args = list(iv = 'iv', iv2 = 'iv2', dv = 'dv')))
    }
    return(list(summary = summary_f, test = test_f))
  }
  df_to_f <- lapply(experiments, map_f_to_exp)
  names(df_to_f) <- experiments
  return(df_to_f)
}

