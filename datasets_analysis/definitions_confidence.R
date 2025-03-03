#' preprocess_dfs_cdb
#' The function preprocesses a single dataset to fit with the analysis of confidence scores.
#' participants with less than two observations in each condition are excluded.
#' @param df a dataframe with the shape (#Participants X #Trials) X (Subj_idx, Response, Confidence)
#' @param ds_name the name of the data frame to preprocess
#' @return a preprocessed dataframe with the shape (#Participants X #Trials) X (idv, iv, iv2, dv),
#' where idv is the identifier of participants, iv is the response, iv2 is set to NA
#' unless the datasets includes an interaction analysis, or if the dependent measure is calculated
#' from more than one variable,and dv is the confidence measure
preprocess_dfs_cdb <- function(df, ds_name) {
  if(length(unique(df$Response)) != 2) {
    return (data.frame())
  }
  # the 'name' of the effect to analyze
  df$exp <- rep(ds_name, nrow(df))
  # rename and select the relevant columns
  df <- df %>% 
    rename(idv = Subj_idx, iv = Response, dv = Confidence) %>% 
    dplyr::select(exp, idv, iv, dv) %>%
    mutate(iv2 = INVALID_VALUE_CODE)
  
  #exclude subjects from experiments if the have too few trials in each cell
  
  df <- exclude_participants(df, vars(iv))
  return (df)
}

#' get_sum_fs_confidence
#' The function sets the relevant summary and test functions for each dataset
#' @param analysis_conf the general analysis configuration class
#' @param experiments the name of the experiments to set summary and test functions for
#' @return a list of functions to use as summary and test functions for each dataset
get_sum_fs_confidence <- function(analysis_conf, experiments) {
  map_f_to_exp <- function(exp_name) {
    summary_f <- function(mat) {
      return (analysis_conf@summary_f(mat$dv))
    }
    summary_f_abs_es <- function(mat_cnd1, mat_cnd2) {
      return(lsr::cohensD(mat_cnd1[,'dv'], mat_cnd2[,'dv']))
    }
    test_f <- function(mat) {
      mat <- as.data.frame(mat)
      conds <- sort(unique(mat$iv))
      return(wilcox.test(mat[mat$iv==conds[1],]$dv, 
                    mat[mat$iv==conds[2],]$dv)$p.value)
    }
    return(list(summary = summary_f, test = test_f, summary_abs_es = summary_f_abs_es))
  }
  df_to_f <- lapply(experiments, map_f_to_exp)
  names(df_to_f) <- experiments
  return(df_to_f)
}

