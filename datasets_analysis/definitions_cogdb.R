
#' preprocess_dfs_cogdb
#' The function preprocesses a single dataset to fit with the analysis pipeline.
#' participants with less than two observations in each condition are excluded.
#' @param df a dataframe with the shape (#Participants X #Trials) X (idv, iv, dv)
#' @param ds_name the name of the data frame to preprocess
#' @return a preprocessed dataframe with the shape (#Participants X #Trials) X (idv, iv, iv2, dv),
#' where idv is the identifier of participants, iv is the experimental condition, iv2 is set to NA
#' unless the datasets includes an interaction analysis, or if the dependent measure is calculated
#' from more than one variable,and dv is the dependent measure
preprocess_dfs_cogdb  <- function(df, ds_name) {
  #exclude subjects from experiments if the have too few trials in each cell
  df <- df %>% 
    mutate(iv = factor(iv))
  if(! 'iv2' %in% names(df)) { df$iv2 <- rep(INVALID_VALUE_CODE, nrow(df))} 
  exc <- df %>%
    mutate(unique_id = paste(exp, idv, sep = '_')) %>%
    group_by(unique_id, iv, iv2) %>%
    summarise(n = n(), .groups = 'drop_last') %>%
    filter(n < 5) %>%
    pull(unique_id)
  
  df <- df %>% 
    dplyr::select(idv,iv,iv2,dv,exp)
  if(length(exc)) { df <- df %>% filter(! paste(exp, idv, sep = '_') %in% exc) }
  
  return (df)
}

#' get_sum_fs_cogdb
#' The function sets the relevant summary and test functions for each dataset
#' @param analysis_conf the general analysis condfiguration class
#' @param experiments the name of the experiments to set summary and test functions for
#' @return a list of functions to use as summary and test functions for each dataset
get_sum_fs_cogdb <- function(analysis_conf, experiments) {
  interaction_args <- list(idv = 'idv', iv = 'iv', iv2 = 'iv2', dv = 'dv', 
                           summary_f = analysis_conf@summary_f)
  map_f_to_exp <- function(exp_name) {
    # special case of an interaction effect
    if(startsWith(exp_name,'Battich') | 
       startsWith(exp_name,'Roelofs') | 
       startsWith(exp_name,'Yap')) {
      args <- interaction_args
      args$iv <- 'iv2'
      summary_f <- function(mat) {
        return (get_diffscore_f(mat, args))
      }
      test_f <- function(mat) {
        return(test_diffscore_perm(mat,args,interaction_args))
      }
      } else {
        summary_f <- function(mat) {
          # if the data is not arranged as a matrix, rearrange and apply summary f
          if(is.null(dimnames(mat))) {
            return(analysis_conf@summary_f(as.numeric(t(mat)[,'dv'])))
          }
          return (analysis_conf@summary_f(as.numeric(mat[,'dv'])))
        }
        test_f <- function(mat) {
          mat <- as.data.frame(mat)

          return(t.test(mat[mat$iv==unique(mat$iv)[1],]$dv,
                             mat[mat$iv==unique(mat$iv)[2],]$dv)$p.value)
        }
    }
    return(list(summary = summary_f, test = test_f))
  }
  df_to_f <- lapply(experiments, map_f_to_exp)
  names(df_to_f) <- experiments
  return(df_to_f)
}

#' get_diffscore_f
#' the function calculates the difference score between two 'iv' conditions
#' @param mat a matrix of the condition labels (iv) and responses (dv) for each trial (rows).
#' @param args a list of parameter values for each column name and the summary function
#' @return the function returns the difference score between two conditions,
#' according to the summary function (for an interaction effect, use args$iv = 'iv2')
get_diffscore_f <- function(mat, args = list(summary_f = mean, iv = 'iv', dv = 'dv')) {
  mat <- as.data.frame(mat) %>% mutate(dv = as.numeric(dv))
  values <- mat %>% pull(dplyr::sym(args$iv))
  conds <- sort(unique(values))
  res <- args$summary_f(mat[values == conds[2],]$dv) - 
    args$summary_f(mat[values == conds[1],]$dv)
  return(res)
}

#' test_diffscore_perm
#' the function tests for a difference score effect for the given data, using a
#' permutations test
#' @param mat a matrix of the condition labels (iv) and responses (dv) for each trial (rows).
#' @param args a list of parameter values for each column name and the summary function
#' according to which the observed difference score is calculated
#' @param args_perm  a list of parameter values for each column name and the summary function
#' which is given as input to the permutations test
#' @return the function returns the test's p-value
test_diffscore_perm <- function(mat, args, args_perm) {
  conds_iv <- sort(unique(mat$iv))
  mat_cnd_1 <- mat[mat$iv == conds_iv[1],]
  mat_cnd_2 <- mat[mat$iv == conds_iv[2],]
  obs <- get_diffscore_f(mat_cnd_2, args) - 
    get_diffscore_f(mat_cnd_1, args)
  return(perm_test_subject(as.data.frame(mat), obs, summary_f = get_diffscore_f, 
                           summary_f_args = args_perm))
}

