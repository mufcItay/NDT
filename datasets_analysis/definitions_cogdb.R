
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
  if(!'iv2' %in% names(df)) { df$iv2 = NA }
  exclusions <- df %>%
    group_by(idv, iv, iv2) %>% 
    count(idv, name = "n", .drop = F) %>% 
    filter (n < 2) %>% 
    pull(idv)
  df <- df %>% 
    dplyr::select(idv,iv,iv2,dv,exp)
  if(length(exclusions)) { df <- df %>% filter(! idv %in% exclusions) }
  
  return (df)
}

#' get_sum_fs_cogdb
#' The function sets the relevant summary and test functions for each dataset
#' @param analysis_conf the general analysis condfiguration class
#' @param experiments the name of the experiments to set summary and test functions for
#' @return a list of functions to use as summary and test functions for each dataset
get_sum_fs_cogdb <- function(analysis_conf, experiments) {
  map_f_to_exp <- function(exp_name) {
    # special case of an interaction effect
    if(startsWith(exp_name,'jasifi')) {
      sum_f <- ifelse(endsWith(exp_name,'rt'), stats::median, base::mean)
      jasifi_args <- list(idv = 'idv', iv = 'iv', iv2 = 'iv2', dv = 'dv', summary_f = sum_f)
      args <- jasifi_args
      args$iv <- 'iv2'
      get_effect_f <- function(mat, args = list(summary_f = mean, iv = 'iv', dv = 'dv')) {
        mat <- as.data.frame(mat)
        values <- mat %>% pull(dplyr::sym(args$iv))
        conds <- sort(unique(values))
        res <- args$summary_f(mat[values == conds[2],]$dv) - 
          args$summary_f(mat[values == conds[1],]$dv)
        return(res)
      }
      summary_f <- function(mat) {
        return (get_effect_f(mat, args))
      }
      test_f <- function(mat) {
        conds_iv <- sort(unique(mat$iv))
        mat_cnd_1 <- mat[mat$iv == conds_iv[1],]
        mat_cnd_2 <- mat[mat$iv == conds_iv[2],]
        obs <- get_effect_f(mat_cnd_2, args) - get_effect_f(mat_cnd_1, args)
        return(perm_test_subject(as.data.frame(mat), obs, summary_f = get_effect_f, 
                          summary_f_args = jasifi_args))
        }
      } else {
        summary_f <- function(mat) {
          # if the data is not arranged as a matrix, rearrange and apply summary f
          if(is.null(dimnames(mat))) {
            return(analysis_conf@summary_f(t(mat)[,'dv']))
          }
          return (analysis_conf@summary_f(mat[,'dv']))
        }
        test_f <- function(mat) {
          mat <- as.data.frame(mat)

          return(wilcox.test(mat[mat$iv==unique(mat$iv)[1],]$dv,
                             mat[mat$iv==unique(mat$iv)[2],]$dv)$p.value)
        }
    }
    return(list(summary = summary_f, test = test_f))
  }
  df_to_f <- lapply(experiments, map_f_to_exp)
  names(df_to_f) <- experiments
  return(df_to_f)
}

