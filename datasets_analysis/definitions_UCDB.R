
#' get_participant_SDT_d
#' The function calculates a d' score for a given participant.
#' It uses an adjustments to the hit and false alarm rate (if the calculated rate
#' is 1, the function sets it to 1-1/2N, and if it is 0 it sets the rate to 1/2N; where N
#' denotes the number of total trials)  
#' @param mat a matrix of the condition labels (iv) and responses (dv) for each trial (rows).
#' @param args a list of parameter values for the independent measure (iv)
#' @return the difference between Z(hit_rate) and Z(fa_rate)
get_participant_SDT_d <- function(mat, args = list(iv = 'iv', dv = 'dv')) {
  mat <- as.data.frame(mat)
  conds <- sort(unique(mat[,args$iv]))
  calc_rate_nrom <- function(cnd, mat) {
    cnd_dat <- mat[mat[,args$iv] == cnd,]
    cnt <- sum(cnd_dat[,args$dv])
    len <- length(cnd_dat[,args$dv]) 
    rate <- ifelse(cnt == 0, 1 / (2*len), 
                   ifelse(cnt == len, 1- 1 / (2*len), cnt / len))
    return (qnorm(rate))
  }
  rate_norms <- sapply(conds, calc_rate_nrom, mat = mat)
  return (diff(rate_norms))
}

#' preprocess_dfs_UC
#' The function preprocesses a single dataset to fit with the analysis of unconscious processing effects.
#' participants with insufficient observations in each condition are excluded.
#' @param df a dataframe with the shape (#Participants X #Trials) X (idv, iv, dv)
#' @param ds_name the name of the data frame to preprocess
#' @return a preprocessed dataframe with the shape (#Participants X #Trials) X (idv, iv, iv2, dv),
#' where idv is the identifier of participants, iv is the experimental condition, 
#' iv2 is set to NA unless the datasets includes an interaction analysis, 
#' or if the dependent measure is calculated from more than one variable,
#' and dv is reaction time in each trials
preprocess_dfs_UC <- function(df, ds_name) {
  # if there is no second independent variable, just set -1 to all iv2 values,
  # the summary functions don't use them anyway so that should work
  if(! 'iv2' %in% names(df)) { df$iv2 <- rep(INVALID_VALUE_CODE, nrow(df))} 
  df <- df %>% 
    dplyr::select(exp, idv, dv, iv, iv2)

  # remove participants with less than 2 observations in a cell
  if(startsWith(ds_name, 'Stein & van Peelen_2020') |
     startsWith(ds_name, 'Skora et al_2020')) {
    df <- exclude_participants(df, vars(iv, iv2, dv))
  } else {
    df <- exclude_participants(df, vars(iv, iv2))
  }
  return(df)
}

#' get_sum_fs_UC
#' The function sets the relevant summary and test functions for each dataset
#' @param analysis_conf the general analysis condfiguration class
#' @param experiments the name of the experiments to set summary and test functions for
#' @return a list of functions to use as summary and test functions for each dataset
get_sum_fs_UC <- function(analysis_conf, experiments) {
  map_f_to_exp <- function(exp_name) {
    if(startsWith(exp_name,'Skora et al_2020')) {
      summary_f <- function(mat) {
        cnt <- sum(mat[,'dv'])
        len <- length(mat[,'dv']) 
        rate <- ifelse(cnt == 0, 1 / (2*len), 
                       ifelse(cnt == len, 1- 1 / (2*len), cnt / len))
        return (qnorm(rate))
      }
      
      test_f <- function(mat) {
        obs_d <- get_participant_SDT_d(mat)
        return(perm_test_subject(as.data.frame(mat), obs_d, get_participant_SDT_d))
      }
    } else if(startsWith(exp_name,'Stein & van Peelen_2020')) {
      svp_args <- list(iv = 'iv2', dv = 'dv')
      summary_f <- function(mat) {
        return (get_participant_SDT_d(mat, svp_args))
      }
      test_f <- function(mat) {
        conds <- sort(unique(mat$iv))
        obs <- get_participant_SDT_d(mat[mat$iv == conds[1],], svp_args) - 
          get_participant_SDT_d(mat[mat$iv == conds[2],], svp_args)
        return(perm_test_subject(as.data.frame(mat), obs, get_participant_SDT_d,  summary_f_args =  
                                   list(iv = 'iv', dv = 'dv', iv2 = 'iv2')))
      }
    } else if(startsWith(exp_name,'Benthien & Hesselmann_2021')) {
      bh_args <- list(idv = 'idv', iv = 'iv', iv2 = 'iv2', dv = 'dv', summary_f = analysis_conf@summary_f)
      summary_f <- function(mat) {
        res<- analysis_conf@summary_f(mat[mat[,'iv2'] == 0, 'dv']) - 
          analysis_conf@summary_f(mat[mat[,'iv2'] == 1, 'dv'])
        return(res) 
      }
      test_f <- function(mat) {
        args <- bh_args
        args$iv = 'iv2'
        conds <- sort(unique(mat$iv))
        obs <- get_diff_effect(mat[mat$iv == conds[1],], args = args) -
          get_diff_effect(mat[mat$iv == conds[2],], args = args)
        return(perm_test_subject(as.data.frame(mat), obs, summary_f = get_diff_effect, 
                          summary_f_args = bh_args))
      }
    } else {
      summary_f <- function(mat) {
        if(is.null(nrow(mat))) {
          return (mat[names(mat) == 'dv'])
        } 
        return(analysis_conf@summary_f(mat[,'dv']))
      }
      test_f <- function(mat) {
        mat <- as.data.frame(mat)
        conds <- sort(unique(mat$iv))
        return(t.test(mat[mat$iv==conds[1],]$dv,
                           mat[mat$iv==conds[2],]$dv)$p.value)}
    }
    return(list(summary = summary_f, test = test_f))
  }
  df_to_f <- lapply(experiments, map_f_to_exp)
  names(df_to_f) <- experiments
  return(df_to_f)
}

