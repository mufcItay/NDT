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

get_participant_SDT_d <- function(mat, args = list(iv = 'iv', dv = 'dv')) {
  d <- as.data.frame(mat) %>%
    group_by(!!dplyr::sym(args$iv), !!dplyr::sym(args$dv), .drop = FALSE) %>%
    summarise(count = n(), .groups = 'drop_last') %>%
    ungroup() %>%
    complete(!!dplyr::sym(args$iv), !!dplyr::sym(args$dv), fill = list(count = 0)) %>% 
    group_by(!!dplyr::sym(args$iv)) %>% 
    summarise(rate = ifelse(count[1] == 0, 1 / (2*sum(count)), 
                            ifelse(count[1] == sum(count), 1- 1 / (2*sum(count)),
                            (count[1]) / (sum(count)))), .groups = 'drop_last') %>%
    summarise(d = (qnorm(rate[1]) - qnorm(rate[2])), .groups = 'drop_last') %>%
    pull(d)
  return(d)
}

calc_effect <- function(mat, args = list(summary_f = mean, iv = 'iv', dv = 'dv')) {
  return (as.data.frame(mat) %>% 
            group_by(!!dplyr::sym(args$iv)) %>% 
            summarise(val = args$summary_f(!!dplyr::sym(args$dv))) %>% 
            summarise(effect = diff(val)) %>% 
            pull(effect))
}  

perm_test_subject <- function(mat, obs, summary_f, summary_f_args = list(iv = 'iv', dv = 'dv'), 
                              n_perm = 10^4, two.sided = TRUE) {
  if('iv2' %in% summary_f_args) {
    resamp_f_args <- summary_f_args
    resamp_f_args$iv = resamp_f_args$iv2
    browser()
    
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

# retrieves the database to analyze (including all individual experiments)
get_sum_fs <- function(analysis_conf, experiments) {
  map_f_to_exp <- function(exp_name) {
    if(startsWith(exp_name,'Skora et al_2020')) {
      # d' as a dependent measure
      summary_f <- function(mat) {
        rate <- (sum(mat[,'dv']) + .1) / (length(mat[,'dv']) + .1)
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
        conds <- unique(mat$iv)
        obs <- get_participant_SDT_d(mat[mat$iv == conds[1],], svp_args) - 
          get_participant_SDT_d(mat[mat$iv == conds[2],], svp_args)
        return(perm_test_subject(as.data.frame(mat), obs, get_participant_SDT_d,  summary_f_args =  
                                   list(iv = 'iv', dv = 'dv', iv2 = 'iv2')))
      }
    } else if(startsWith(exp_name,'Benthien & Hesselmann_2021')) {
      bh_args <- list(idv = 'idv', iv = 'iv', iv2 = 'iv2', dv = 'dv', summary_f = analysis_conf@summary_f)
      summary_f <- function(mat) {
        # args <- bh_args
        # args$iv = 'iv2'
        # calc_effect(mat, args = args)
        res<- analysis_conf@summary_f(mat[mat[,'iv2'] == 0, 'dv']) - 
          analysis_conf@summary_f(mat[mat[,'iv2'] == 1, 'dv'])
        return(res) 
      }
      test_f <- function(mat) {
        args <- bh_args
        args$iv = 'iv2'
        conds <- unique(mat$iv)
        obs <- calc_effect(mat[mat$iv == conds[1],], args = args) -
          calc_effect(mat[mat$iv == conds[2],], args = args)
        perm_test_subject(as.data.frame(mat), obs, summary_f = calc_effect, 
                                  summary_f_args = bh_args)
      }
    } else {
      summary_f <- function(mat) {
        return(analysis_conf@summary_f(mat[,'dv']))
      }
      test_f <- function(mat) {
        return(wilcox.test(mat[mat$iv==unique(mat$iv)[1],]$dv, 
                    mat[mat$iv==unique(mat$iv)[2],]$dv)$p.value)}
    }
    return(list(summary = summary_f, test = test_f))
  }
  df_to_f <- lapply(experiments, map_f_to_exp)
  names(df_to_f) <- experiments
  return(df_to_f)
}

# retrieves the database to analyze (including all individual experiments)

run_analysis <-function(analysis_conf) {
  set.seed(analysis_conf@seed)
  dfs <- get_input_df(analysis_conf)
  analysis_fs <- get_sum_fs(analysis_conf, unique(dfs$exp))
  # get a dataframe of the results 
  # (for the RT analysis we use Median as summary_function, and for the AUC analysis we use get_AUC)
  res <- dfs %>%
    group_by(exp) %>%
    group_modify(~data.frame(
      # non_directional = test_sign_consistency(.x,'idv', c('dv','iv2'), 'iv',
      #                              null_dist_samples = analysis_conf@n_samp,
      #                              summary_function = analysis_fs[[.y[[1]]]]$summary)[c('statistic','p')],
      #                        directional_effect = test_directional_effect(.x,'idv', c('dv','iv2'), 'iv',
      #                               null_dist_samples = analysis_conf@n_samp,
      #                               summary_function = analysis_fs[[.y[[1]]]]$summary)[c('statistic','p')],
      #                        quid = run_quid(.x)[c('pos_bf')],
      pbt = run_pbt(.x, analysis_fs[[.y[[1]]]]$test)[c('low','high')]))
  
  browser()
  # adjust p-values of the directional test
  res <- res %>% mutate(directional_effect.p = 2*min(directional_effect.p, 1-directional_effect.p))
  
  res_dir <- dirname(analysis_conf@results_fn)
  if(!dir.exists(res_dir)) {
    dir.create(res_dir, recursive = TRUE)
  }
  
  # write results to file
  write.csv(res, analysis_conf@results_fn)  
}
