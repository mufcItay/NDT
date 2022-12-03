
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

preprocess_dfs_UC <- function(df, ds_name) {
  # special treatment of the multisensory experiment (they used log(rt))
  if(startsWith(ds_name, 'Faivre')) {
    df <- df %>% mutate(dv = log(dv))
  }
  if(! 'iv2' %in% names(df)) { df$iv2 <- rep(NA, nrow(df))} 
  df <- df %>% 
    dplyr::select(exp, idv, dv, iv, iv2)
  
  # remove participants with less than 5 observations in a cell
  if(startsWith(ds_name, 'Stein & van Peelen_2020') |
     startsWith(ds_name, 'Skora et al_2020')) {
    min_trials <- 1
    exc <- df %>%
      group_by(exp, idv, iv, iv2,dv) %>%
      summarise(n = n(), .groups = 'drop_last') %>%
      filter(n < min_trials) %>%
      pull(idv)
    
  } else {
    min_trials <- 5
    exc <- df %>%
      group_by(exp, idv, iv, iv2) %>%
      summarise(n = n(), .groups = 'drop_last') %>%
      filter(n < min_trials) %>%
      pull(idv)
    
  }
  df <- df %>% filter(! idv %in% exc)
  
  return(df)
}

# retrieves the database to analyze (including all individual experiments)
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
        conds <- unique(mat$iv)
        obs <- calc_effect(mat[mat$iv == conds[1],], args = args) -
          calc_effect(mat[mat$iv == conds[2],], args = args)
        perm_test_subject(as.data.frame(mat), obs, summary_f = calc_effect, 
                          summary_f_args = bh_args)
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
        return(wilcox.test(mat[mat$iv==unique(mat$iv)[1],]$dv, 
                           mat[mat$iv==unique(mat$iv)[2],]$dv)$p.value)}
    }
    return(list(summary = summary_f, test = test_f))
  }
  df_to_f <- lapply(experiments, map_f_to_exp)
  names(df_to_f) <- experiments
  return(df_to_f)
}

