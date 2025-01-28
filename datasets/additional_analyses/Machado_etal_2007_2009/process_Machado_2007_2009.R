library(dplyr)

# exp = 2007 / 2009
get_df <- function(exp, folder_path) {
  folder <- paste(folder_path, paste('data',exp, sep = '_'), sep = .Platform$file.sep)
  fn_pattern <- ifelse(exp == 2007, '.DAT', '.txt')
  # the data was shared with us privately, so if no datasets are
  # available, just return statistics
  if(dir.exists(folder) == FALSE) {
    return(NA)
  }
  fns <- list.files(folder, pattern = fn_pattern, full.names = TRUE)
  if(exp == 2007) {
    data <- do.call(rbind, lapply(fns, read.table, skip = 2, header = FALSE))
  } else {
    data <- do.call(rbind, lapply(fns, read.csv, sep = '\t', header = FALSE,
                                  skip =1))
  }
  return(data)
}

analyze_machado <- function(exp, folder_path) {
  # read the dataset
  data <- get_df(exp, folder_path)
  # the data was shared with us privately, so providing only statistics
  if(all(is.na(data))) {
    res <- data.frame(SOA = integer(), de_statistic = double(), de_p = double(),
                      sc_statistic = double(), sc_p = double())
    if(exp == 2007) {
      res <- rbind(res, list(SOA = 350, de_statistic = -4.424109, de_p = 0.5873413, 
                             sc_statistic = 0.7802222, sc_p = 9.999e-05))
    } else if (exp == 2009) {
      res <- rbind(res, list(SOA = 650, de_statistic = -0.001725521, de_p = 0.8593141, 
                             sc_statistic = 0.6423, sc_p = 0.01049895))
    }
    return(res)
  }
  ## Machado_et_al_2007
  if (exp == 2007) {
    # in the paper they refer to both 352 and 353 as the SOA = 350ms condition,
    # we do the same
    ns_directional_soas <- c(352, 353)
    names(data) <- c('S', 'respmap', 'flkonset', 'SOA', 'flkpos','tarcolor',
                            'flkcolor', 'resp', 'rt', 'err')
    filtered_data <- data %>%
      rename(idv = S, dv = rt) %>%
      mutate(idv = factor(as.numeric(factor(idv))),
             iv = factor(as.numeric(tarcolor == flkcolor)),
             exp = 'Machado_et_al_2007',
             SOA = ifelse(SOA %in% ns_directional_soas, 350, SOA)) %>%
      filter(SOA == 350, err == '*') %>%
      dplyr::select(idv,iv,dv, exp, SOA)
  } else if (exp == 2009) { # Machado_et_al_2009
    # drop NA column (last column)
    data <- data [1: ncol(data)-1 ]
    names(data) <- c('S', 'Block', 'Trial', 'SOA', 'FlkPos', 'FlkCol','TarCol',
                            'OnLine', 'RT', 'MT', 'Distance')
    filtered_data <- data %>%
      rename(idv = S, dv = RT) %>%
      mutate(idv = factor(as.numeric(factor(idv))),
             iv = factor(as.numeric(TarCol == FlkCol)),
             exp = paste('Machado_et_al_2009', SOA, sep = '_')) %>%
      filter(SOA == 650 | SOA == 950) %>%
      dplyr::select(idv,iv,dv, exp, SOA)
  }
  
  summary_f_abs_es <- function(mat_cnd1, mat_cnd2) {
    res <- lsr::cohensD(mat_cnd1, mat_cnd2)
    return(res)
  }
  
  res <- get_tests_res(filtered_data, unique(filtered_data$SOA), summary_f_abs_es)
}


get_tests_res <- function(data, soas, summary_f_es, summary_f = mean) {
  res <- data.frame(SOA = integer(), de_statistic = double(), de_p = double(),
                    sc_statistic = double(), sc_p = double())
  for (soa in soas) {
    dat <- data %>% filter(SOA == soa)
    res_de <- 
      signcon::test_directional_effect(dat, idv = 'idv', dv = 'dv', iv = 'iv', 
                                       summary_function = summary_f)
    res_abs_es <-
      signcon::test_absolute_es(dat,  idv = 'idv', dv = 'dv', iv = 'iv',
                                summary_function = summary_f_es, perm_repetitions = 100)
    res_sc <- 
      signcon::test_sign_consistency(dat,  idv = 'idv', dv = 'dv', iv = 'iv',
                                     summary_function = summary_f, perm_repetitions = 100)
    
    res <- rbind(res, data.frame(SOA = soa, 
                                 de_statistic = res_de$statistic, 
                                 de_p = res_de$p, 
                                 sc_statistic = res_sc$statistic, 
                                 sc_p = res_sc$p,
                                 abs_es_statistic = res_abs_es$statistic,
                                 abs_es_p = res_abs_es$p))
  } 
  return(res)
}