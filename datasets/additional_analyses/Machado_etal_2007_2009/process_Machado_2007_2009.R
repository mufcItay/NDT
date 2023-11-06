library(dplyr)

# exp = 2007 / 2009
get_df <- function(exp, folder_path) {
  folder <- paste(folder_path, paste('data',exp, sep = '_'), sep = .Platform$file.sep)
  fn_pattern <- ifelse(exp == 2007, '.DAT', '.txt')
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
  res <- get_tests_res(filtered_data, unique(filtered_data$SOA))
}


get_tests_res <- function(data, soas, summary_f = mean) {
  res <- data.frame(SOA = integer(), de_statistic = double(), de_p = double(),
                    sc_statistic = double(), sc_p = double())
  for (soa in soas) {
    dat <- data %>% filter(SOA == soa)
    res_de <- 
      signcon::test_directional_effect(dat, idv = 'idv', dv = 'dv', iv = 'iv', 
                                       summary_function = summary_f)
    res_sc <- 
      signcon::test_sign_consistency(dat,  idv = 'idv', dv = 'dv', iv = 'iv',
                                     summary_function = summary_f, perm_repetitions = 100)
    res <- rbind(res, data.frame(SOA = soa, 
                                 de_statistic = res_de$statistic, 
                                 de_p = res_de$p, 
                                 sc_statistic = res_sc$statistic, 
                                 sc_p = res_sc$p))
  } 
  return(res)
}