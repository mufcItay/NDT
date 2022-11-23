library(weaknull)
library(dplyr)

# Set our working directory and load data
dirPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirPath)

# read the dataset
study_name <- 'Zerweck et al_2021'
data_dir <- 'data'
exps <- list.dirs(data_dir,recursive = FALSE)
read_data_f <- function(dir_name) {
  exp_num <- substring(dir_name,  nchar(dir_name), nchar(dir_name))
  if(exp_num == '1') {
    data_fn <- list.files(dir_name, pattern = '*ndirectTask.dat', full.names = TRUE)
    data <- read.csv(data_fn, sep = ' ') %>%
      rename(idv = sub, iv = congruency) %>%
      filter(time_error =="Normal",
        key_error == 'correct') %>%
      mutate(exp = paste(study_name, exp_num, sep = '_'))
  } else if(exp_num == '2') {
    data_fn <- list.files(dir_name, pattern = '*ndirectTask.dat', full.names = TRUE)
    data <- read.csv(data_fn, sep = ' ')
    uc_contrasts <- sort(unique(data$Contrast))[1:3]
    data <- data %>%
      filter(Contrast %in% uc_contrasts,
             TimingError == 0, Task== "RTTaskExp", ResponseTime=="Normal",
             ResponseError=="keypress_correct") %>%
      mutate(exp = paste(study_name, exp_num,Contrast, sep = '_')) %>%
      rename(idv = VP, iv = Congruency, RT = RT.Matlab)
  } else if(exp_num == '3') {
    data_fn <- list.files(paste(dir_name, 'data_Exp3',sep = .Platform$file.sep), 
                          pattern = 'om1203_rttask*', full.names = TRUE)
    data_fn_uc <- data_fn[1:4]
    data <- do.call(rbind, lapply(data_fn_uc, 
                                  function(fn) read.csv(fn, sep = ' ') %>% 
                                    mutate(exp = substr(fn[1], nchar(fn[1]) - 5, nchar(fn[1]) - 4))))
    
    # data_fn <- paste(dir_name, 'data_Exp3', 'om1203_rttask_10.dat', sep = .Platform$file.sep)
    data <- data %>%
      filter(!endsWith(block_type, 'Prac'),
             time_error=="Normal", key_error=="correct") %>%
      mutate(exp = paste(study_name, exp_num, 'Duration' ,exp ,sep = '_')) %>%
      rename(idv = sub, iv = congruency)
  }
  
  return (data %>%
            rename(dv = RT) %>%
            select(idv, iv, exp, dv))
}
data <- do.call(rbind, lapply(exps, read_data_f))
data <- data[data$exp != 'Zerweck et al_2021_1',]
write.csv(data, paste0(study_name,'.csv'))


# validate against results in each section + Figures 4,6
data %>%
  group_by(exp, iv, idv) %>%
  summarise(m = mean(dv)) %>%
  group_by(exp, idv) %>%
  summarise(m = diff(m)) %>%
  group_by(exp) %>%
  summarise(effect = mean(m))


########################################### UTILITY
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

perm_test_subject <- function(mat, obs, summary_f, summary_f_args = list(iv = 'iv', dv = 'dv'), 
                              n_perm = 10^4, two.sided = TRUE) {
  inner_perm <- function(iteration, mat, summary_f, summary_f_args) {
    n_trials <- nrow(mat)
    mat[,summary_f_args$dv] <- mat[sample(n_trials),summary_f_args$dv]
    return (summary_f(mat, summary_f_args))
  }
  
  if('iv2' %in% summary_f_args) {
    resamp_f_args <- summary_f_args
    resamp_f_args$iv = resamp_f_args$iv2 
    resample_d <- function(iteration, mat) {
      summary_f(mat[sample(nrow(mat), replace = TRUE), ], resamp_f_args)
    }
    conds <- unique(mat[,summary_f_args$iv])
    library(doParallel)
    iv1_ds <- sapply (1:n_perm, resample_d, mat=mat[mat[,summary_f_args$iv] == conds[1],])
    iv2_ds <- sapply (1:n_perm, resample_d, mat=mat[mat[,summary_f_args$iv] == conds[1],])
    null_dist <- sample(c(-1,1), n_perm, replace = TRUE) * (iv1_ds - iv2_ds)
  } else {
    null_dist <- sapply(1:n_perm, inner_perm, mat = mat, 
                        summary_f = summary_f, summary_f_args = summary_f_args)
    
  }
  p_value <- mean(obs < null_dist, na.rm=TRUE)
  if(two.sided) {p_value <- 2 * min(p_value, 1 - p_value)}
  return (p_value)
}

summary_f <- function(mat) {
  stein_sum_f_args <- list(iv = 'iv2', dv = 'dv')
  return (get_participant_SDT_d(mat, stein_sum_f_args))
}
test_f <- function(mat) {
  stein_sum_f_args <- list(iv = 'iv2', dv = 'dv')
  conds <- unique(mat$iv)
  obs_d_diff <- get_participant_SDT_d(mat[mat$iv == conds[1],], stein_sum_f_args) - 
    get_participant_SDT_d(mat[mat$iv == conds[2],], stein_sum_f_args)
  
  return(perm_test_subject(as.matrix(mat), obs_d_diff, get_participant_SDT_d,  summary_f_args =  
                             list(iv = 'iv', dv = 'dv', iv2 = 'iv2')))
}

########################################### UTILITY

# summary_f(data_exp3_loc[data_exp3_loc$idv == 1 & data_exp3_loc$iv == 1,])
res <- data %>% filter(exp == 'Zerweck et al_2021_2_20') %>% group_by(idv) %>% 
  group_modify(~data.frame(p = test_f(.x)))