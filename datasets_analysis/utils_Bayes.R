library(ggplot2)
source('datasets_analysis\\quid.R')
source('datasets_analysis\\pbt.R')
plot_figure <- function(res, title, save = TRUE) {
  res %>%
    mutate(dir_sig = (directional_effect.p < .05),
           nondir_sig = (sign_con.p < .05)) %>%
    mutate(Result = ifelse(dir_sig, ifelse(nondir_sig, 'Both', 'Directional'),
                           ifelse(nondir_sig, 'Weaknull', 'Strongnull'))) %>%
    group_by(Result) %>%
    summarise(N = n(), N_prc = n() / nrow(res)) %>%
    ggplot(aes(x = Result, y = N, fill = Result, label = scales::percent(N_prc))) +
    geom_bar(stat = "identity")+
    geom_text(position = position_dodge(width = .9),    # move to center of bars
              vjust = 1.2,    # nudge above top of bar
              size = 7, color = 'white') +
    ggtitle(paste0(title, ' (N=',nrow(res), ')')) +
    scale_fill_brewer(palette="Dark2") +
    theme_minimal(base_size = 22)
  if(save) {
    ggsave(filename = paste0('figures\\',title,'.png'))
  }
}

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
    df <- df[!rowSums(is.na(df[names(df)])), ]
    df <- df[!rowSums(is.nan(is.nan(as.matrix(df)))), ]
    df <- analysis_conf@preprocess_f(df, basename(fn))
    
    return (df)
  })
  df_all <- do.call(rbind, dfs)
  return(df_all)
}

# retrieves the database to analyze (including all individual experiments)
get_sum_fs <- function(analysis_conf, experiments) {
  map_f_to_exp <- function(exp_name) {
    if(startsWith(exp_name,'Skora et al_2020')) {
      # d' as a dependent measure
      f <- function(mat) {
        rate <- (sum(mat[,'dv']) + .1) / (length(mat[,'dv']) + .1)
        return (qnorm(rate))
      }
    } else if(startsWith(exp_name,'Stein & van Peelen_2020')) {
      # d' as a dependent measure
      f <- function(mat) {
        mat[,'dv'] = mat[,'dv'] ==  mat[,'iv2']
        conditions <- unique(mat[,'iv2'])
        mat_1 <- mat[mat[,'iv2'] == conditions[1],]
        mat_2 <- mat[mat[,'iv2'] == conditions[2],]
        
        rate_1 <- (sum(mat_1[,'dv']) + .1) / (length(mat_1[,'dv']) + .1)
        rate_2 <- (sum(mat_2[,'dv']) + .1) / (length(mat_2[,'dv']) + .1)
        rate_1 <- ifelse(rate_1 == 1, rate_1 - .1/length(mat_1[,'dv']), rate_1)
        rate_2 <- ifelse(rate_2 == 1, rate_2 - .1/length(mat_2[,'dv']), rate_2)
        return (qnorm(rate_1) - qnorm(rate_2))
      }
    } else if(startsWith(exp_name,'Benthien & Hesselmann_2021')) {
      # interaction effect (using iv2 to simulate simple effects)
      f <- function(mat) {
        res<- stats::median(mat[mat[,'iv2'] == 0, 'dv']) - 
          stats::median(mat[mat[,'iv2'] == 1, 'dv'])
        return(res) 
        }
    } else {
      f <- function(mat) {
        return(analysis_conf@summary_f(mat[,'dv']))
      }
    }
  }
  df_to_f <- lapply(experiments, map_f_to_exp)
  names(df_to_f) <- experiments
  return(df_to_f)
}

run_analysis <-function(analysis_conf) {
  set.seed(analysis_conf@seed)
  dfs <- get_input_df(analysis_conf)
  sum_fs <- get_sum_fs(analysis_conf, unique(dfs$exp))
  # get a dataframe of the results 
  # (for the RT analysis we use Median as summary_function, and for the AUC analysis we use get_AUC)
  res <- dfs %>%
    group_by(exp) %>%
    group_modify(~data.frame(quid = run_quid(.x)[c('low','high')],
                             pbt = run_pbt(.x, stats::wilcox.test)[c('low','high')]))
  
  res_dir <- dirname(analysis_conf@results_fn)
  if(!dir.exists(res_dir)) {
    dir.create(res_dir, recursive = TRUE)
  }
  
  # write results to file
  write.csv(res, 'results\\BAYES_UCDB_Results.csv')  
}