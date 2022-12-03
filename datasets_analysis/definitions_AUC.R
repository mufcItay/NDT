library(pracma)
# a summary function for the calculation of AUC
get_AUC <- function(mat){
  if(length(dimnames(mat) [[2]]) <2) {
    # not enough trials overall
    return(NA)
  }
  
  accuracy <- mat[,'iv2']
  if(length(unique(accuracy)) < 2) {
    # not enough accuracy levels
    return(NA)
  }
  confidence <- mat[,'dv']
  accuracy <- accuracy[order(confidence, decreasing=TRUE)]
  AUC <- trapz(cumsum(!accuracy)/sum(!accuracy),
               cumsum(accuracy)/sum(accuracy))
  return(AUC)
}

# create an instance of the analysis configuration for the confidence DB
preprocess_dfs_AUC <- function(df, ds_name) {
  if((!'Accuracy' %in% names(df)) || length(unique(df$Response)) != 2) {
    return (data.frame())
  }
  # the 'name' of the effect to analyze
  df$exp <- rep(ds_name, nrow(df))
  # rename and select the relevant columns
  df <- df %>% 
    rename(idv = Subj_idx, iv2 = Accuracy, dv = Confidence, iv = Response) %>% 
    dplyr::select(exp, idv, dv, iv2, iv) %>%
    drop_na()
  #exclude subjects from experiments if the have too few trials in each cell
  exclusions <- df %>% 
    mutate(iv = factor(iv), iv2 = factor(iv2)) %>% 
    group_by(idv, iv, iv2) %>% 
    count(idv, name = "n", .drop = F) %>% 
    filter (n < 10) %>%
    pull(idv)
  if(length(exclusions)) { df <- df %>% filter(! idv %in% exclusions) }
  return(df)
}

# retrieves the database to analyze (including all individual experiments)
get_sum_fs_AUC <- function(analysis_conf, experiments) {
  map_f_to_exp <- function(exp_name) {
    summary_f <- function(mat) {
      return (analysis_conf@summary_f(mat))
    }
    test_f <- function(mat) {
      return(-999)
    }
    return(list(summary = summary_f, test = test_f))
  }
  df_to_f <- lapply(experiments, map_f_to_exp)
  names(df_to_f) <- experiments
  return(df_to_f)
}

