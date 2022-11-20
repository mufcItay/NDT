# a class defining the graphics configuration for plots
analysisConfCName <- "analysisConf"
#' 'analysisConfCName' (according to the parameter value)
#' a class including graphics parameters for plotting figures
#'
#' @slot input_source the format of figures (.svg /.png /...) 
#' @slot db_output_fn number of width pixels in figure
#' @slot preprocess_f number of height pixels in figure
#' @slot summary_f resolution of the figure
setClass(
  analysisConfCName,
  slots = list(
    input_source = "character",
    db_output_fn = "character",
    results_fn = "character",
    preprocess_f = "function",
    summary_f = "function",
    n_samp = "numeric",
    seed = "numeric"
  )
)
setMethod("initialize", analysisConfCName,
          function(.Object,
                   input_source,
                   db_output_fn,
                   results_fn,
                   preprocess_f,
                   summary_f,
                   n_samp = 1000,
                   seed = 101) {
            .Object@input_source <- input_source
            .Object@db_output_fn <- db_output_fn
            .Object@results_fn <- results_fn
            # define a preprocessing function for the analysis (dataset specific preprocessing)
            .Object@preprocess_f <- preprocess_f
            .Object@summary_f <- summary_f
            .Object@n_samp <- n_samp
            .Object@seed <- seed
            return(.Object)
          })

## Confidence analysis
prep_confidence <- function(df, ds_name) {
  if(length(unique(df$Response)) != 2) {
    return (data.frame())
  }
  # the 'name' of the effect to analyze
  df$Exp <- rep(ds_name, nrow(df))
  # rename and select the relevant columns
  df <- df %>% 
    rename(id = Subj_idx, iv = Response, dv1 = Confidence, exp = Exp) %>% 
    dplyr::select(exp, id, iv, dv1)
  #exclude subjects from experiments if the have too few trials in each cell
  exclusions <- df %>% 
    mutate(iv = factor(iv)) %>% 
    group_by(id, iv) %>% 
    count(id, name = "n", .drop = F) %>% 
    filter (n < 2) %>% 
    pull(id)
  if(length(exclusions)) { df <- df %>% filter(! id %in% exclusions) }
  
  return (df)
}
# create an instance of the analysis configuration for the confidence DB
cofidence_analysis_conf <- new(analysisConfCName, 'datasets\\cdb\\Usable\\', 
                               'results\\Confidence_DB.csv', 'results\\Confidence_Results.csv',
                               prep_confidence, base::mean)


## AUC analysis
# a summary function for the calculation of AUC
get_AUC <- function(mat){
  if(length(dimnames(mat) [[2]]) <2) {
    # not trials overall
    return(NA)
  }
  accuracy <- mat[,'accuracy']
  if(length(unique(accuracy)) < 2) {
    # not enough accuracy levels
    return(NA)
  }
  confidence <- mat[,'confidence']
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
  df$Exp <- rep(ds_name, nrow(df))
  # rename and select the relevant columns
  df <- df %>% rename(id = Subj_idx, accuracy = Accuracy, confidence = Confidence, iv = Response, exp = Exp) %>% select(exp, id,accuracy, confidence, iv)
  #exclude subjects from experiments if the have too few trials in each cell
  exclusions <- df %>% 
    mutate(iv = factor(iv), accuracy = factor(accuracy)) %>% 
    group_by(id, iv, accuracy) %>% 
    count(id, name = "n", .drop = F) %>% 
    filter (n < 10) %>% 
    pull(id)
  if(length(exclusions)) { df <- df %>% filter(! id %in% exclusions) }
  
  return(df)
}
AUC_analysis_conf <- new(analysisConfCName, 'datasets\\ConfDB_AUC.csv',
                         'results\\AUC_DB.csv', 'results\\AUC_Results.csv', preprocess_dfs_AUC, get_AUC)


## UC analysis

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

UCID_analysis_conf <- new(analysisConfCName, 'datasets\\ucdb\\',
                        'results\\UC_DB.csv', 'results\\UCDB_Results.csv', 
                        preprocess_dfs_UC, stats::median)
