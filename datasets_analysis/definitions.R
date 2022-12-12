# invalid value code
INVALID_VALUE_CODE <- -99999

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
    sum_fs = 'function',
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
                   sum_fs,
                   n_samp = 1000,
                   seed = 101) {
            .Object@input_source <- input_source
            .Object@db_output_fn <- db_output_fn
            .Object@results_fn <- results_fn
            # define a preprocessing function for the analysis (dataset specific preprocessing)
            .Object@preprocess_f <- preprocess_f
            .Object@summary_f <- summary_f
            .Object@n_samp <- n_samp
            .Object@sum_fs <- sum_fs
            .Object@seed <- seed
            return(.Object)
          })


init_analysis <- function() {
  # get directiory path
  dir_path <- 'datasets_analysis'
  defs_prefix <- 'definitions'

  ## Confidence analysis
  source(paste(dir_path, paste0(defs_prefix, '_confidence.R'), 
               sep = .Platform$file.sep))
  # create an instance of the analysis configuration for the confidence DB
  confidence_analysis_conf <- new(analysisConfCName, 'datasets\\cdb\\Usable\\', 
                                  'results\\Confidence_DB.csv', 'results\\Confidence_Results.csv',
                                  preprocess_dfs_cdb, base::mean,
                                  sum_fs = get_sum_fs_confidence)
  
  
  ## AUC analysis
  source(paste(dir_path, paste0(defs_prefix, '_AUC.R'), 
               sep = .Platform$file.sep))
  AUC_analysis_conf <- new(analysisConfCName, 'datasets\\cdb\\Usable\\',
                           'results\\AUC_DB.csv', 'results\\AUC_Results.csv',
                           preprocess_dfs_AUC, get_AUC, 
                           sum_fs = get_sum_fs_AUC)
  ## UC analysis
  source(paste(dir_path, paste0(defs_prefix, '_UCDB.R'), 
               sep = .Platform$file.sep))
  UCID_analysis_conf <- new(analysisConfCName, 'datasets\\ucdb\\',
                            'results\\UC_DB.csv', 'results\\UCDB_Results.csv', 
                            preprocess_dfs_UC, stats::median,
                            sum_fs = get_sum_fs_UC)
  
  ## cogdb analysis
  source(paste(dir_path, paste0(defs_prefix, '_cogdb.R'), 
               sep = .Platform$file.sep))
  cogdb_analysis_conf <- new(analysisConfCName, 'datasets\\cogdb\\',
                             'results\\UC_DB.csv', 'results\\cogdb_Results.csv', 
                             preprocess_dfs_cogdb, stats::median,
                             sum_fs = get_sum_fs_cogdb)
}
