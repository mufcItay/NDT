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
    analyze_exps_f = 'function',
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
                   analyze_exps_f,
                   n_samp = 10000,
                   seed = 101) {
            .Object@input_source <- input_source
            .Object@db_output_fn <- db_output_fn
            .Object@results_fn <- results_fn
            # define a preprocessing function for the analysis (dataset specific preprocessing)
            .Object@preprocess_f <- preprocess_f
            .Object@summary_f <- summary_f
            .Object@n_samp <- n_samp
            .Object@sum_fs <- sum_fs
            .Object@analyze_exps_f <- analyze_exps_f
            .Object@seed <- seed
            return(.Object)
          })


Confidence_analysis_lbl <- 'cdb'
Metacognitive_Sensitivity_analysis_lbl <- 'auc'
Unconscious_Processing_analysis_lbl <- 'ucdb'
Cognitive_Psychology_analysis_lbl <- 'cogdb'
all_analysis_types <- c(Confidence_analysis_lbl, Metacognitive_Sensitivity_analysis_lbl,
                        Unconscious_Processing_analysis_lbl, Cognitive_Psychology_analysis_lbl)
#' init_analysis
#' the function gets a string description of configurations and returns a
#' configuration object, accordingly.
#' @param type a string descriptor of the analysis to run
#' @return the configuration object that is relevant for the given analysis 'type'
init_analysis <- function(type) {
  # get directory path
  dir_path <- 'datasets_analysis'
  defs_prefix <- 'definitions'

  ## Confidence analysis
  if(type == Confidence_analysis_lbl) {
    source(paste(dir_path, paste0(defs_prefix, '_Confidence.R'), 
                 sep = .Platform$file.sep))
    # create an instance of the analysis configuration for the confidence DB
    conf <- new(analysisConfCName, 'datasets\\cdb\\CONFDB\\', 'results\\Confidence_DB.csv', 
                'results\\Confidence_Results.csv', preprocess_dfs_cdb, mean,
                sum_fs = get_sum_fs_confidence, analyze_exps_f = run_signcon_analyses)
  }
  
  
  ## Metacognitive Sensitivity analysis
  if(type == Metacognitive_Sensitivity_analysis_lbl) {
    source(paste(dir_path, paste0(defs_prefix, '_Metacognitive_Sensitivity.R'), 
                 sep = .Platform$file.sep))
    conf <- new(analysisConfCName, 'datasets\\cdb\\AUCDB\\',
                'results\\AUC_DB.csv', 'results\\Metacognitive Sensitivity_Results.csv',
                preprocess_dfs_AUC, get_AUC, sum_fs = get_sum_fs_AUC, 
                analyze_exps_f = run_signcon_analyses)
  }

  ## Unconscious Processing analysis
  if(type == Unconscious_Processing_analysis_lbl) {
      source(paste(dir_path, paste0(defs_prefix, '_Unconscious_Processing.R'), 
                 sep = .Platform$file.sep))
    conf <- new(analysisConfCName, 'datasets\\ucdb\\',
                'results\\UC_DB.csv', 'results\\Unconscious Processing_Results.csv', 
                preprocess_dfs_UC, mean, sum_fs = get_sum_fs_UC, 
                analyze_exps_f = run_all_analyses)
  }
  
  ## Cognitive Psychology analysis
  if(type == Cognitive_Psychology_analysis_lbl) {
      source(paste(dir_path, paste0(defs_prefix, '_Cognitive_Psychology.R'), 
                 sep = .Platform$file.sep))
    conf <- new(analysisConfCName, 'datasets\\cogdb\\','', 
                'results\\Cognitive Psychology_Results.csv', preprocess_dfs_cogdb, mean,
                sum_fs = get_sum_fs_cogdb, analyze_exps_f = run_signcon_analyses)
  }
  return(conf)
}

#' run_all_analyses
#' the function gets a dataset, runs all analyses (signcon, DT, PBT, QUID, OANOVA) 
#' and returns a data frame of the results
#' @param conf a configuration object to run all analyses according to
#' @param analysis_fs a list that maps between experiment labels and the analysis
#' function to use.  
#' @param data the data of the given experiment to analyze (#Participants * #Trials) X (#Recorded Variables)
#' @param exp_name the name of the experiment to analyze
#' @return a data frame with all of the results:
#' for signcon - (N_participants X (p-value, statistic, individual scores))
#' for DT - a single row with a p-value, statistic, low_ci and high_ci
#' for quid - a single row with the Bayesian factor for the comparison between the global null
#' model and the random effects model
#' for PBT - a single row with the lower and upper bound of the returned HDI, and
#' the MAP for the effect
#' for OAVONA - the p-value and F-values according to the test
run_all_analyses <- function(conf, analysis_fs, data, exp_name) {
  set.seed(conf@seed)
  signcon_res <- test_sign_consistency(data,'idv', c('dv','iv2'), 'iv',
                                       null_dist_samples = conf@n_samp,
                                       summary_function = analysis_fs[[exp_name]]$summary)
  directional_test_res <- test_directional_effect(data,'idv', c('dv','iv2'), 'iv',
                                                  null_dist_samples = conf@n_samp, ci_reps = 10^5,
                                                  summary_function = analysis_fs[[exp_name]]$summary)
  data.frame(
    signcon = signcon_res[c('p','statistic','null_dist')],
    directional_test = directional_test_res[c('statistic','p', 'ci_low', 'ci_high')],
    quid = run_quid(data)[c('quid_bf')],
    pbt = run_pbt(data, analysis_fs[[exp_name]]$test)[c('low','high','MAP')],
    oanova = run_oanova_test(data)
  )
}

#' run_signcon_analyses
#' the function gets a dataset, runs the signcon and directional analyses and returns
#' a dataframe of the results
#' @param conf a configuration object to run all analyses according to
#' @param analysis_fs a list that maps between experiment labels and the analysis
#' function to use.  
#' @param data the data of the given experiment to analyze
#' @param exp_name the name of the experiment to analyze
#' @return a data frame with all of the results:
#' for signcon - (N_participants X (p-value, statistic, individual scores))
#' for DT - a single row with a p-value, statistic, low_ci and high_ci
run_signcon_analyses <- function(conf, analysis_fs, data, exp_name) {
  set.seed(conf@seed)
  signcon_res <- test_sign_consistency(data,'idv', c('dv','iv2'), 'iv',
                                  null_dist_samples = conf@n_samp,
                                  summary_function = analysis_fs[[exp_name]]$summary)
  directional_test_res <- test_directional_effect(data,'idv', c('dv','iv2'), 'iv',
                                                  null_dist_samples = conf@n_samp, ci_reps = 10^5,
                                                  summary_function = analysis_fs[[exp_name]]$summary)
  data.frame(
    signcon = signcon_res[c('statistic','p')],
    directional_test = directional_test_res[c('statistic','p', 'ci_low', 'ci_high')])
  
}