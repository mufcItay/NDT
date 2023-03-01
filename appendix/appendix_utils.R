library(weaknull)
library(ggplot2)
library(dplyr)
library(foreach)
library(stringr)

# load sources from folders
apdx_fld <- 'appendix'
analysis_fld <- 'datasets_analysis'
results_fld <- paste(apdx_fld, 'results', sep = .Platform$file.sep)

source(paste(analysis_fld, 'quid.R', sep = .Platform$file.sep))
source(paste(analysis_fld, 'pbt.R', sep = .Platform$file.sep))
# create the results folder if it does not exist
if(!dir.exists(results_fld)) {
  dir.create(results_fld)
}

## functions
# a function that runs the individual-level test for effects used in PBT
pbt_test_f <- function(data) {
  conditions <- unique(data$iv)
  t_res <- t.test(data[data$iv == conditions[2],]$dv,
                  data[data$iv == conditions[1],]$dv)
  return(t_res$p.value)
}

# the function creates a cluster to run iterations of the simulation in parallel.
# the argument 'n_cores_from_max' sets the number of cores to be used as:
# number of cores - n_cores_from_max
get_cluster <- function(n_cores_from_max = 1) {
  # use a cluster to run the analysis in parallel
  cluster <- parallel::makeCluster(
    parallel::detectCores() - n_cores_from_max, 
    type = "PSOCK"
  )
  parallel::clusterExport(cluster, c("generate_dataset", "run_quid", "run_pbt",
                                     "quid", "prep.models", "make.bf", "prior.p.greater",
                                     "pbt_test_f", "bayesprev_hpdi", "bayesprev_map"))
  doParallel::registerDoParallel(cl = cluster)
  return(cluster)
}

# the function initializes the configuration for the simulation
initialize_simulation <- function(N_p, N_t, sigma_b, sigma_w, mu, max_seed,
                                  results_cols) {
  # create a grid of all parameter combinations
  params <- crossing(N_p, N_t, sigma_b, sigma_w, mu)
  # initialize a results grid to store the power analysis results
  seeds <- 1:max_seed
  results <- crossing(N_p, N_t, sigma_b, sigma_w, mu, seeds)
  results[,results_cols] <- -1
  
  return(list(params = params, results = results, results_cols = results_cols))
}

#' Create Sample Data
#' @description The function generated mock data for tests and examples according to the arguments
#' @param p_mean the effect's population mean
#' @param p_sd the standard deviation of the population's effect
#' @param seed - a seed to use when generating the resulting data frame
#' @param N - the number of simulated participants
#' @param trials_per_cnd - the number of simulated trials per condition
#' @param wSEsd - the standard deviation of the dependent measure (within subject error term)
#'
#' @return a data frame with three columns: id (participant id), 'iv' (condition label), and 'dv' (the dependent variable)
generate_dataset <- function(p_mean, p_sd, seed = 1, N = 30, trials_per_cnd = 100, wSEsd = 2) {
  set.seed(seed)
  # 0 = faster/smaller condition (e.g., 'congruent'), 1 = slower/larger condition (e.g., 'incongruent'),
  conditionLabels <- c(0,1)
  # define the number of trials across all conditions
  trialsN <- trials_per_cnd * length(conditionLabels)
  
  # define the baseline dependent measure statistical features
  effect_baseline <- 0
  within_subj_effect_sd <- wSEsd
  
  # define the effect statistical features
  population_sd = p_sd
  population_mean = p_mean
  
  # create an id column for the samples data
  idv <- rep(1:N, each = trialsN)
  # create a independent variable column
  iv <- rep(rep(conditionLabels, each = trials_per_cnd), N)
  
  # sample effects for each subject
  subj_true_effect <- stats::rnorm(N,population_mean,population_sd)
  # sample effects for each subject and trial
  subj_true_effect_per_trial <- rep(subj_true_effect, each = trialsN)
  # set the dependent variable columns according to baseine, the true effect, and the indepdent variable
  dv <- stats::rnorm(length(idv), effect_baseline, within_subj_effect_sd) + iv * subj_true_effect_per_trial
  # create a dataframe based on the three columns generated above
  sampled_data <- data.frame(idv, iv, dv)
  return (sampled_data)
}

# the functions applies the 'inner_sim_f' function to the generated data
# according to each seed
run_simulation <- function(conf, inner_sim_f, cluster = NULL) {
  if(is.null(cluster)) {
    cluster <- get_cluster()
  }
  seeds <- unique(conf$results$seeds)
  n_sim_conditions <- nrow(conf$params)
  # initialize a progress bar to track the progress of the power analysis
  pb <- txtProgressBar(min = 1, max = n_sim_conditions, style = 3)
  # iterate over parameter combinations
  for (param_ind in 1:n_sim_conditions) { 
    # iterate over repetitions within each parameter combination
    res = foreach (seed = seeds, .combine = 'c',
                   .packages = c("dplyr", "weaknull", "nleqslv")) %dopar% {
                     set.seed(seed)
                     params <- conf$params[param_ind,]
                     # create the datasets according to parameters
                     df <- generate_dataset(p_mean = params$mu, 
                                            p_sd = params$sigma_b, seed = seed, 
                                            N = params$N_p, 
                                            trials_per_cnd = params$N_t, 
                                            wSEsd = params$sigma_w)
                     inner_sim_f(conf, params, df, seed)
                     }
    # save all results
    res_idx <- (param_ind - 1) * max(seeds) + seeds
    conf$results[res_idx, conf$results_cols] <- 
      t(matrix(unlist(res), ncol = max(seeds)))
    setTxtProgressBar(pb, param_ind) 
    
  }
  parallel::stopCluster(cl = cluster)
  return(conf$results)
}

# the function saves the dataframe to file according to a fixed format
save_results <- function(results, postfix) {
  # save the results to file
  datetime <- paste(Sys.Date(), str_replace_all(format(Sys.time(), "%X"), 
                                                pattern = ':',replacement = '_'), 
                    sep = '_')
  write.csv(results, file = paste(apdx_fld, paste0(datetime,postfix,'.csv'),
                                  sep = .Platform$file.sep))
}

# the function saves the plot to file according to a fixed format
save_plot <- function(plot, fn) {
  ggsave(paste(apdx_fld, paste0(fn,'.svg'), sep = .Platform$file.sep),
         width=15, height=12,plot = plot)
}

