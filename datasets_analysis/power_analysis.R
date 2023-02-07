library(dplyr)
library(weaknull)
library(foreach)
library(tidyr)
library(weaknull)
source('datasets_analysis\\pbt.R')

## from the weak null packages (not included)
#' Create Sample Data
#' @description The function generated mock data for tests and examples according to the arguments
#' @param p_mean the effect's population mean
#' @param p_sd the standard deviation of the population's effect
#' @param seed - a seed to use when generating the resulting data frame
#' @param N - the number of simulated participants
#' @param trials_per_cnd - the number of simulated trials per condition
#' @param wSEsd - the standard deviation of the dependent measure (within subject error term)
#'
#' @return a data frame with three columns: id (participant id), 'condition' (condition label), and 'var' (the dependent variable)
create_sample_data <- function(p_mean, p_sd, seed = 1, N = 30, trials_per_cnd = 100, wSEsd = 2) {
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

pbt_test_f <- function(data) {
  conditions <- unique(data$iv)
  t_res <- t.test(data[data$iv == conditions[2],]$dv,
                  data[data$iv == conditions[1],]$dv)
  return(t_res$p.value)
}

pa.cluster <- parallel::makeCluster(
  parallel::detectCores() - 1, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)

p_sigma <- 1
grp_N <- c(50)
p_N <- c(20,500)
grp_sig = c(.2)
grp_mu = c(.1)
max_seed <- 100
seeds <- 1:max_seed

# create a grid of all parameter combinations
params_table <- crossing(grp_N, p_N, grp_sig,grp_mu)
results_df <- crossing(grp_N, p_N, grp_sig,grp_mu, seeds)
results_cols <- c(paste('sc', c('p','stat'),sep = '_'),
                  paste('pbt', c('low_hdi95', 'high_hdi95', 'MAP'),sep = '_'),
                  paste('ttest', c('p','stat'),sep = '_'))
results_df[,results_cols] <- -1
simulation_conditions <- nrow(params_table)
for (param_ind in 1:simulation_conditions) {
  res = foreach (seed = 1:max_seed, .combine = 'c',
                 .packages = c("dplyr", "weaknull", "nleqslv")) %dopar% {
    set.seed(seed)
    params <- params_table[param_ind,]
    
    # create the datasets for the simulation
    df <- create_sample_data(p_mean = params$grp_mu, p_sd = params$grp_sig, seed = seed, 
                       N = params$grp_N, trials_per_cnd = params$p_N / 2, wSEsd = p_sigma)
    # run all tests
    res_sc <- test_sign_consistency(df, idv = 'idv', iv = 'iv', dv = 'dv')
    res_pbt <- run_pbt(df, pbt_test_f)
    res_ttest <- df %>%
      group_by(idv, iv) %>%
      summarise(mdv = mean(dv)) %>%
      group_by(idv) %>%
      summarise(effect = diff(mdv)) %>%
      pull(effect) %>%
      t.test()
    c(res_sc[c('p', 'statistic')], 
      res_pbt[c('low', 'high', 'MAP')], 
      res_ttest[c('p.value', 'statistic')])
    }
  res_idx <- (param_ind - 1) * max_seed + 1:max_seed
  results_df[res_idx, results_cols] <- t(matrix(unlist(res), ncol = max_seed))
}

parallel::stopCluster(cl = pa.cluster)

alpha <- .05
results_summary <- results_df %>%
  mutate(SC_sig = sc_p <= alpha,
         PBT_sig = pbt_low_hdi95 > 0,
         ttest_sig = ttest_p  <= alpha) %>%
  group_by(grp_N, p_N) %>%
  summarise_all(mean) %>%
  dplyr::select(grp_N, p_N, grp_N, p_N, SC_sig, PBT_sig, ttest_sig)
results_summary

