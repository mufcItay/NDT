#' generate_dataset
#' @description The function generates mock data for tests and examples 
#' according to the arguments.
#' @param p_mean the effect's population mean
#' @param p_sd the standard deviation of the population's effect
#' @param seed - a seed to use when generating the resulting data frame
#' @param N - the number of simulated participants
#' @param trials_per_cnd - the number of simulated trials per condition
#' @param wSEsd - the standard deviation of the dependent measure (within subject error term)
#' @param dv_offset - an offset added to the dependent variable
#'
#' @return a data frame with three columns: id (participant id), 'iv' (condition label), and 'dv' (the dependent variable).
generate_dataset <- function(p_mean, p_sd, seed = 1, N = 30, 
                             trials_per_cnd = 100, wSEsd = 2,
                             dv_offset = 0) {
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
  dv <- stats::rnorm(length(idv), effect_baseline, within_subj_effect_sd) + 
    iv * subj_true_effect_per_trial
  # add offset to the depdent variable
  dv <- dv + dv_offset
  # create a dataframe based on the three columns generated above
  sampled_data <- data.frame(idv, iv, dv)
  return (sampled_data)
}
