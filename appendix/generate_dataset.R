#' get dvs
#'
#' @description the function generated the dependent variable according
#' to the arguments 
#' @param subj_true_effect the true effect scores of each participant
#' @param iv the independent variable column generated for each trial
#' @param wSEsd the within participant SD
#' @param trials_per_cnd the number of trials of each participant in each condition
#' @param dist_type the type of distribution of the 'dv'. Possible values: "Normal"/"Wald" 
#' @param dv_offset the argument sets the shift of the dv distribution 
#' in all trials according
#'
#' @return a vector if the trial-by-trial dependent variable for 
#' the entire dataset ((#Participants X #Trials) X 1) 
get_dvs <- function(subj_true_effect, iv, wSEsd, trials_per_cnd,
                    dist_type = 'Normal', dv_offset = 650) {
  if(dist_type == 'Wald') {
    # non-decision time
    tau = 200
    baseline_mu = dv_offset - tau
    
    # Wald dist parameters conversion
    # calculate lambda based on mu and sigma, based on Anders et al., 2016
    # (see equations 4 and 5, which were further expanded)
    lambda_base <- get_wald_lambda(baseline_mu, wSEsd)
    lambda_effect <- get_wald_lambda(baseline_mu, wSEsd)
    
    # sample effects for each subject and trial
    subj_true_effect_per_trial <- rep(subj_true_effect, each = trials_per_cnd)
    # set the dependent variable columns according the true effect, and the indepdent variable
    dv_baseline <- extraDistr::rwald(length(iv)/2, mu = baseline_mu, 
                                     lambda = lambda_base)
    dv_effect <- extraDistr::rwald(length(iv)/2, mu = baseline_mu + subj_true_effect_per_trial, 
                                   lambda = lambda_effect)
    # interleave baseline and vector conditions
    dv <- tau + rbind(dv_baseline, dv_effect)
    attributes(dv) <- NULL
  } else if(dist_type == 'Normal') {
    # sample effects for each subject and trial
    subj_true_effect_per_trial <- rep(subj_true_effect, each = trials_per_cnd * 2)
    # set the dependent variable columns according to baseline, the true effect, and the indepdent variable
    dv <- stats::rnorm(length(iv), 0, wSEsd) + 
      iv * subj_true_effect_per_trial
    # add offset to the dependent variable
    dv <- dv + dv_offset
  }
  else { stop('Unknown distribution provided to \'get_dv\' function')}
  
  return(dv)
}

#' get Wald lambda
#' 
#' @description The function converts mu and sigma of a normal distribution
#' to a lambda parameter for a Wald distribution.

#' @param mu the mu of the normal distribution
#' @param sigma the sigma of a normal distribution
#'
#' @return the function returns the lambda parameter of a Wald distribution 
#' according to the given mu and sigma
get_wald_lambda <- function(mu, sigma) {
  return(mu^3/ sigma^2)
}

#' generate_dataset
#' @description The function generates mock data for tests and examples 
#' according to the arguments.
#' @param p_mean the effect's population mean
#' @param p_sd the standard deviation of the population's effect
#' @param seed a seed to use when generating the resulting data frame
#' @param N the number of simulated participants
#' @param trials_per_cnd the number of simulated trials per condition
#' @param wSEsd the standard deviation of the dependent measure (within subject error term)
#' @param dv_offset an offset added to the dependent variable
#' @param dist_type the type of distribution of the 'dv'. Possible values: "Normal"/"Wald" 
#'
#' @return a data frame with three columns: id (participant id), 'iv' (condition label), and 'dv' (the dependent variable).
generate_dataset <- function(p_mean, p_sd, seed = 1, N = 30, 
                             trials_per_cnd = 100, wSEsd = 2,
                             dv_offset = 650, dist_type = 'Normal') {
  set.seed(seed)
  # 0 = faster/smaller condition (e.g., 'congruent'), 1 = slower/larger condition (e.g., 'incongruent'),
  conditionLabels <- c(0,1)
  # define the number of trials across all conditions
  trialsN <- trials_per_cnd * length(conditionLabels)
  
  # define the baseline dependent measure statistical features
  effect_baseline <- 0
  
  # define the effect statistical features
  population_sd = p_sd
  population_mean = p_mean
  
  # create an id column for the samples data
  idv <- rep(1:N, each = trialsN)
  # create a independent variable column
  iv <- rep(rep(conditionLabels, trials_per_cnd), N)
  
  # sample effects for each subject
  subj_true_effect <- stats::rnorm(N,population_mean,population_sd)
  dv <- get_dvs(subj_true_effect, iv, wSEsd, trials_per_cnd,
                dist_type, dv_offset)
  # create a dataframe based on the three columns generated above
  sampled_data <- data.frame(idv, iv, dv)
  return (sampled_data)
}
