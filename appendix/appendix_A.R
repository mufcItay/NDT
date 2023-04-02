library(dplyr)
library(tidyr)

# source the utilities script
apdx_fld <- 'appendix'
source(paste(apdx_fld, 'appendix_utils.R', sep = .Platform$file.sep))

## define the common simulation parameters
results_cols <- c('QUID', 'OANOVA')
# number of trials
N_t <- 100
# we assume that the equal variance condition sigma_w is lower than the sigma_w
# used in the unequal variability condition
sigma_w_equal = 1
sigma_w_unequal = 1000
sigma_w <- c(sigma_w_equal, sigma_w_unequal)
mu <- 0
# defines the number of simulations
max_seed <- 100

## define the specific simulation parameters
# FAs simulation
N_p_FAs <- 100
sigma_b_FAs = 0
conf_FAs <- initialize_simulation(N_p_FAs, N_t, sigma_b_FAs, sigma_w, mu, 
                                  max_seed, results_cols)
# Sensitivity simulation
N_p_sensitivity <- 30
sigma_b_sensitivity = 15
conf_sensitivity <- initialize_simulation(N_p_sensitivity, N_t, 
                                          sigma_b_sensitivity, sigma_w, mu, 
                                          max_seed, results_cols)

# define analysis function (common for both analyses)
variability_analysis <- function(conf, params, df, seed) {
  # get the low within participant sigma, which is the sigma used in the equal
  # variance condition
  equal_var_value <- min(conf$params$sigma_w)
  if(params$sigma_w != equal_var_value) {
    equal_var_df <- generate_dataset(p_mean = 0, p_sd = params$sigma_b, 
                                    N = params$N_p, trials_per_cnd = params$N_t, 
                                    wSEsd = equal_var_value, 
                                      seed = seed)
      df[df$idv%in% seq(1,params$N_p -1), 'dv'] <- 
        equal_var_df[equal_var_df$idv%in% seq(1, params$N_p -1), 'dv']
  }
  # run all tests
  QUID <- run_quid(df)
  OANOVA <- run_oanova_test(df)
  return(c(1 / QUID$quid_bf, OANOVA$p))
}

# run both simulations
results_df_FAs <- run_simulation(conf_FAs, variability_analysis)
results_df_sensitivity <- run_simulation(conf_sensitivity, variability_analysis)
# aggregate simulations results to a single data frame
results_df <- rbind(results_df_FAs, results_df_sensitivity)
# save the results to file
save_results(results_df, 'Appendix_A_QUID_OANOVA')

# analyze results - we use the bf_criteria to categorize iterations with
# moderate evidence for and against H0 (global null)
alpha = .05
bf_criteria <- 3
bf_criteria_high <- bf_criteria
bf_criteria_low <- 1/bf_criteria_high
# data frame structure:   rows      X ( Condition   ,    Analysis   , Result) 
#                      (iterations) X (equal/unequal, FA/Sensitivity, BF)
results_df <- results_df %>% 
  mutate(Condition = factor(sigma_w),
         Analysis = factor(sigma_b),
         Result_QUID = as.numeric(QUID),
         Result_OANOVA = as.numeric(OANOVA))

# analyze QUID's results (% of iterations with moderate evidence in 
# each analysis X condition combination)
analysis_quid <- results_df %>%
  group_by(Analysis, Condition) %>%
  summarise(sig_QUID = ifelse(Result_QUID <= bf_criteria_low, 'H0',
                                        ifelse(Result_QUID >= bf_criteria_high, 'H1',
                                               'Inconclusive'))) %>%
  group_by(Analysis, Condition,sig_QUID) %>%
  summarise(sig_prop = 100 * n() / max_seed)
analysis_quid

# analyze the OANOVA test's results (% of iterations with significant effects in 
# each analysis X condition combination)
analysis_OANOVA <- results_df %>%
  group_by(Analysis, Condition) %>%
  summarise(sig_OANOVA = Result_OANOVA <= alpha) %>%
  group_by(Analysis, Condition,sig_OANOVA) %>%
  summarise(sig_prop = 100 * n() / max_seed)
analysis_OANOVA