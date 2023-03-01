rm(list = ls())

library(weaknull)
library(ggplot2)
library(dplyr)
library(ggtext)
library(tidyr)
apdx_fld <- 'appendix'
source(paste(apdx_fld, 'appendix_utils.R', sep = .Platform$file.sep))

## simulation
results_cols <- c('QUID')
N_t <- 100
sigma_w_equal = 1
sigma_w_unequal = 1000
sigma_w <- c(sigma_w_equal, sigma_w_unequal)
mu <- 0
max_seed <- 1000

# set simulation parameters for the FAs demonstration
N_p_FAs <- 100
sigma_b_FAs = 0
conf_FAs <- initialize_simulation(N_p_FAs, N_t, sigma_b_FAs, sigma_w, mu, 
                                  max_seed, results_cols)
# set simulation parameters for the sensitivity demonstration
N_p_sensitivity <- 30
sigma_b_sensitivity = 15
conf_sensitivity <- initialize_simulation(N_p_sensitivity, N_t, 
                                          sigma_b_sensitivity, sigma_w, mu, 
                                          max_seed, results_cols)

# define analysis function
variability_analysis <- function(conf, params, df, seed) {
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
  return(c(1 / QUID$quid_bf))
}

# run the simulation
results_df_FAs <- run_simulation(conf_FAs, variability_analysis)
results_df_sensitivity <- run_simulation(conf_sensitivity, variability_analysis)
results_df <- rbind(results_df_FAs, results_df_sensitivity)
# save the results to file
save_results(results_df, 'Appendix_A')

# analyze results
alpha = .05
bf_criteria <- 3
bf_criteria_high <- bf_criteria
bf_criteria_low <- 1/bf_criteria_high
results_df <- results_df %>% 
  mutate(Condition = factor(sigma_w),
         Analysis = factor(sigma_b),
         Result = as.numeric(QUID))
# analyze the results (% of iterations with significant/substantial results)
analysis_quid <- results_df %>%
  group_by(Analysis, Condition) %>%
  summarise(sig = ifelse(Result <= bf_criteria_low, 'H0',
                                        ifelse(Result >= bf_criteria_high, 'H1',
                                               'Inconclusive'))) %>%
  group_by(Analysis, Condition,sig) %>%
  summarise(sig_prop = 100 * n() / max_seed)

# plot the results
plot_BF_dists <- function(data, bf_criteria = 3) {
  bf_criteria_high <- bf_criteria
  bf_criteria_low <- 1/bf_criteria_high
  
  data %>%
  ggplot(aes(x = log10(Result), fill = Condition)) +
    geom_density(alpha = .5) +
    xlab('log<sub>10</sub>(BF)<br><i>global null</i> ↓') +
    geom_vline(xintercept = log10(c(bf_criteria_high, bf_criteria_low)), 
               linetype='solid', linewidth = 1, color = '#E9CB9A') +
    theme_classic() +
    scale_fill_manual(labels = c(bquote("Equal "~sigma[w]), 
                                 bquote("Unequal "~sigma[w])),
                      values = c('#3246a8', '#eb2d5f')) +
    theme(axis.title.x = element_markdown(size = 25),
          axis.title.y = element_markdown(size = 25),
          axis.text = element_text(size = 20),
          legend.position = 'top',
          legend.text = element_text(size = 26),
          legend.title = element_text(size = 26))
}

plt_appendix_A_FAs <- 
  plot_BF_dists(results_df %>% filter(sigma_b == sigma_b_FAs), bf_criteria)
plt_appendix_A_sensitivity <-
  plot_BF_dists(results_df %>% filter(sigma_b == sigma_b_sensitivity), bf_criteria)
        
plt_appendix_A_FAs
plt_appendix_A_sensitivity

save_plot(plt_appendix_A_FAs, fn = 'Appendix_A_FAs')
save_plot(plt_appendix_A_sensitivity, fn = 'Appendix_A_sensitivity')
