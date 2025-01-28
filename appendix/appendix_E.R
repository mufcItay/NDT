library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(signcon)

## APPENDIX E

# source the utilities script
apdx_fld <- 'appendix'
source(paste(apdx_fld, 'appendix_utils.R', sep = .Platform$file.sep))

## define the common simulation parameters
results_cols <- c('SC_p', 'SC_statistic', 'ABS_ES_p', 'ABS_ES_statistic', 'GNT_p', 'GNT_ci_low', 'GNT_ci_high')
# set power analysis parameter combinations
N_p <- c(10, 30, 50) # the number of participants
N_t <- c(50, 100, 500) # the number of experimental trials
sigma_b = 1.5
sigma_w <- 10
mu = c(0, 1)
# defines the number of simulations
n_iterations <- 10^3
# note that we feed the initialization function with N_t /2 because it expects
# the number of trials per condition (we use two conditions)
apndx_e_conf <- initialize_simulation(N_p, N_t/2, sigma_b, sigma_w, mu, n_iterations, 
                                      results_cols = results_cols)

# define the power analysis function
power_analysis <- function(conf, params, df, seed) {
  # run the sign-consistency and GNT tests
  res_absolute_es <- test_absolute_es(df, idv = 'idv', iv = 'iv', dv = 'dv')
  res_sc <- test_sign_consistency(df, idv = 'idv', iv = 'iv', dv = 'dv', perm_repetitions = 100)
  res_gnt <- run_gnt(df)
  # return the statistics of interest to store in the results data frame
  return(c(res_sc[c('p', 'statistic')],
           res_absolute_es[c('p', 'statistic')],
           res_gnt[c('p', 'ci_low', 'ci_high')]))
}

# run both simulations
run_appendixE <- function(conf) {
  # get the power analysis results
  results_df <- run_simulation(conf, power_analysis)
  
  # create a summary data frame for the results:
  # count significant results
  alpha <- .05
  # data frame structure:
  # (condition) X (sign-consistency and GNT significance)
  results_summary <- results_df %>%
    mutate(
      SC = SC_p <= alpha,
      ABSES = ABS_ES_p <= alpha,
      GNT = GNT_p <= alpha,
      N_t = N_t *2) %>% # switch from trials per condition to total #trials
    group_by(mu, N_p, N_t) %>%
    summarise_all(mean) %>%
    gather(Test, Power, SC:GNT)
  # save the summary of the results to file
  save_results(results_summary, 'Appendix_E')
  return(results_summary)
}


save_plot_appendixE <- function(results_summary) {
  # process summary table for plot
  res_per_test_df <- results_summary %>%
    mutate(Power = round(100 * Power),
           Test = factor(Test),
           mu = factor(ifelse(mu == 0, 'Non-directional differences','Directional effect'), 
                       levels = c('Directional effect', 'Non-directional differences')),
           N_p = factor(N_p),
           N_t = factor(N_t))
  
  # plot the results
  plt_appendix_E <- res_per_test_df %>%
    ggplot(aes(fill=Power, 
               x = N_p, y = N_t)) +
    geom_tile(colour = 'black', linewidth = .5) +
    geom_text(aes(label = as.character(Power)),
              size = 7, color = 'black') +
    theme_minimal() + 
    xlab(expression(N[p])) +
    ylab(expression(N[t])) + 
    scale_fill_gradientn(colors=c("seashell1", 'lightblue', 'royalblue1' ,'royalblue4'),
                         guide = 'colorbar') +
    facet_grid(vars(Test), vars(mu)) +
    theme(strip.background = element_rect(fill = "white"),
          strip.text = element_text(color = 'black', size = 26),
          axis.title = element_text(size = 26),
          axis.text = element_text(size = 22),
          panel.grid = element_blank(),
          legend.title=element_text(size=26),
          legend.text = element_text(size=22),
          legend.position = 'bottom',
          strip.placement = "outside",
          panel.spacing=unit(.5, "lines")) +
    guides(fill = guide_colourbar(barwidth = 20,
                                  title="Power (%)"))
  # save the plot
  save_plot(plt_appendix_E, fn = 'Appendix_E')
}


