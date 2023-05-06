library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(signcon)

# source the utilities script
apdx_fld <- 'appendix'
source(paste(apdx_fld, 'appendix_utils.R', sep = .Platform$file.sep))

# set power analysis parameter combinations
N_p <- c(10, 30, 50) # the number of participants
N_t <- c(20, 100, 500) # the number of experimental trials
sigma_b = 2
sigma_w <- 10
mu = c(0, 1)
# defines the number of simulations
max_seed <- 10^3
seeds <- 1:max_seed
results_cols <- c(paste('sc', c('p','stat'),sep = '_'),
                  paste('pbt', c('low_hdi95', 'high_hdi95', 'MAP'),sep = '_'))
# note that we feed the initialization function with N_t /2 because it expects
# the number of trials per condition (we use two conditions)
apndx_b_conf <- initialize_simulation(N_p, N_t/2, sigma_b, sigma_w, mu, max_seed, 
                              results_cols = results_cols)
# define the power analysis function
power_analysis <- function(conf, params, df, seed) {
  # run the sign-consistency and PBT tests
  res_sc <- test_sign_consistency(df, idv = 'idv', iv = 'iv', dv = 'dv')
  res_pbt <- run_pbt(df, pbt_test_f)
  # return the statistics of interest to store in the results data frame 
  return(c(res_sc[c('p', 'statistic')], 
    res_pbt[c('low', 'high', 'MAP')]))
}

# run both simulations
run_appendixB <- function(conf) {
  # get the power analysis results
  results_df <- run_simulation(conf, power_analysis)
  
  # create a summary data frame for the results:
  # count significant sign-consistency and HDI excludes zero (for PBT)
  alpha <- .05
  # data frame structure:
  # (condition) X (sign-consistency significance,  PBT's HDI exceeds zero)
  results_summary <- results_df %>%
    mutate(SC = sc_p <= alpha,
           PBT = pbt_low_hdi95 > 0,
           N_t = N_t *2) %>% # switch from trials per condition to total #trials
    group_by(mu, N_p, N_t) %>%
    summarise_all(mean) %>%
    gather(Test, Power, SC:PBT)
  # save the summary of the results to file
  save_results(results_summary, 'Appendix_B')
  return(results_summary)
}

save_plot_appendixB <- function(results_summary) {
  # plot the results
  plt_appendix_B <- results_summary %>%
    mutate(Power = round(100 * Power),
           Test = factor(Test),
           mu = factor(ifelse(mu == 0, 'Non-directional differences',
                              'Directional effect ')),
           N_p = factor(N_p),
           N_t = factor(N_t)) %>%
    ggplot(aes(fill=Power, 
               x = N_p, y = N_t)) +
    geom_tile(colour = 'black', linewidth = .5) +
    geom_text(aes(label = as.character(Power)),
              size = 7, color = 'black') +
    theme_minimal() + 
    xlab(expression(N[p])) +
    ylab(expression(N[t])) + 
    scale_fill_gradientn(colors=c('red',"grey", 'blue'),
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
  save_plot(plt_appendix_B, fn = 'Appendix_B')
  return(plt_appendix_B)
}


