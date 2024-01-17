library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(signcon)

## APPENDIX H

# source the utilities script
apdx_fld <- 'appendix'
source(paste(apdx_fld, 'appendix_utils.R', sep = .Platform$file.sep))
analysis_fld <- 'datasets_analysis'
source(paste(analysis_fld, 'utils.R', sep = .Platform$file.sep))
source(paste(analysis_fld, 'definitions.R', sep = .Platform$file.sep))
uc_analysis_type <- init_analysis(Unconscious_Processing_analysis_lbl)
uc_dfs <- get_input_df(uc_analysis_type)

# get common numbers on UC studies for N and n
summary_uc_dfs <- uc_dfs %>%
  group_by(exp) %>%
  summarise(Np = length(unique(idv)), Nt = n() / length(unique(idv)))


## define the common simulation parameters
results_cols <- c('SC_p', 'SC_stat')
# set power analysis parameter combinations
apndx_H_percentiles <- c(.25,.5,.75)
N_p <- as.numeric(quantile(summary_uc_dfs$Np, apndx_H_percentiles))
N_t <- round(as.numeric(quantile(summary_uc_dfs$Nt, apndx_H_percentiles)))
sigma_b = seq(1,2, by = .5)
sigma_w <- 10
mu = c(0)
# defines the number of simulations
apndx_H_n_iterations <- 250
# note that we feed the initialization function with N_t /2 because it expects
# the number of trials per condition (we use two conditions)
apndx_H_conf <- initialize_simulation(N_p, N_t/2, sigma_b, sigma_w, mu, apndx_H_n_iterations, 
                              results_cols = results_cols)
# define the power analysis function
power_analysis <- function(conf, params, df, seed) {
  # run the sign-consistency and PBT tests
  res_sc <- test_sign_consistency(df, idv = 'idv', iv = 'iv', dv = 'dv', perm_repetitions = 100)
  # return the statistics of interest to store in the results data frame 
  return(c(res_sc[c('p', 'statistic')]))
}

# run both simulations
run_appendixH <- function(conf) {
  # get the power analysis results
  results_df <- run_simulation(conf, power_analysis)
  
  # create a summary data frame for the results:
  # count significant sign-consistency and HDI excludes zero (for PBT)
  alpha <- .05
  # data frame structure:
  # (condition) X (sign-consistency significance,  PBT's HDI exceeds zero)
  results_summary <- results_df %>%
    mutate(SC = SC_p <= alpha,
           N_t = N_t *2, sds_ratio = sigma_b/sigma_w) %>%
         group_by(mu, N_p, N_t, sds_ratio) %>%
         summarise_all(mean) %>%
    gather(Test, Power, SC)
  # save the summary of the results to file
  save_results(results_summary, 'Appendix_H')
  return(results_summary)
}

save_plot_appendixH <- function(results_summary) {
  # plot the results
  plt_appendix_H <- results_summary %>%
    mutate(Power = round(100 * Power),
           Test = factor(Test),
           mu = factor(ifelse(mu == 0, 'Non-directional differences','Directional effect'), 
                       levels = c('Directional effect', 'Non-directional differences')),
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
    scale_fill_gradientn(colors=c("seashell1", 'lightblue', 'royalblue1' ,'royalblue4'),
                         guide = 'colorbar') +
    facet_grid(vars(sds_ratio), vars(mu)) +
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
  save_plot(plt_appendix_H, fn = 'Appendix_H')
  return(plt_appendix_H)
}


