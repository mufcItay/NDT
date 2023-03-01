rm(list = ls())

library(dplyr)
library(ggplot2)
library(stringr)
library(weaknull)
library(tidyr)
library(weaknull)
apdx_fld <- 'appendix'
source(paste(apdx_fld, 'appendix_utils.R', sep = .Platform$file.sep))

# set power analysis parameter combinations
N_p <- c(10, 30, 50)
N_t <- c(20, 100, 500)
sigma_b = c(2)
sigma_w <- 10
mu = c(0, 1)
max_seed <- 10^4
seeds <- 1:max_seed
results_cols <- c(paste('sc', c('p','stat'),sep = '_'),
                  paste('pbt', c('low_hdi95', 'high_hdi95', 'MAP'),sep = '_'))
conf <- initialize_simulation(N_p, N_t, sigma_b, sigma_w, mu, max_seed, 
                              results_cols = results_cols)

power_analysis <- function(conf, params, df, seed) {
  # run all tests
  res_sc <- test_sign_consistency(df, idv = 'idv', iv = 'iv', dv = 'dv')
  res_pbt <- run_pbt(df, pbt_test_f)
  return(c(res_sc[c('p', 'statistic')], 
    res_pbt[c('low', 'high', 'MAP')]))
}

results_df <- run_simulation(conf, power_analysis)

# create a summary function for the results
alpha <- .05
results_summary <- results_df %>%
  mutate(SC = sc_p <= alpha,
         PBT = pbt_low_hdi95 > 0) %>%
  group_by(mu, N_p, N_t) %>%
  summarise_all(mean) %>%
  gather(Test, Power, SC:PBT)
save_results(results_summary, 'Appendix_B')

# plot the results
plt_appendix_B <- results_summary %>%
  mutate(Power = round(100 * Power),
         Test = factor(Test),
         mu = factor(ifelse(mu == 0, 'Qualitative differences',
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
        legend.title=element_text(size=26),
        legend.text = element_text(size=22),
        legend.position = 'bottom',
        strip.placement = "outside") +
  guides(fill = guide_colourbar(barwidth = 20,
                                title="Power (%)"))

save_plot(plt_appendix_B, fn = 'Appendix_B')