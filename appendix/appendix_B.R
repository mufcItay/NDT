rm(list = ls())

library(dplyr)
library(weaknull)
library(foreach)
library(tidyr)
library(weaknull)
library(progress)
source('datasets_analysis\\pbt.R')
source('appendix\\generate_dataset.R')

# a function that runs the individual-level test for effects used in PBT
pbt_test_f <- function(data) {
  conditions <- unique(data$iv)
  t_res <- t.test(data[data$iv == conditions[2],]$dv,
                  data[data$iv == conditions[1],]$dv)
  return(t_res$p.value)
}

# use a cluster to run power analysis in parallel for iterations within each
# paramters combination
pa.cluster <- parallel::makeCluster(
  parallel::detectCores() - 1, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = pa.cluster)

# set power analysis paramter combinations
sigma_w <- 1
N_p <- c(10, 30, 50)
N_t <- c(20, 100, 500)
sigma_b = c(.2)
mu = c(0, .1)
max_seed <- 10^3
seeds <- 1:max_seed

# create a grid of all parameter combinations
params_table <- crossing(N_p, N_t, sigma_b,mu)
# initialize a results grid to store the power analysis results
results_df <- crossing(N_p, N_t, sigma_b,mu, seeds)
results_cols <- c(paste('sc', c('p','stat'),sep = '_'),
                  paste('pbt', c('low_hdi95', 'high_hdi95', 'MAP'),sep = '_'))
results_df[,results_cols] <- -1
simulation_conditions <- nrow(params_table)
# initialize a progress bar to track the progress of the power analysis
pb <- txtProgressBar(min = 1, max = simulation_conditions, style = 3)
# iterate over parameter combinations
for (param_ind in 1:simulation_conditions) { 
  setTxtProgressBar(pb, param_ind) 
  # iterate over repetitions within each parameter combination
  res = foreach (seed = 1:max_seed, .combine = 'c',
                 .packages = c("dplyr", "weaknull", "nleqslv")) %dopar% {
                   set.seed(seed)
                   params <- params_table[param_ind,]
                   # create the datasets according to parameters
                   df <- generate_dataset(p_mean = params$mu, p_sd = params$sigma_b, seed = seed, 
                                            N = params$N_p, trials_per_cnd = params$N_t / 2, wSEsd = sigma_w)
                   # run all tests
                   res_sc <- test_sign_consistency(df, idv = 'idv', iv = 'iv', dv = 'dv')
                   res_pbt <- run_pbt(df, pbt_test_f)
                   c(res_sc[c('p', 'statistic')], 
                     res_pbt[c('low', 'high', 'MAP')])
                 }
  # save all results
  res_idx <- (param_ind - 1) * max_seed + 1:max_seed
  results_df[res_idx, results_cols] <- t(matrix(unlist(res), ncol = max_seed))
}
parallel::stopCluster(cl = pa.cluster)

# create a summary of the results
alpha <- .05
results_summary <- results_df %>%
  mutate(SC = sc_p <= alpha,
         PBT = pbt_low_hdi95 > 0) %>%
  group_by(mu, N_p, N_t) %>%
  summarise_all(mean) %>%
  mutate(Power = round(100 * Power),
         Test = factor(Test),
         mu = factor(mu),
         N_p = factor(N_p),
         N_t = factor(N_t))
results_summary
gather(key="Test", value="Power", 4:5)

# save the results to file
datetime <- paste(Sys.Date(),
                  str_replace_all(format(Sys.time(), "%X"), pattern = ':',replacement = '_')
                  , sep = '_')
write.csv(results_summary, file = paste0('appendix\\',datetime,'_power_analysis.csv'))


plt <- results_summary %>%
  ggplot(aes(fill=Power, 
             x = grp_N, y = p_N)) +
  geom_tile(colour = 'black', size = .5) +
  geom_text(aes(label = as.character(Power)),
            size = 4.5, color = 'black') +
  theme_minimal() + 
  xlab(expression(N[p])) +
  ylab(expression(N[t])) + 
  scale_fill_gradientn(colors=c('red',"grey", 'blue'),
                       guide = 'colorbar') +
  facet_grid(vars(Test), vars(grp_mu)) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(color = 'black', size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title=element_text(size=20),
        legend.text = element_text(size=14),
        legend.position = 'bottom',
        strip.placement = "outside") +
  guides(fill = guide_colourbar(barwidth = 20,
                                title="Power (%)"))
