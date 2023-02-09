rm(list = ls())

library(weaknull)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)
source('appendix\\generate_dataset.R')
source('datasets_analysis\\quid.R')
source('datasets_analysis\\pbt.R')

# set power simulation parametres
N_p <- 30
N_t <- 100
sigma_b = 30
sigma_w_same = 50
sigma_w_diff = sigma_w_same * 5
max_seed <- 10
seeds <- 1:max_seed

# initialize a results grid to store the power analysis results
results_df <- data.frame(seed = rep(seeds,each = 2),
                         variability = rep(c('same','diff'), length(seeds)),
                         QUID = rep(-1, 2 * length(seeds)),
                         PBT = rep(-1, 2 * length(seeds)),
                         SC = rep(-1, 2 * length(seeds)))

# a function that runs the individual-level test for effects used in PBT
pbt_test_f <- function(data) {
  conditions <- unique(data$iv)
  t_res <- t.test(data[data$iv == conditions[2],]$dv,
                  data[data$iv == conditions[1],]$dv)
  return(t_res$p.value)
}

# the function generates datasets according to the parameters
generate_data <- function(seed, offset_rt = 650) {
  same_var_data <- generate_dataset(p_mean = 0, p_sd = sigma_b, N = N_p,
                                trials_per_cnd = N_t, wSEsd = sigma_w_same, 
                                seed = seed) %>%
    mutate(dv = dv + offset_rt)
  diff_var_data <- generate_dataset(p_mean = 0, p_sd = sigma_b, N = N_p,
                                    trials_per_cnd = N_t, wSEsd = sigma_w_diff, 
                                    seed = seed) %>%
    mutate(dv = dv + offset_rt)

  return(list(same = same_var_data, diff = diff_var_data))
}

for (seed in 1:max_seed) {
  # generate datasets for the same an different variance conditions
  res <- generate_data(seed)
  # run all tests
  QUID_same_var <- run_quid(res$same)
  QUID_diff_var <- run_quid(res$diff)
  PBT_same_var <- run_pbt(res$same, pbt_test_f)
  PBT_diff_var <- run_pbt(res$diff, pbt_test_f)
  SC_same_var <- test_sign_consistency(res$same, idv = 'idv', iv = 'iv', dv = 'dv')
  SC_diff_var <- test_sign_consistency(res$diff, idv = 'idv', iv = 'iv', dv = 'dv')
  # save the results
  # save same variability results
  results_df[seed * 2 - 1,] <- c(seed, 'Same', 1 / QUID_same_var$quid_bf, 
                         PBT_same_var$low, SC_same_var$p)
  # save different variability results
  results_df[seed * 2,] <- c(seed, 'Different', 1 / QUID_diff_var$quid_bf,
                         PBT_diff_var$low, SC_diff_var$p)
}


# save the results to file
datetime <- paste(Sys.Date(),
                  str_replace_all(format(Sys.time(), "%X"), pattern = ':',replacement = '_')
                  , sep = '_')
write.csv(results_df, file = paste0('appendix\\',datetime,'_QUID_simulation.csv'))
