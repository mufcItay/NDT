library(weaknull)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(grDevices)
library(gtable)
library(data.table)
library(grid)
library(scales)
source('datasets_analysis\\definitions.R')

# set alpha value
alpha <- .05
invalid_quid_res <- INVALID_VALUE_CODE


#' generate_NDT_plot
#' The function generates the sign-consistency (non-directional test) results sub-plot
#' @param data a dataframe with the results of the sign-consistency solution. The shape of 
#' the dataframe is (#Datasets) X (exp, p), where 'exp' is the name of the experiment (and
#' respective dataset), and 'p' is the p-value according to the sign-consistency solution. 
#' @param graphics_conf a list with different graphics configurations to be used by
#' @param alpha an alpha value highlight significant results
#' @param eps an epsilon value to be used when transforming p-values to log values 
#'
#' @return the plot describing the results of the (non-directional) sign-consistency test
generate_NDT_plot <- function(data, graphics_conf, alpha = .05, is_percent = TRUE) {
  scale <- ifelse(is_percent, 100, 1)
  data <- data %>%
    mutate(effect = p < alpha)
  plt <- ggplot(data, aes(x = exp, y = stat * scale, fill = effect)) +
    xlab('Experiment') +
    ylab(graphics_conf$y_title) +
    geom_point(size = 3, shape=21, 
               colour = 'black', stroke =2) +
    geom_errorbar(aes(ymin = ci.low * scale, ymax = ci.high * scale), width = 0.5) +
    ylim(min(data$ci.low) * scale - .1, max(data$ci.high) * scale + .1) +
    geom_hline(yintercept = ifelse(is_percent, 50, 0), linetype='dotted') +
    theme_classic() +
    scale_fill_manual(breaks = c(TRUE,FALSE),
                       values=c(graphics_conf$significant_color,
                                graphics_conf$ns_color))+
    theme(legend.position = 'none',
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.text.x = element_text(angle = 90))
  
  return (plt)
}


#' generate_quid_plot
#' The function generates the QUID results sub-plot
#' @param data a dataframe with the results of the QUID solution. The shape of 
#' the dataframe is (#Datasets) X (exp, quid_bf), where 'exp' is the name of the experiment (and
#' respective dataset), and 'quid_bg' is the Bayes Factor according to the solution. 
#' @param graphics_conf a list with different graphics configurations to be used by
#' @param criteria an Bayesian factors criteria to highlight convincing effects
#' @param eps an epsilon value to be used when transforming p-values to log values 
#'
#' @return the plot describing the results of the QUID test
generate_quid_plot <- function(data, graphics_conf, criteria = 3, eps = 1/10^5) {
  data <- data %>%
    mutate(effect = ifelse(quid_bf > criteria, 'effect',
                           ifelse(quid_bf < 1/criteria, 'ns', 'uncertain')),
           log_bf = log10(quid_bf + eps))
  plt <- ggplot(data, aes(x = exp, y = log_bf, fill = effect)) +
    xlab('Experiment') +
    ylab(expression(atop('log(BF)', italic('global null')))) +
    geom_point(size = 7, shape=21, 
               colour = 'black', stroke =2) +
    geom_hline(yintercept = 0, linetype='dotted') +
    theme_classic() +
    scale_fill_manual(breaks = c('effect','uncertain','ns'),
                       values=c(graphics_conf$significant_color,
                                graphics_conf$med,
                                graphics_conf$ns_color))+
    theme(legend.position = 'none',
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.text.x = element_text(angle = 90)) +
    ylim(min(data$log_bf) - .5, max(data$log_bf) + .5)


  return (plt)
}

#' generate_pbt_plot
#' The function generates the PBT results sub-plot
#' @param data a dataframe with the results of the QUID solution. The shape of 
#' the dataframe is (#Datasets) X (exp, pbt.low, pbt.high), where 'exp' is the name of 
#' the experiment (and respective dataset), 'pbt.low' and 'pbt.high' are the lower,
#' and higher bounds of the HDI according to the solution. 
#' @param graphics_conf a list with different graphics configurations to be used by
#'
#' @return the plot describing the results of the PBT test
generate_pbt_plot <- function(data, graphics_conf) {
  data$effect <- data$pbt.low > 0
  plt <- ggplot(data, aes(x = exp, fill = effect)) +
    geom_errorbar(aes(ymin  = pbt.low * 100, ymax  = pbt.high*100),
                  width = .15, linewidth  = 1.5) +
    geom_point(aes(y = pbt.MAP*100), size = 5, shape=21, 
               colour = 'black', stroke =2,
               fill = ifelse(data$pbt.MAP > 0, graphics_conf$med_color,
                              graphics_conf$ns_color)) +
    xlab('Experiment') +
    ylab('Estimated prevalence\nof within-subject effects (%)') +
    geom_hline(yintercept = 0) +
    theme_classic() +
    scale_fill_manual(breaks = c(TRUE,FALSE),
                       values=c(graphics_conf$med_color,
                                graphics_conf$ns_color))+
    
    theme(legend.position = 'none',
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank()) +
  ylim(0, plyr::round_any(max(data$pbt.high) * 100, accuracy = 0.1, f = ceiling))
  
  return (plt)
}

get_initials <- function(name, nchar=1, sep = '_', co_sep = '&') {
  only_name <- str_split(name, sep)[[1]][1]
  names <- str_split(only_name, co_sep)[[1]]
  return(paste(sapply(names, function(s) toupper(substr(trimws(s), 1, nchar))), collapse = ''))
} 

# configure the graphics of the figure
graphics_conf <- list(size_seg = 2, color_spreading_lines = '#71E9CC',
                      margin_y_subj = 0.5, margin_y_conds = 0.125, legnth_med = 2,
                      ns_color = 'white', significant_color = 'red', 
                      med_color = 'gray',
                      vline_size = 1, x_title_size = 22, x_text_size = 16)
# read the results of the UC database 
res_summary_fn <- 'UCDB_Results.csv'
results <- read.csv(paste('results', res_summary_fn, sep=.Platform$file.sep))
results$long_exp_name <- results$exp
short_exp_name <- sapply(results$exp, get_initials)
results$exp <- paste0(short_exp_name, data.table::rowid(short_exp_name)) 

# get only n.s results in the directional test for effects for which we could use all tests
ns_results_RT <- results %>%
  filter(quid_bf != invalid_quid_res, directional_effect.p > alpha)
# generate the PBT sub-plot
pbt_res <- ns_results_RT %>% dplyr::select(exp, pbt.high,pbt.low, pbt.MAP)
plt_pbt <- generate_pbt_plot(pbt_res,graphics_conf)
plt_pbt
n_effect_MAP <- sum(pbt_res$pbt.MAP > 0)
n_null_MAP <- nrow(pbt_res) - n_effect_MAP 
paste('N_Total, N_Effect, N_Null =', n_effect_MAP + n_null_MAP, ',',
      n_effect_MAP, ',', n_null_MAP)
paste('Max, Med,IQR (MAP) =', max(pbt_res$pbt.MAP), median(pbt_res$pbt.MAP), ',', IQR(pbt_res$pbt.MAP))

# generate the QUID sub-plot
quid_res <- ns_results_RT %>% 
  dplyr::select(exp, quid_bf) %>%
  mutate(quid_bf = 1/quid_bf)
bf_criteria <- 3
plt_quid <- generate_quid_plot(quid_res,graphics_conf, criteria = bf_criteria)
plt_quid
n_effect_QUID <- sum(quid_res$quid_bf > bf_criteria)
n_null_QUID <- sum(quid_res$quid_bf < 1/bf_criteria)
paste('Max, Med,IQR (MAP) =', max(quid_res$quid_bf), median(quid_res$quid_bf), ',', IQR(quid_res$quid_bf))
inconclusive <- quid_res$quid_bf[(quid_res$quid_bf >= 1/bf_criteria) &
                                   (quid_res$quid_bf <= bf_criteria)]

# generate the NDT sub-plot
nondir_res <- ns_results_RT %>% 
  dplyr::select(exp, non_directional.p, non_directional.statistic, non_directional.ci_low, non_directional.ci_high) %>% 
  rename(p = non_directional.p, stat = non_directional.statistic,
         ci.low = non_directional.ci_low, ci.high = non_directional.ci_high)
plt_ns_nondir <- generate_NDT_plot(nondir_res,graphics_conf)
plt_ns_nondir


# generate the non / directional test figure
graphics_conf$y_title <- 'Sign-Consistency (%)'
nondir_res <- results %>% 
  dplyr::select(exp, non_directional.p, non_directional.statistic, non_directional.ci_low, non_directional.ci_high) %>% 
  rename(p = non_directional.p, stat = non_directional.statistic,
         ci.low = non_directional.ci_low, ci.high = non_directional.ci_high)
plt_nondir <- generate_NDT_plot(nondir_res,graphics_conf)
plt_nondir

graphics_conf$y_title <- 'Statistic'
dir_res <- results %>% dplyr::select(exp, directional_effect.p, directional_effect.statistic, directional_effect.ci_low, directional_effect.ci_high) %>% 
  rename(p = directional_effect.p, stat = directional_effect.statistic,
         ci.low = directional_effect.ci_low, ci.high = directional_effect.ci_high)
plt_dir <- generate_NDT_plot(dir_res,graphics_conf, is_percent = FALSE)
plt_dir <- plt_dir + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank())

# generate the directional test sub-plot - Non RT datasets
graphics_conf$y_title <- 'Statistic'
dir_res_nonRT <- results %>% 
  dplyr::select(exp, directional_effect.p, directional_effect.statistic, 
                directional_effect.ci_low, directional_effect.ci_high) %>% 
  rename(p = directional_effect.p, stat = directional_effect.statistic,
         ci.low = directional_effect.ci_low, ci.high = directional_effect.ci_high) %>%
  filter(startsWith(exp,'S') & !(exp %in% ns_results_RT$exp))
plt_dir_nonRT <- generate_NDT_plot(dir_res_nonRT,graphics_conf, is_percent = FALSE)
plt_dir_nonRT

# aggregate together the Bayesian tests resutlts (PBT & QUID)
plt_bayes <- grid.arrange(plt_pbt, plt_quid, ncol = 1)
ggsave('figures\\bayes_methods_res.svg', width=15, height=12,plot = plt_bayes)
# aggregate together the NHST tests resutlts (Sign-Consistency & Directional permutations)
ggsave('figures\\plt_sign_con__res.svg', width=10, height=8,plot = plt_ns_nondir)
plt_nhst <- grid.arrange(plt_dir, plt_nondir, ncol = 1, heights = c(0.5, 1))
ggsave('figures\\plt_nhst_methods_res.svg', width=15, height=12,plot = plt_nhst)


