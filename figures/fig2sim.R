library(weaknull)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)
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
generate_NDT_plot <- function(data, graphics_conf, alpha = .05, eps = 1/10^5) {
  data <- data %>%
    mutate(effect = p < alpha, p = log10(p + eps))
  plt <- ggplot(data, aes(x = exp, y = p, color = effect)) +
    xlab('Experiment') +
    ylab(graphics_conf$y_title) +
    geom_point(size = 5) +
    ylim(min(data$p) - .1, max(data$p) + .1) +
    geom_hline(yintercept = log10(alpha), linetype='dotted') +
    theme_classic() +
    scale_color_manual(breaks = c(TRUE,FALSE),
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
    mutate(effect = quid_bf > criteria, log_bf = log10(quid_bf + eps))
  plt <- ggplot(data, aes(x = exp, y = log_bf, color = effect)) +
    xlab('Experiment') +
    ylab('log(BF)') +
    geom_point(size = 7) +
    geom_hline(yintercept = 0, linetype='dotted') +
    theme_classic() +
    scale_color_manual(breaks = c(TRUE,FALSE),
                       values=c(graphics_conf$significant_color,
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
  plt <- ggplot(data, aes(x = exp, color = effect)) +
    geom_errorbar(aes(ymin  = pbt.low, ymax  = pbt.high),
                  width = .15, size  = 1.5) +
    xlab('Experiment') +
    ylab('Prevalence') +
    geom_hline(yintercept = 0) +
    theme_classic() +
    scale_color_manual(breaks = c(TRUE,FALSE),
                       values=c(graphics_conf$significant_color,
                                graphics_conf$ns_color))+
    
    theme(legend.position = 'none',
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank()) +
  ylim(0, plyr::round_any(max(data$pbt.high), accuracy = 0.1, f = ceiling))
  
  return (plt)
}


# configure the graphics of the figure
graphics_conf <- list(size_seg = 2, color_spreading_lines = '#71E9CC',
                      margin_y_subj = 0.5, margin_y_conds = 0.125, legnth_med = 2,
                      ns_color = 'black', significant_color = 'red', med_color = 'gray',
                      vline_size = 1, x_title_size = 22, x_text_size = 16)
# read the results of the UC database 
res_summary_fn <- 'UCDB_Results.csv'
results <- read.csv(paste('results', res_summary_fn, sep=.Platform$file.sep))

# get only n.s results in the directional test for effects for which we could use all tests
results_RT <- results %>%
  filter(quid_bf != invalid_quid_res, directional_effect.p > alpha)
# generate the PBT sub-plot
pbt_res <- results_RT %>% dplyr::select(exp, pbt.high,pbt.low)
plt_pbt <- generate_pbt_plot(pbt_res,graphics_conf)
plt_pbt

# generate the QUID sub-plot
quid_res <- results_RT %>% 
  dplyr::select(exp, quid_bf) %>%
  mutate(quid_bf = 1/quid_bf)
plt_quid <- generate_quid_plot(quid_res,graphics_conf)
plt_quid

# generate the NDT sub-plot
graphics_conf$y_title <- 'Sign-Consistency - log(p)'
nondir_res <- results %>% 
  dplyr::select(exp, non_directional.p) %>% 
  rename(p = non_directional.p)
plt_nondir <- generate_NDT_plot(nondir_res,graphics_conf)
plt_nondir

# generate the directional test sub-plot
graphics_conf$y_title <- 'Directional - log(p)'
nondir_res <- results %>% select(exp, directional_effect.p) %>% rename(p = directional_effect.p)
plt_dir <- generate_NDT_plot(nondir_res,graphics_conf)
plt_dir <- plt_dir + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank())

# aggregate together the Bayesian tests resutlts (PBT & QUID)
plt_bayes <- grid.arrange(plt_pbt, plt_quid, ncol = 1)
ggsave('figures\\bayes_methods_res.svg', width=15, height=12,plot = plt_bayes)
# aggregate together the NHST tests resutlts (Sign-Consistency & Directional permutations)
plt_nhst <- grid.arrange(plt_dir, plt_nondir, ncol = 1, heights = c(0.5, 1))
ggsave('figures\\plt_nhst_methods_res.svg', width=15, height=12,plot = plt_nhst)
