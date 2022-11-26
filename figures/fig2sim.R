library(weaknull)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)

generate_NDT_plot <- function(data, graphics_conf, eps = 1/10^5) {
  data <- data %>%
    mutate(effect = p < .05, p = log10(p + eps))
  plt <- ggplot(data, aes(x = exp, y = p, color = effect)) +
    xlab('Experiment') +
    ylab(graphics_conf$y_title) +
    geom_point(size = 5) +
    ylim(min(data$p) - .1, max(data$p) + .1) +
    geom_hline(yintercept = log10(0.05), linetype='dotted') +
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


generate_quid_plot <- function(data, graphics_conf, criteria = 3, eps = 1/10^5) {
  data <- data %>%
    mutate(effect = quid_bf < 1/criteria, log_bf = log10(quid_bf + eps))
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

generate_agg_plot <- function(data, graphics_conf, rng_ratio = -1) {
  res <- prepate_data(data)
  data_effects <- res$effects
  data_qs_incong <- res$qs_incong
  data_qs_cong <- res$qs_cong
  p_left <- generate_effects_plot(data_effects, graphics_conf)  
  p_right <- generate_qs_plot(data_qs_cong, data_qs_incong, graphics_conf)  
  
  
  if(rng_ratio == -1) {
    rng_left <- max(data_effects$effect) - min(data_effects$effect) 
    rng_right <- max(data_qs_incong$high) - min(data_qs_incong$low) 
    rng_ratio <- rng_right / rng_left
  }
  grid.arrange(p_left ,p_right, widths = c(1, rng_ratio), ncol = 2,
               left = textGrob("Subject", rot = 90, gp = gpar(fontsize = graphics_conf$x_title_size)))
  
  return(rng_ratio)
}

# configure the graphics of the plot
graphics_conf <- list(size_seg = 2, color_spreading_lines = '#71E9CC',
                      margin_y_subj = 0.5, margin_y_conds = 0.125, legnth_med = 2,
                      ns_color = 'black', significant_color = 'red', med_color = 'gray',
                      vline_size = 1, x_title_size = 22, x_text_size = 16)
# generate agg plot
res_summary_fn <- 'UCDB_Results.csv'
invalid_quid_res <- -99999
results <- read.csv(paste('results', res_summary_fn, sep=.Platform$file.sep))

results_RT <- results %>%
  filter(pos_bf != invalid_quid_res, directional_effect.p > .05) %>%
  rename(quid_bf = pos_bf)
pbt_res <- results_RT %>% select(exp, pbt.high,pbt.low)
plt_pbt <- generate_pbt_plot(pbt_res,graphics_conf)
plt_pbt

quid_res <- results_RT %>% select(exp, quid_bf)
plt_quid <- generate_quid_plot(quid_res,graphics_conf)
plt_quid

graphics_conf$y_title <- 'Sign-Consistency - log(p)'
nondir_res <- results %>% select(exp, non_directional.p) %>% rename(p = non_directional.p)
plt_nondir <- generate_NDT_plot(nondir_res,graphics_conf)
plt_nondir

graphics_conf$y_title <- 'Directional - log(p)'
nondir_res <- results %>% select(exp, directional_effect.p) %>% rename(p = directional_effect.p)
plt_dir <- generate_NDT_plot(nondir_res,graphics_conf)
plt_dir <- plt_dir + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank())


plt_bayes <- grid.arrange(plt_pbt, plt_quid, ncol = 1)
ggsave('figures\\bayes_methods_res.svg', width=15, height=12,plot = plt_bayes)
plt_nhst <- grid.arrange(plt_dir, plt_nondir, ncol = 1, heights = c(0.5, 1))
ggsave('figures\\plt_nhst_methods_res.svg', width=15, height=12,plot = plt_nhst)
