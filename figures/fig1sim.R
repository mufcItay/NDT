library(weaknull)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(gtable)
library(grid)
rm(list = ls())

## from the weak null packages (not included)
#' Create Sample Data
#' @description The function generated mock data for tests and examples according to the arguments
#' @param p_mean the effect's population mean
#' @param p_sd the standard deviation of the population's effect
#' @param seed - a seed to use when generating the resulting data frame
#' @param N - the number of simulated participants
#' @param trials_per_cnd - the number of simulated trials per condition
#' @param wSEsd - the standard deviation of the dependent measure (within subject error term)
#'
#' @return a data frame with three columns: id (participant id), 'condition' (condition label), and 'var' (the dependent variable)
create_sample_data <- function(p_mean, p_sd, seed = 1, N = 30, trials_per_cnd = 100, wSEsd = 2) {
  set.seed(seed)
  # 0 = faster/smaller condition (e.g., 'congruent'), 1 = slower/larger condition (e.g., 'incongruent'),
  conditionLabels <- c(0,1)
  # define the number of trials across all conditions
  trialsN <- trials_per_cnd * length(conditionLabels)
  
  # define the baseline dependent measure statistical features
  effect_baseline <- 0
  within_subj_effect_sd <- wSEsd
  
  # define the effect statistical features
  population_sd = p_sd
  population_mean = p_mean
  
  # create an id column for the samples data
  idv <- rep(1:N, each = trialsN)
  # create a independent variable column
  iv <- rep(rep(conditionLabels, each = trials_per_cnd), N)
  
  # sample effects for each subject
  subj_true_effect <- stats::rnorm(N,population_mean,population_sd)
  # sample effects for each subject and trial
  subj_true_effect_per_trial <- rep(subj_true_effect, each = trialsN)
  # set the dependent variable columns according to baseine, the true effect, and the indepdent variable
  dv <- stats::rnorm(length(idv), effect_baseline, within_subj_effect_sd) + iv * subj_true_effect_per_trial
  # create a dataframe based on the three columns generated above
  sampled_data <- data.frame(id = idv, condition = iv, var = dv)
  return (sampled_data)
}


#' prepare-data
#' A helper function that prepares data to be presented in the figure 
#' @param data a dataframe of the shape: (#Trials X #Participants) X (id, condition,var)
#' where 'id' is the identifier of participants, 'condition' is the condition identifier (we
#' simulate two different conditions within each participant), 
#' and 'var' is the dependent variable measured on each trial.   
#' @return a list with the following items:
##' \itemize{
##'  \item{"effects"}{a dataframe with summary statstics of the effect measure (incongrueny - congruent),
##'   for each participant, including the directional effect ('effect' column'), 
##'   and sign-consistency ('sc' column)}
##'  \item{"data_ps_incong"}{a dataframe with the percentiles of the dependent measure
##'  within each participants, for the incongruent condition}
##'  \item{"data_ps_cong"}{a dataframe with the percentiles of the dependent measure
##'  within each participants, for the congruent condition}
##' }
##'
prepare_data <- function(data, ci_percentile = 5) {
  # transform the dependent variable to RT
  data$var <- data$var * 20  + 650
  # get directional effect
  res_dir <- data %>% get_directional_effect(idv = 'id', dv = 'var', iv = 'condition', summary_function = stats::median)
  ord_effects <- order(res_dir$effect_per_id$score)
  res_dir$effect_per_id$orgid <- res_dir$effect_per_id$id[ord_effects]
  res_dir$effect_per_id$score <- sort(res_dir$effect_per_id$score)
  
  # get sign-consistency scores
  res_non_dir <- data %>% get_sign_consistency(idv = 'id', dv = 'var', iv = 'condition', 
                                               summary_function = stats::median)
  sc <- res_non_dir$consistency_per_id$score[res_dir$effect_per_id$orgid]
  # add sign_consistency to the directional analysis results dataframe
  res_dir$effect_per_id$sc <- sc
  data_effects <-  res_dir$effect_per_id %>% 
    rename(effect = score)
  
  # get the percentiles for each participant and condition
  lowbnd <- ci_percentile / 100
  lowbnd <- lowbnd / 2
  highbnd <- 1- lowbnd
  percentiles <- c(lowbnd, 0.5, highbnd)
  percentile_cols <- c('low', 'med', 'high') 
  data_ps <- data %>%
    group_by(id,condition) %>%
    summarise(q = percentile_cols,
              RT = quantile(var, percentiles)) %>%
    spread(q,RT)
  # split to conditions and add subject id column
  data_ps_cong <- data_ps %>% filter(condition == 0)
  data_ps_incong <- data_ps %>% filter(condition == 1)
  data_ps_cong <- data_ps_cong[res_dir$effect_per_id$orgid, ]
  data_ps_cong$id <- 1:nrow(res_dir$effect_per_id)
  data_ps_incong <- data_ps_incong[res_dir$effect_per_id$orgid, ]
  data_ps_incong$id <- 1:nrow(res_dir$effect_per_id)
  
  return(list(effects = data_effects, ps_incong = data_ps_incong, ps_cong = data_ps_cong))
}

#' generate_effects_plot
#' A helper function that generates the directional effects per participant plot
#' @param data - a dataframe of the shape (#Participants) X (id, effect, sc),
#' where 'sc' is the sign-consistency score of each participant, and 'effect' is the directional
#' effect of each participant
#' @param graphics_conf a list with different graphics configurations to be used by
#' the plotting helper functions
#' @return the directional effects per participant plot
generate_effects_plot <- function(data, graphics_conf) {
  plt <- ggplot(data, aes(x = effect, y = id)) +
    xlab('Effect') +
    ylab('Subject') +
    xlim(-50,50) +
    geom_point(size = 3) +
    geom_hline(yintercept = data$id + graphics_conf$margin_y_subj, 
               size = graphics_conf$size_seg/2, linetype='dotted', 
               col = graphics_conf$color_spreading_lines) +
    geom_vline(xintercept = 0,  size = graphics_conf$vline_size) +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    ylim(1, nrow(data)) +
    scale_y_continuous(breaks = c(1,seq(5, nrow(data), by = 5)))
  
  return (plt)
}

#' generate_ps_plot
#' A helper function that within participants ranges plot
#' @param data_ps_cong a dataframe with the low, median and high percetiles of the 
#' dependent measure of each participant for the congruent condition.
#' The shape of the dataframe is (#Participants) X (id, low, med, high), where id indicates
#' the identifier of the participant, and 'low', 'med', and 'high' indicates the low, 50%
#' and high percentiles of the depdent measure 
#' @param data_ps_incong same as the 'data_ps_cong' dataframe, for the incongruent condition
#' @param graphics_conf a list with different graphics configurations to be used by
#' the plotting helper functions
#'
#' @return the within participants ranges plot
generate_ps_plot <- function(data_ps_cong, data_ps_incong, graphics_conf) {
  plt <- ggplot(data_ps_cong, aes(y = id)) +
    geom_segment(data = data_ps_incong, aes(x = low, xend = high, y = id + graphics_conf$margin_y_conds, yend = id + graphics_conf$margin_y_conds),
                 size = graphics_conf$size_seg, colour = graphics_conf$incong_color) +
    geom_segment(data = data_ps_cong, aes(x = low, xend = high, y = id - graphics_conf$margin_y_conds, 
                                          yend = id - graphics_conf$margin_y_conds), 
                 size = graphics_conf$size_seg, colour = graphics_conf$cong_color) +
    geom_segment(data = data_ps_incong, aes(x = med - graphics_conf$legnth_med, xend = med + graphics_conf$legnth_med,  y = id + graphics_conf$margin_y_conds, 
                                            yend = id + graphics_conf$margin_y_conds), 
                 size = graphics_conf$size_seg, colour = graphics_conf$med_color) +
    geom_segment(data = data_ps_cong, aes(x = med - graphics_conf$legnth_med, xend = med + graphics_conf$legnth_med, y = id - graphics_conf$margin_y_conds, 
                                          yend = id - graphics_conf$margin_y_conds), 
                 size = graphics_conf$size_seg, colour = graphics_conf$med_color) +
    geom_hline(yintercept = data_ps_cong$id + graphics_conf$margin_y_subj, 
               size = graphics_conf$size_seg/2, linetype='dotted', 
               col = graphics_conf$color_spreading_lines) +
    geom_vline(xintercept = mean(data_ps_cong$med),  size = graphics_conf$vline_size) +
    xlab('RT') +
    ylab('Subject') +
    xlim(300, 1000) +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank()) +
    ylim(1, nrow(data_ps_cong)) +
    scale_y_continuous(breaks = c(1,seq(5, nrow(data_ps_cong), by = 5)))
  return (plt)
}

#' generate_agg_plot
#' Generates the aggregated figure of the directional effects and within participant percentiles
#' @param data a dataframe of the shape: (#Trials X #Participants) X (id, condition,var)
#' where 'id' is the identifier of participants, 'condition' is the condition identifier (we
#' simulate two different conditions within each participant), 
#' and 'var' is the dependent variable measured on each trial.   
#' @param graphics_conf a list with different graphics configurations to be used by
#' @param rng_ratio if has a value different from -1 (the default value), sets the width
#' of the right-plot in the aggregated figure according to the assigned value (where the
#' left side of the aggregated plot has width of 1)
#'
#' @return returns a figure of the directional effects and within participant percentiles
generate_agg_plot <- function(data, graphics_conf, fn, rng_ratio = -1) {
  res <- prepare_data(data)
  data_effects <- res$effects
  data_ps_incong <- res$ps_incong
  data_ps_cong <- res$ps_cong
  p_left <- generate_effects_plot(data_effects, graphics_conf)  
  p_right <- generate_ps_plot(data_ps_cong, data_ps_incong, graphics_conf)  
  
  
  if(rng_ratio == -1) {
    rng_left <- max(data_effects$effect) - min(data_effects$effect) 
    rng_right <- max(data_ps_incong$high) - min(data_ps_incong$low) 
    rng_ratio <- rng_right / rng_left
  }
  plt <- grid.arrange(p_left ,p_right, widths = c(1, rng_ratio), ncol = 2,
               left = textGrob("Subject", rot = 90, vjust = 1, gp = gpar(fontsize = graphics_conf$x_title_size)))
  ggsave(paste('figures',paste0(fn, '.svg'), sep = .Platform$file.sep), 
         width=10, height=5,plot = plt)
  
  return(rng_ratio)
}

# configure the graphics of the plot
graphics_conf <- list(size_seg = 2, color_spreading_lines = '#71E9CC',
                      margin_y_subj = 0.5, margin_y_conds = 0.125, legnth_med = 2,
                      incong_color = 'black', cong_color = 'red', med_color = 'gray',
                      vline_size = 1, x_title_size = 22, x_text_size = 20)
# generate weaknull plot
wn_data <- create_sample_data(p_mean = 0, p_sd = 1, N = 15, trials_per_cnd = 100, wSEsd = 2)
ratio <- generate_agg_plot(wn_data, graphics_conf, 'wn_plt')
sn_data <- create_sample_data(p_mean = 0, p_sd = 0, N = 15, trials_per_cnd = 100, wSEsd = 6)
generate_agg_plot(sn_data, graphics_conf, 'sn_plt', ratio)

# common analysis, two-sided t-test
wn_t_test <- wn_data %>%
  group_by(id, condition) %>%
  summarise(mrt = mean(var)) %>%
  group_by(id) %>%
  summarise(effect = diff(mrt)) %>%
  pull(effect) %>%
  t.test()
sn_t_test <- sn_data %>%
  group_by(id, condition) %>%
  summarise(mrt = mean(var)) %>%
  group_by(id) %>%
  summarise(effect = diff(mrt)) %>%
  pull(effect) %>%
  t.test()

# Bayesian analysis
# QUID
source('datasets_analysis\\quid.R')
renamed_wn_data <- wn_data %>%
  rename(idv = id, iv = condition, dv = var)
renamed_sn_data <- sn_data %>%
  rename(idv = id, iv = condition, dv = var)
wn_quid_res <- run_quid(renamed_wn_data)
sn_quid_res <- run_quid(renamed_sn_data)
wn_bf <- 1/ wn_quid_res$quid_bf
sn_bf <- 1/ sn_quid_res$quid_bf

#PBT
source('datasets_analysis\\pbt.R')
t_f <- function(data) {
  conditions <- unique(data$iv)
  t_res <- t.test(data[data$iv == conditions[2],]$dv,
                  data[data$iv == conditions[1],]$dv)
  return(t_res$p.value)
}
wn_pbt_res <- run_pbt(renamed_wn_data, test_function = t_f)
sn_pbt_res <- run_pbt(renamed_sn_data, test_function = t_f)
