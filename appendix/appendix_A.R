library(signcon)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(tidyr)
library(extraDistr)

# source scripts from subfolders
figures_fld <- 'figures'
apdx_fld <- 'appendix'
source(paste(figures_fld, 'plotting_utils.R', sep = .Platform$file.sep))
source(paste(apdx_fld, 'generate_dataset.R', sep = .Platform$file.sep))

#' prepare-data
#' A helper function that prepares data to be presented in the figure 
#' @param data a dataframe of the shape: (#Trials X #Participants) X (idv, iv,dv)
#' where 'idv' is the identifier of participants, 'iv' is the condition identifier (we
#' simulate two different conditions within each participant), 
#' and 'dv' is the dependent variable measured on each trial.   
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
  # get directional effect
  res_dir <- data %>% get_directional_effect(idv = 'idv', dv = 'dv', iv = 'iv',
                                             summary_function = mean)
  ord_effects <- order(res_dir$effect_per_id$score)
  res_dir$effect_per_id$orgid <- res_dir$effect_per_id$idv[ord_effects]
  res_dir$effect_per_id$score <- sort(res_dir$effect_per_id$score)
  
  # get sign-consistency scores
  res_non_dir <- data %>% get_sign_consistency(idv = 'idv', dv = 'dv', iv = 'iv', 
                                               summary_function = mean)
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
  data_ps <-   data %>%
    group_by(idv,iv) %>%
    reframe(q = percentile_cols,
              RT = mean(dv) + sd(dv)/sqrt(n()) * qnorm(p = c(.025,0.5,.975), 0,1))%>%
    spread(q,RT)
  # split to conditions and add subject idv column
  data_ps_cong <- data_ps %>% filter(iv == 0)
  data_ps_incong <- data_ps %>% filter(iv == 1)
  data_ps_cong <- data_ps_cong[res_dir$effect_per_id$orgid, ]
  data_ps_cong$idv <- 1:nrow(res_dir$effect_per_id)
  data_ps_incong <- data_ps_incong[res_dir$effect_per_id$orgid, ]
  data_ps_incong$idv <- 1:nrow(res_dir$effect_per_id)
  
  return(list(effects = data_effects, ps_incong = data_ps_incong, ps_cong = data_ps_cong))
}

#' generate_effects_plot
#' A helper function that generates the directional effects per participant plot
#' @param data - a dataframe of the shape (#Participants) X (idv, effect, sc),
#' where 'sc' is the sign-consistency score of each participant, and 'effect' is the directional
#' effect of each participant
#' @param graphics_conf a list with different graphics configurations to be used by
#' the plotting helper functions
#' @return the directional effects per participant plot
generate_effects_plot <- function(data, graphics_conf) {
  plt <- ggplot(data, aes(x = effect, y = idv)) +
    xlab('Effect') +
    ylab('Subject') +
    xlim(-50,50) +
    geom_point(size = 3) +
    geom_hline(yintercept = head(data$idv,-1) + graphics_conf$margin_y_subj, 
               linewidth = graphics_conf$size_seg/2, linetype='dotted', 
               col = graphics_conf$color_spreading_lines) +
    geom_vline(xintercept = 0,  linewidth = graphics_conf$vline_size) +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = graphics_conf$x_title_size,
                                      margin = margin(t = 15))
          ) +
    scale_y_continuous(limits = c(1 - graphics_conf$margin_y_subj, nrow(data) + graphics_conf$margin_y_subj), breaks = c(1,seq(5, nrow(data), by = 5)))
  return (plt)
}

#' generate_ps_plot
#' A helper function that within participants ranges plot
#' @param data_ps_cong a dataframe with the low, median and high percetiles of the 
#' dependent measure of each participant for the congruent condition.
#' The shape of the dataframe is (#Participants) X (idv, low, med, high), where idv indicates
#' the identifier of the participant, and 'low', 'med', and 'high' indicates the low, 50%
#' and high percentiles of the depdent measure 
#' @param data_ps_incong same as the 'data_ps_cong' dataframe, for the incongruent condition
#' @param graphics_conf a list with different graphics configurations to be used by
#' the plotting helper functions
#'
#' @return the within participants ranges plot
generate_ps_plot <- function(data_ps_cong, data_ps_incong, graphics_conf) {
  plt <- ggplot(data_ps_cong, aes(y = idv)) +
    geom_segment(data = data_ps_incong, aes(x = low, xend = high, y = idv + graphics_conf$margin_y_conds, yend = idv + graphics_conf$margin_y_conds),
                 linewidth = graphics_conf$size_seg, colour = graphics_conf$incong_color) +
    geom_segment(data = data_ps_cong, aes(x = low, xend = high, y = idv - graphics_conf$margin_y_conds, 
                                          yend = idv - graphics_conf$margin_y_conds), 
                 linewidth = graphics_conf$size_seg, colour = graphics_conf$cong_color) +
    geom_segment(data = data_ps_incong, aes(x = med - graphics_conf$legnth_med, xend = med + graphics_conf$legnth_med,  y = idv + graphics_conf$margin_y_conds, 
                                            yend = idv + graphics_conf$margin_y_conds), 
                 linewidth = graphics_conf$size_seg, colour = graphics_conf$med_color) +
    geom_segment(data = data_ps_cong, aes(x = med - graphics_conf$legnth_med, xend = med + graphics_conf$legnth_med, y = idv - graphics_conf$margin_y_conds, 
                                          yend = idv - graphics_conf$margin_y_conds), 
                 linewidth = graphics_conf$size_seg, colour = graphics_conf$med_color) +
    geom_hline(yintercept = head(data_ps_cong$idv,-1) + graphics_conf$margin_y_subj, 
               linewidth = graphics_conf$size_seg/2, linetype='dotted', 
               col = graphics_conf$color_spreading_lines) +
    geom_vline(xintercept = mean(data_ps_cong$med),  linewidth = graphics_conf$vline_size) +
    xlab('RT') +
    ylab('Subject') +
    xlim(600, 700) +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.title.x = element_text(margin = margin(t = 15))) +
    scale_y_continuous(limits = c(1 - graphics_conf$margin_y_conds, nrow(data_ps_cong) + graphics_conf$margin_y_conds), breaks = c(1,seq(5, nrow(data_ps_cong), by = 5)))
  return (plt)
}

#' generate_agg_plot
#' Generates the aggregated figure of the directional effects and within participant percentiles
#' @param data a dataframe of the shape: (#Trials X #Participants) X (idv, iv,dv)
#' where 'idv' is the identifier of participants, 'iv' is the condition identifier (we
#' simulate two different conditions within each participant), 
#' and 'dv' is the dependent variable measured on each trial.   
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
               left = textGrob("Subject", rot = 90, vjust = .5, gp = gpar(fontsize = graphics_conf$x_title_size)))
  ggsave(paste(plots_fld,paste0(fn, '.png'), sep = .Platform$file.sep), 
         width=10, height=5,plot = plt)
  
  return(rng_ratio)
}

# configure the graphics of the plot
graphics_conf <- list(size_seg = 2, color_spreading_lines = '#B1C2CF',
                      margin_y_subj = 0.5, margin_y_conds = 0.125, legnth_med = 2,
                      incong_color = '#e41a1c', cong_color = '#377eb8', med_color = 'gray',
                      vline_size = 1, x_title_size = 22, x_text_size = 20)
# for illustration, add an RT offset
offset_rt <- 650
wald_tau <- 200 # we shift the wald distribution according to an initial RT offset
wald_mu <- offset_rt - wald_tau 
sim_seed <- 911
# generate nde plot
nde_sd_b <- 15
nde_sd_w <- 30
nde_n <- 15
nde_trials <- 100
wald_nde_data <- generate_dataset(p_mean = 0, p_sd = nde_sd_b, N = nde_n,
                             trials_per_cnd = nde_trials, 
                             wSEsd = nde_sd_w, dist_type = 'Wald',
                             dv_offset = offset_rt, seed = sim_seed)
ratio <- generate_agg_plot(wald_nde_data, graphics_conf, 'wald_nde_plt')
sn_sd_b <- 0
sn_sd_w <- 100
sn_n <- 15
sn_trials <- 100
wald_sn_data <- generate_dataset(p_mean = 0, p_sd = 0, N = 15, 
                            trials_per_cnd = 100, 
                            wSEsd = 100, dist_type = 'Wald',
                            dv_offset = offset_rt, seed = sim_seed)
generate_agg_plot(wald_sn_data, graphics_conf, 'wald_sn_plt', ratio)

# common analysis, two-sided t-test
wald_nde_t_test <- wald_nde_data %>%
  group_by(idv, iv) %>%
  summarise(mrt = mean(dv)) %>%
  group_by(idv) %>%
  summarise(effect = diff(mrt)) %>%
  dplyr::pull(effect) %>%
  t.test()
wald_sn_t_test <- wald_sn_data %>%
  group_by(idv, iv) %>%
  summarise(mrt = mean(dv)) %>%
  group_by(idv) %>%
  summarise(effect = diff(mrt)) %>%
  dplyr::pull(effect) %>%
  t.test()

# Bayesian analysis
bf_criteria <- 3
# QUID
source(paste('datasets_analysis', 'quid.R', sep = .Platform$file.sep))
wald_nde_quid_res <- run_quid(wald_nde_data)
wald_sn_quid_res <- run_quid(wald_sn_data)
wald_nde_bf <- 1/ wald_nde_quid_res$quid_bf
wald_sn_bf <- 1/ wald_sn_quid_res$quid_bf

#GNT
source(paste('datasets_analysis', 'gnt.R', sep = .Platform$file.sep))
t_f <- function(data) {
  conditions <- unique(data$iv)
  t_res <- t.test(data[data$iv == conditions[2],]$dv,
                  data[data$iv == conditions[1],]$dv)
  return(t_res$p.value)
}
wald_nde_gnt_res <- run_gnt(wald_nde_data, test_function = t_f)
wald_sn_gnt_res <- run_gnt(wald_sn_data, test_function = t_f)

# OANOVA Test
oanova_alpha <- .05
source(paste('datasets_analysis', 'oanova_test.R', sep = .Platform$file.sep))
wald_nde_OANOVA_res <- run_oanova_test(wald_nde_data)
wald_sn_OANOVA_res <- run_oanova_test(wald_sn_data)

# signcon test
wald_nde_sc_res <- test_sign_consistency(wald_nde_data, idv = 'idv', iv = 'iv', dv = 'dv')
wald_sn_sc_res <- test_sign_consistency(wald_sn_data, idv = 'idv', iv = 'iv', dv = 'dv')
wald_sc_nde_p <- wald_nde_sc_res$p
wald_sc_nde_stat <- wald_nde_sc_res$statistic
wald_sc_sn_p <- wald_sn_sc_res$p
wald_sc_sn_stat <- wald_sn_sc_res$statistic

# absolute effect size test
wald_nde_abs_es_res <- test_absolute_es(wald_nde_data, idv = 'idv', iv = 'iv', dv = 'dv')
wald_sn_abs_es_res <- test_absolute_es(wald_sn_data, idv = 'idv', iv = 'iv', dv = 'dv')
wald_abs_es_nde_p <- wald_nde_abs_es_res$p
wald_abs_es_nde_stat <- wald_nde_abs_es_res$statistic
wald_abs_es_sn_p <- wald_sn_abs_es_res$p
wald_abs_es_sn_stat <- wald_sn_abs_es_res$statistic


## plot Kruschke style plots for the figure
nde_b <- dists$normal
nde_w <- dists$wald
nde_mu <- nde_w$ddist_params$mu
nde_w$ddist_params$lambda = get_wald_lambda(nde_w$ddist_params$mu, nde_sd_w) 

save_png <- function(fn, dist, labels) {
  fn <- paste(plots_fld, paste0('wald_', '_', fn, '.png'), sep = .Platform$file.sep)
  png(fn, width=165, height=123, bg="transparent", res=72, )
  plot_dist(dist, labels = labels, plot_dist_name = F)
  dev.off()
}

# upper panel - nde (non-directional effect)
save_png('wald_nde_b', nde_b, c(mean = expression(N (0, sigma[b]))))
save_png('wald_nde_w', nde_w, c(mean = expression(W (mu, lambda))))

# left panel 
sn_b <- dists$normal
# set sd to a very low value (ideally zero would be used here)
sn_b$ddist_params$sd = 0.0000001 
sn_b$plot_type <- 'line'
sn_w <- dists$wald
sn_mu <- sn_w$ddist_params$mu
sn_w$ddist_params$lambda = get_wald_lambda(sn_w$ddist_params$mu, sn_sd_w) 
# lower panel - sn (strong/global null)
save_png('wald_sn_b', sn_b, c(mean = expression(delta (0))))
save_png('wald_sn_w', sn_w, c(mean = expression(W (mu, lambda))))