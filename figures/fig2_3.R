library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)
library(ggtext)
library(ggridges)
library(ggh4x)


figures_fld <- 'figures'
source(paste(figures_fld, 'plotting_utils.R', sep = .Platform$file.sep))

# set alpha value
alpha <- .05
invalid_res <- INVALID_VALUE_CODE


#' add_simulation_results
#' the function adds the simulation results to the empirical data results
#' @param emp_data the dataset with the results for all relevant datasets
#' @return a dataframe with all of the results of the empiric datasets + two
#' rows with the simulation results for the global null and qualitative differences
add_simulation_results <- function(emp_data) {
  emp_data$is_sim <- FALSE
  source(paste(figures_fld, 'fig1sim.R', sep = .Platform$file.sep))
  n_null_samples <- 10^5
  sign_con_nde <- test_sign_consistency(nde_data, idv = 'idv', iv = 'iv', dv = 'dv', null_dist_samples = n_null_samples)
  sign_con_sn <- test_sign_consistency(sn_data, idv = 'idv', iv = 'iv', dv = 'dv', null_dist_samples = n_null_samples)
  abs_es_nde <- test_absolute_es(nde_data, idv = 'idv', iv = 'iv', dv = 'dv', null_dist_samples = n_null_samples)
  abs_es_sn <- test_absolute_es(sn_data, idv = 'idv', iv = 'iv', dv = 'dv', null_dist_samples = n_null_samples)
  
  gn_df <- data.frame(exp = rep('GN', n_null_samples),
                      signcon.null_dist = sign_con_sn$null_dist,
                      absolute_es.null_dist = abs_es_sn$null_dist)
  gn_df$quid_bf <- 1/sn_bf
  gn_df$gnt.p <- sn_gnt_res$p
  gn_df$gnt.stat <- sn_gnt_res$stat
  gn_df$gnt.ci_low <- sn_gnt_res$ci_low
  gn_df$gnt.ci_high <- sn_gnt_res$ci_high
  gn_df$oanova.p <- sn_OANOVA_res$p
  gn_df$signcon.p <- sign_con_sn$p
  gn_df$signcon.statistic <- sign_con_sn$statistic
  gn_df$absolute_es.p <- abs_es_sn$p
  gn_df$absolute_es.statistic <- abs_es_sn$statistic
  nd_df <- data.frame(exp = rep('ND', n_null_samples),
                      signcon.null_dist = sign_con_nde$null_dist,
                      absolute_es.null_dist = abs_es_nde$null_dist)
  nd_df$quid_bf <- 1/nde_bf
  nd_df$gnt.p <- nde_gnt_res$p
  nd_df$gnt.stat <- nde_gnt_res$stat
  nd_df$gnt.ci_low <- nde_gnt_res$ci_low
  nd_df$gnt.ci_high <- nde_gnt_res$ci_high
  nd_df$oanova.p <- nde_OANOVA_res$p
  nd_df$signcon.p <- sign_con_nde$p
  nd_df$signcon.statistic <- sign_con_nde$statistic
  nd_df$absolute_es.p <- abs_es_nde$p
  nd_df$absolute_es.statistic <- abs_es_nde$statistic
  sim_data <- rbind(gn_df, nd_df)                      
  sim_data$is_sim <- TRUE
  sim_data$directional_test.p <- invalid_res
  sim_data$directional_test.statistic <- 0
  sim_data$directional_test.ci_low <- 0
  sim_data$directional_test.ci_high <- 0
  return(rbind(emp_data[,names(sim_data)],sim_data))
}

#' generate_signcon_plot
#' The function generates the sign-consistency (non-directional test) results plot
#' @param data a dataframe with the results of the sign-consistency solution. The shape of 
#' the dataframe is (#Datasets) X (exp, p, stat, null_dist), 
#' where 'exp' is the name of the effect, 'p' is the p-value according to the 
#' sign-consistency solution, 'stat' is the group-level sign-consistency, and
#' 'null_dist' is the samples from the null distribution of sign-consistency.
#' @param graphics_conf a list with different graphics configurations to be used by
#' @param alpha an alpha value that determines which effects are significant
#' @return the plot describing the results of the (non-directional) sign-consistency test
generate_signcon_plot <- function(data, graphics_conf, alpha = .05) {
  n_sim <- length(unique(data[data$is_sim,]$exp))
  data_sim_rect <- data.frame(ymin = 1, ymax = n_sim + .5,
                              xmin = 0, xmax = 100)
  data <- data %>%
    mutate(effect = p <= alpha, exp = factor(exp), stat = stat * 100, null = null * 100)
  effect_per_exp <- data %>%
    group_by(exp) %>%
    summarise(effect = first(effect)) %>%
    dplyr::pull(effect)
  exp_label_colors <- sapply(effect_per_exp, function(e) ifelse(e, graphics_conf$significant_color, 'black'))
  exp_label_face <- sapply(effect_per_exp, function(e) ifelse(e, "bold","plain"))
  highly_sig_markers_df <- data %>% 
    filter(p < 10^-2) %>% 
    group_by(exp) %>% 
    summarise(stat = first(stat)) %>% 
    mutate(x = stat, xend = stat, y = as.integer(exp), yend = as.integer(exp) + .5)
  scs <- data %>% 
    group_by(exp) %>% 
    summarise(sc = unique(stat), null_dist_id = sum(null), effect = unique(effect)) %>%
    mutate(exp = as.integer(exp))
  qf <- function(x,probs) {
    scs %>% filter(null_dist_id == sum(x)) %>% dplyr::pull(sc) %>% first()
  }
  plt <- ggplot(data, aes(x = null, y = exp, fill = effect)) +
    geom_rect(data = data_sim_rect, 
              aes(NULL, NULL , xmin = xmin, xmax = xmax, ymin = ymin, 
                  ymax = ymax,shape = NULL),
              fill = graphics_conf$sim_rect_color, alpha = .5) +
    stat_density_ridges(geom = "density_ridges_gradient",alpha = 1, size=1, 
                        vline_size = graphics_conf$vline_size, 
                        vline_color = c(graphics_conf$vline_color), 
                        quantile_fun = qf, from = 0, to = 100,quantiles = 2, 
                        quantile_lines = TRUE, calc_ecdf = TRUE, fill= graphics_conf$dist_below_color) +
    geom_segment(data = highly_sig_markers_df, aes(fill = NULL, x = x, xend = xend, y = y, yend = yend), 
                 linewidth = graphics_conf$vline_size, color = graphics_conf$vline_color) +
    scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10))+
    coord_flip() +
    ylab('Experiment') +
    xlab('Sign-Consistency (%)') +
    theme_classic() +
    theme(legend.position = 'none',
          plot.title = element_text(size = graphics_conf$title_size, hjust = 0,
                                    margin=margin(0,0,30,0)),
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.text.x = element_text(size = graphics_conf$x_text_size, vjust = .1,
                                     colour = exp_label_colors, face = exp_label_face,
                                     angle = 45),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.title.x = element_text(size = graphics_conf$x_title_size, vjust = -3),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
          plot.margin = (unit(c(1, .5, 1, .5), "cm")))
  
  # modify the layers of the plot to add coloring by sign-consistenct
  plt_breakdown <- ggplot_build(plt)
  plt_breakdown$data[[2]] = plt_breakdown$data[[2]] %>%
    left_join(., scs,
              by = c("group" = "exp")) %>%
    mutate(fill = case_when(effect ~ graphics_conf$significant_color,
                            x < sc ~ fill,
                            TRUE ~ graphics_conf$dist_above_color)) 
  
  plt_breakdown <- ggplot_gtable(plt_breakdown)
  
  return (plt_breakdown)
}


#' generate_abs_es_plot
#' The function generates the absolute effect size test (non-directional test) results plot
#' @param data a dataframe with the results of the abs effect size test.
#'  The shape of the dataframe is (#Datasets) X (exp, p, stat, null_dist), 
#' where 'exp' is the name of the effect, 'p' is the p-value according to the 
#' test, 'stat' is the group-level absolute effect size, and
#' 'null_dist' is the samples from the null distribution of the statistic.
#' @param graphics_conf a list with different graphics configurations to be used by
#' @param alpha an alpha value that determines which effects are significant
#' @return the plot describing the results of the (non-directional) absolute effect size test
generate_abs_es_plot <- function(data, graphics_conf, alpha = .05) {
  n_sim <- length(unique(data[data$is_sim,]$exp))
  data_sim_rect <- data.frame(ymin = 1, ymax = n_sim + .5,
                              xmin = 0, xmax = 0.7)
  data <- data %>%
    mutate(effect = p <= alpha, exp = factor(exp), stat = stat, null = null)
  effect_per_exp <- data %>%
    group_by(exp) %>%
    summarise(effect = first(effect)) %>%
    dplyr::pull(effect)
  exp_label_colors <- sapply(effect_per_exp, function(e) ifelse(e, graphics_conf$significant_color, 'black'))
  exp_label_face <- sapply(effect_per_exp, function(e) ifelse(e, "bold","plain"))
  highly_sig_markers_df <- data %>% 
    filter(p < 10^-2) %>% 
    group_by(exp) %>% 
    summarise(stat = first(stat)) %>% 
    mutate(x = stat, xend = stat, y = as.integer(exp), yend = as.integer(exp) + .5)
  absess <- data %>% 
    group_by(exp) %>% 
    summarise(abs_es = unique(stat), null_dist_id = sum(null), effect = unique(effect)) %>%
    mutate(exp = as.integer(exp))
  qf <- function(x,probs) {
    absess %>% filter(null_dist_id == sum(x)) %>% dplyr::pull(abs_es) %>% first()
  }
  plt <- ggplot(data, aes(x = null, y = exp, fill = effect)) +
    geom_rect(data = data_sim_rect, 
              aes(NULL, NULL , xmin = xmin, xmax = xmax, ymin = ymin, 
                  ymax = ymax,shape = NULL),
              fill = graphics_conf$sim_rect_color, alpha = .5) +
    stat_density_ridges(geom = "density_ridges_gradient",alpha = 1, size=1, 
                        vline_size = graphics_conf$vline_size, 
                        vline_color = c(graphics_conf$vline_color), 
                        quantile_fun = qf, n = 4096, from = 0, to = 100,quantiles = 2, 
                        quantile_lines = TRUE, calc_ecdf = TRUE, fill= graphics_conf$dist_below_color) +
    geom_segment(data = highly_sig_markers_df, aes(fill = NULL, x = x, xend = xend, y = y, yend = yend), 
                 linewidth = graphics_conf$vline_size, color = graphics_conf$vline_color) +
    scale_x_continuous(limits=c(0,0.7), breaks=seq(0,0.7,.1))+
    coord_flip() +
    ylab('Experiment') +
    xlab('|Effect Size|') +
    theme_classic() +
    theme(legend.position = 'none',
          plot.title = element_text(size = graphics_conf$title_size, hjust = 0,
                                    margin=margin(0,0,30,0)),
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.text.x = element_text(size = graphics_conf$x_text_size, vjust = .1,
                                     colour = exp_label_colors, face = exp_label_face,
                                     angle = 45),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.title.x = element_text(size = graphics_conf$x_title_size, vjust = -3),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
          plot.margin = (unit(c(1, .5, 1, .5), "cm")))
  
  # modify the layers of the plot to add coloring by sign-consistenct
  plt_breakdown <- ggplot_build(plt)
  plt_breakdown$data[[2]] = plt_breakdown$data[[2]] %>%
    left_join(., absess,
              by = c("group" = "exp")) %>%
    mutate(fill = case_when(effect ~ graphics_conf$significant_color,
                            x < abs_es ~ fill,
                            TRUE ~ graphics_conf$dist_above_color)) 
  
  plt_breakdown <- ggplot_gtable(plt_breakdown)
  
  return (plt_breakdown)
}

#' generate_quid_plot
#' The function generates the QUID results sub-plot
#' @param data a dataframe with the results of the QUID solution. The shape of 
#' the dataframe is (#Datasets) X (exp, quid_bf), where 'exp' is the name of the experiment (and
#' respective dataset), and 'quid_bg' is the Bayes Factor according to the solution. 
#' @param graphics_conf a list with different graphics configurations to be used by
#' @param criteria an Bayesian factors criteria to highlight convincing effects
#' @param eps an epsilon value to be used when transforming p-values to log values
#' @return the plot describing the results of the QUID test
generate_quid_plot <- function(data, graphics_conf, criteria = 3, eps = 10^-2) {
  n_sim <- sum(data$is_sim)
  data <- data %>%
    mutate(effect = ifelse(quid_bf > criteria, 'effect', 
                           ifelse(quid_bf < 1/criteria, 'ns', 'uncertain')),
           log_bf = ifelse(log10(quid_bf) < log10(eps), log10(eps),
                           ifelse(log10(quid_bf) > log10(1/eps), log10(1/eps), 
                                  log10(quid_bf))))
  data_sim_rect <- data.frame(xmin = .75, xmax = n_sim + .25, 
                              ymin = min(data$log_bf)-.1, ymax = max(data$log_bf) + .1)
  exp_label_colors <- sapply(data$effect, function(e) ifelse(e == 'effect', graphics_conf$significant_color, 'black'))
  exp_label_face <- sapply(data$effect, function(e) ifelse(e == 'effect', "bold","plain"))
  # set symmetric bf log transformed ticks
  ticks_bf <- c(1,criteria,10, 10^2, 10^3)
  ticks_bf <- log10(c(1/ticks_bf, ticks_bf))
  plt <- ggplot(data, aes(x = exp, y = log_bf, fill = effect, shape = is_sim)) +
    geom_rect(data = data_sim_rect, 
              aes(NULL, NULL , xmin = xmin, xmax = xmax, ymin = ymin, 
                  ymax = ymax,shape = NULL),
              fill = graphics_conf$sim_rect_color, alpha = .5) +
    xlab('Experiment') +
    ylab('Bayes Factor') +
    geom_hline(yintercept = 0, linetype='dotted', linewidth = 2) +
    geom_hline(yintercept = c(log10(criteria),log10(1/criteria)), 
               linetype='solid', linewidth = 1.5, color = graphics_conf$pale_color) +
    geom_point(size = 5.5, colour = 'black', stroke =2) +
    scale_shape_manual(values = graphics_conf$shapes_per_sim) +
    ggtitle(graphics_conf$title) +
    theme_classic() +
    scale_fill_manual(breaks = c('effect','uncertain','ns'),
                      values=c(graphics_conf$significant_color,
                               graphics_conf$med_color,
                               graphics_conf$null_support_color))+
    theme(legend.position = 'none',
          plot.title = element_text(size = graphics_conf$title_size, hjust = 0,
                                    margin=margin(0,0,30,0)),
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.text.x = element_text(size = graphics_conf$x_text_size, vjust = .1,
                                     colour = exp_label_colors, face = exp_label_face,
                                     angle = 45),
          axis.title.y = element_markdown(margin = margin(r = 25)),
          axis.title.x = element_text(size = graphics_conf$x_title_size, vjust = -3),
          plot.margin = (unit(c(.5, .5, 1, .5), "cm"))) +
    
    annotate('text', label = c('global null ↓', 'qualitative differences ↑'), 
             x = c(graphics_conf$annotate_bfs_x_low, graphics_conf$annotate_bfs_x_high),
             y = c(log10(1/criteria) - graphics_conf$annotate_bfs_y_space_low, 
                   log10(criteria) + graphics_conf$annotate_bfs_y_space_high), 
             fontface = 'italic', size = 7) +
    scale_x_discrete(expand=c(0.02, 0)) +
    scale_y_continuous(label = function(x) round(10^x,digits = 2),
                       limits = c(min(data$log_bf) - .1, max(data$log_bf) + .1), 
                       breaks= ticks_bf)
  res_plt <- add_y_separators(plt, y_seps = log10(c(1/eps - 15, 1/eps - 35)),
                        y_lim = log10(c(eps, 1/eps)), min_x = .75,
                        angle = graphics_conf$seps_angle, length = graphics_conf$seps_length, 
                        linewidth= graphics_conf$seps_lw)
  
  return (res_plt)
}

#' generate_OANOVA_plot
#' The function generates the OANOVA test results sub-plot
#' @param data a dataframe with the results of the OANOVA solution. The shape of 
#' the dataframe is (#Datasets) X (exp, OANOVA.F, oanova.p), where 'exp' is the name of the experiment (and
#' respective dataset), and 'oanova.p' and 'OANOVA.F' are the p and F values 
#' obtained by the solution. 
#' @param graphics_conf a list with different graphics configurations to be used by
#' @param alpha the alpha level according to which we consider an effect as
#' @param eps an epsilon value to be used when transforming p-values to log values
#' significant
#' @return the plot describing the results of the QUID test
generate_OANOVA_plot <- function(data, graphics_conf, alpha = .05, eps = 10^-3) { 
  n_sim <- sum(data$is_sim)
  data <- data %>%
    mutate(effect = oanova.p <= alpha, log_p = ifelse(oanova.p == 0, log10(eps),log10(oanova.p)))
  data_sim_rect <- data.frame(xmin = .75, xmax = n_sim + .25, 
                              ymin = log10(eps-eps/5), ymax = log10(1))
  exp_label_colors <- sapply(data$effect, function(e) ifelse(e, graphics_conf$significant_color, 'black'))
  exp_label_face <- sapply(data$effect, function(e) ifelse(e, "bold","plain"))
  # set symmetric bf log transformed ticks
  ticks_p <- log10(c(10^-3, 10^-2, alpha, 10^-1, .5, 1))
  plt <- ggplot(data, aes(x = exp, y = log_p, fill = effect, shape = is_sim)) +
    geom_rect(data = data_sim_rect, 
              aes(NULL, NULL , xmin = xmin, xmax = xmax, ymin = ymin, 
                  ymax = ymax,shape = NULL),
              fill = graphics_conf$sim_rect_color, alpha = .5) +
    xlab('Experiment') +
    ylab('p-value') +
    geom_hline(yintercept = log10(alpha),linetype='solid', 
               linewidth = 1.5, color = graphics_conf$pale_color) +
    geom_hline(yintercept = log10(1),linetype='solid', 
               linewidth = 1.5, color = 'black') +
    geom_point(size = 5.5, colour = 'black', stroke =2) +
    scale_shape_manual(values = graphics_conf$shapes_per_sim) +
    ggtitle(graphics_conf$title) +
    theme_classic() +
    scale_fill_manual(breaks = c(TRUE,FALSE),
                      values=c(graphics_conf$significant_color,
                               graphics_conf$ns_color))+
    theme(legend.position = 'none',
          plot.title = element_text(size = graphics_conf$title_size, hjust = 0,
                                    margin=margin(0,0,30,0)),
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.text.x = element_text(size = graphics_conf$x_text_size, vjust = .1,
                                     colour = exp_label_colors, face = exp_label_face,
                                     angle = 45),
          axis.title.y = element_markdown(margin = margin(r = 25)),
          axis.title.x = element_text(size = graphics_conf$x_title_size, vjust = -3),
          plot.margin = (unit(c(.5, .5, 1, .5), "cm"))) +
    
    scale_x_discrete(expand=c(0.02, 0)) +
    scale_y_continuous(label = function(x) round(10^x,digits = 3),
                       limits = c(log10(eps-eps/5), log10(1)), 
                      breaks= ticks_p)
  res_plt <- add_y_separators(plt, y_seps = log10(c(eps * 5/3, eps * 4/3)),
                              y_lim = log10(c(eps, 1)), min_x = .75,
                              angle = graphics_conf$seps_angle, length = graphics_conf$seps_length, 
                              linewidth= graphics_conf$seps_lw)
  return (res_plt)
}

#' generate_gnt_plot
#' The function generates the GNT results sub-plot
#' @param data a dataframe with the results of the GNT solution. The shape of 
#' the dataframe is (#Datasets) X (exp, gnt.p, gnt.stat, gnt.ci_low, gnt.ci_high), where 'exp' is the name of 
#' the experiment (and respective dataset), 'gnt.ci_low' and 'gnt.ci_high' are the lower,
#' and higher bounds on prevalence, gnt.stat is the prevalence statistic and 'gnt.p' is the
#' p value for the test against zero prevalence (global null) 
#' @param graphics_conf a list with different graphics configurations to be used by
#' @return the plot describing the results of the GNT test
generate_gnt_plot <- function(data, graphics_conf) {
  alpha <- .05
  n_sim <- sum(data$is_sim)
  data_sim_rect <- data.frame(xmin = .75, xmax = n_sim + .25, 
                              ymin = 100*min(data$gnt.ci_low), ymax = 100*max(data$gnt.ci_high))
  data$effect <- data$gnt.p < alpha
  exp_label_colors <- sapply(data$effect, function(e) ifelse(e, graphics_conf$significant_color, 'black'))
  exp_label_face <- sapply(data$effect, function(e) ifelse(e, "bold","plain"))
  plt <- ggplot(data, aes(x = exp, fill = effect, shape = is_sim)) +
    geom_rect(data = data_sim_rect, aes(NULL, NULL, xmin = xmin, xmax = xmax, 
                                        ymin = ymin, ymax = ymax, shape = NULL), 
              fill = graphics_conf$sim_rect_color, alpha = .5) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 100 * alpha, linetype='solid', 
               linewidth = 1.5, color = graphics_conf$pale_color) +
    geom_errorbar(aes(ymin=gnt.ci_low * 100, ymax=gnt.ci_high*100),
                  width = .75, linewidth  = 1) +
    geom_point(aes(y = gnt.stat*100), size = 5, colour = 'black', stroke =2,
               fill = ifelse(data$effect, graphics_conf$significant_color,
                             ifelse(data$gnt.stat > 0,graphics_conf$med_color,
                              graphics_conf$null_support_color))) +
    scale_shape_manual(values = graphics_conf$shapes_per_sim) +
    xlab('Experiment') +
    ylab('Estimated prevalence\nof within-subject effects (%)') +
    ggtitle(graphics_conf$title) +
    theme_classic() +
    scale_fill_manual(breaks = c(TRUE,FALSE),
                       values=c(graphics_conf$med_color,
                                graphics_conf$null_support_color))+
    theme(legend.position = 'none',
          plot.title = element_text(size = graphics_conf$title_size, hjust = 0,
                                    margin=margin(0,0,30,0)),
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.text.x = element_text(size = graphics_conf$x_text_size, vjust = .1,
                                     colour = exp_label_colors, face = exp_label_face,
                                     angle = 45),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.title.x = element_text(size = graphics_conf$x_title_size, vjust = -3),
          axis.title.y = element_text(margin = margin(t = 0, r = 17, b = 0, l = 0)),
          plot.margin = (unit(c(.5, .5, 1, .5), "cm"))) +
    scale_x_discrete(expand=c(0.0, 0)) +
    coord_cartesian(clip = "off", xlim = c(.75, NA))   
  return (plt)
}

#' get_short_exp_lbl
#' the function gets a full study label, and returns a short label according to
#' the format X where X is the first letter of the first author name, or XY
#' where Y is the first letter of the second author when there are only two
#' authors
#'
#' @param study_label 
#' @param nchar indicating how many chars from each author to use for 
#' the output label
#' @param study_lbl_sep the separator between authors and study year
#' @param co_authors_sep the co-authors separator (in case the study label 
#' includes two authors) 
#' @return the function returns a short label including the initial letter of the
#' authors (or the intitial letters of two authors if the study was conducted
#' by two authors)
get_short_exp_lbl <- function(study_label, nchar=1, study_lbl_sep = '_', co_authors_sep = '&') {
  only_authors <- str_split(study_label, study_lbl_sep)[[1]][1]
  all_authors_names <- str_split(only_authors, co_authors_sep)[[1]]
  return(paste(sapply(all_authors_names, function(s) toupper(substr(trimws(s), 1, nchar))), collapse = ''))
} 

# read the results of the UC database 
res_summary_fn <- 'Unconscious Processing_Results.csv'
uc_results <- read.csv(paste(results_fld, res_summary_fn, sep=.Platform$file.sep))
uc_results$long_exp_name <- uc_results$exp
short_exp_names <- sapply(unique(uc_results$exp), get_short_exp_lbl)
exp_reps <- uc_results %>% group_by(exp) %>% summarise(n=n())
transformed_exp_name <- paste0(short_exp_names, data.table::rowid(short_exp_names))
uc_results$exp <-rep(transformed_exp_name, exp_reps$n)  
uc_results <- add_simulation_results(uc_results)
uc_results$is_quid_valid <- uc_results$quid_bf != invalid_res
uc_results$exp <- factor(uc_results$exp,
                      levels = unique(uc_results[order(!uc_results$is_sim, !uc_results$is_quid_valid),'exp']))
uc_ns_results <- uc_results[uc_results$directional_test.p > alpha | uc_results$is_sim,]
uc_effect_results <- uc_results[uc_results$directional_test.p <= alpha | uc_results$is_sim,]

# get only n.s results in the directional test for RT effects, and no interaction (valid for quid)
effect_results_quid <- uc_effect_results[uc_ns_results$is_quid_valid == TRUE,]
n_effect <- length(unique(uc_effect_results$exp))
n_ns <- length(unique(uc_ns_results$exp))

# generate dataframes for analyses
# GNT
ns_gnt_res <- uc_ns_results %>% group_by(exp) %>% summarise_all(first) %>%
  dplyr::select(exp, gnt.ci_high,gnt.ci_low, gnt.stat, gnt.p, is_sim)
effect_gnt_res <- uc_effect_results %>% group_by(exp) %>% summarise_all(first) %>%
  dplyr::select(exp, gnt.ci_high,gnt.ci_low, gnt.stat, gnt.p, is_sim)
# QUID
ns_results_quid <- uc_ns_results[uc_ns_results$is_quid_valid == TRUE,] %>% 
  group_by(exp) %>% summarise_all(first)
effect_results_quid <- uc_effect_results[uc_effect_results$is_quid_valid == TRUE,] %>% 
  group_by(exp) %>% summarise_all(first)

ns_quid_res <- ns_results_quid %>% 
  dplyr::select(exp, quid_bf, is_sim) %>%
  mutate(quid_bf = 1/quid_bf)
ns_n_quid <- nrow(ns_quid_res)
effect_quid_res <- effect_results_quid %>% 
  dplyr::select(exp, quid_bf, is_sim) %>%
  mutate(quid_bf = 1/quid_bf)
effect_n_quid <- nrow(effect_quid_res)
# OANOVA Test
ns_OANOVA_res <- uc_ns_results %>% filter(oanova.p != invalid_res) %>% 
  group_by(exp) %>% summarise_all(first) %>%
  dplyr::select(exp, oanova.p, is_sim)
ns_n_OANOVA <- nrow(ns_OANOVA_res)
effect_OANOVA_res <- uc_effect_results %>% filter(oanova.p != invalid_res) %>%
  group_by(exp) %>% summarise_all(first) %>%
  dplyr::select(exp, oanova.p, is_sim)
effect_n_OANOVA <- nrow(effect_OANOVA_res)

# Non-directional sign-consistency
ns_signcon_nondir_res <- uc_ns_results %>% 
  dplyr::select(exp, signcon.p, signcon.statistic,
                signcon.null_dist, is_sim) %>% 
  rename(p = signcon.p, stat = signcon.statistic,
         null = signcon.null_dist)
effect_signcon_nondir_res <- uc_effect_results %>% 
  dplyr::select(exp, signcon.p, signcon.statistic, 
                signcon.null_dist, is_sim) %>% 
  rename(p = signcon.p, stat = signcon.statistic,
         null = signcon.null_dist)

# Non-directional absolute effect size
ns_abs_es_nondir_res <- uc_ns_results %>% 
  dplyr::select(exp, absolute_es.p, absolute_es.statistic,
                absolute_es.null_dist, is_sim) %>% 
  rename(p = absolute_es.p, stat = absolute_es.statistic,
         null = absolute_es.null_dist)
effect_abs_es_nondir_res <- uc_effect_results %>% 
  dplyr::select(exp, absolute_es.p, absolute_es.statistic,
                absolute_es.null_dist, is_sim) %>% 
  rename(p = absolute_es.p, stat = absolute_es.statistic,
         null = absolute_es.null_dist)

# Directional
dir_res_all <- uc_results %>% 
  dplyr::select(exp, directional_test.p, directional_test.statistic, 
                directional_test.ci_low, directional_test.ci_high, is_sim) %>% 
  rename(p = directional_test.p, stat = directional_test.statistic,
         ci.low = directional_test.ci_low, ci.high = directional_test.ci_high)

## Plots
# configure the graphics of the figure
graphics_conf <- list(title_size = 30, size_seg = 2,
                      ns_color = 'gray', significant_color = '#116897', 
                      null_support_color = 'white', med_color = 'gray', pale_color = "#E9CB9A", 
                      vline_size = 3, vline_color = '#4eaf4a',
                      dist_below_color = '#dadada', dist_above_color = '#dadada',
                      x_title_size = 22, 
                      x_text_size = 17, sim_rect_color = '#8A62A4', 
                      shapes_per_sim = list('TRUE' = 22, 'FALSE' = 21),
                      seps_angle = 30, seps_lw = .7, seps_length = .1)


# generate the GNT sub-plot
graphics_conf$title = 'Global Null Test (GNT)'
ns_plt_gnt <- generate_gnt_plot(ns_gnt_res,graphics_conf)
ns_plt_gnt
effect_plt_gnt <- generate_gnt_plot(effect_gnt_res,graphics_conf)
effect_plt_gnt

# GNT stats
ns_n_effect_stat <- sum(ns_gnt_res$gnt.stat > 0)
ns_n_null_stat <- nrow(ns_gnt_res) - ns_n_effect_stat 
paste('NS Effects (GNT):')
paste('N_Total, N_Effect, N_Null =', ns_n_effect_stat + ns_n_null_stat, ',',
      ns_n_effect_stat, ',', ns_n_null_stat)
paste('Max, Med, IQR (STAT) =', max(ns_gnt_res$gnt.stat), median(ns_gnt_res$gnt.stat),
      IQR(ns_gnt_res$gnt.stat))
effect_n_effect_stat <- sum(effect_gnt_res$gnt.stat > 0)
effect_n_null_stat <- nrow(effect_gnt_res) - effect_n_effect_stat 
paste('Directional Effects (GNT):')
paste('N_Total, N_Effect, N_Null =', effect_n_null_stat + effect_n_effect_stat, ',',
      effect_n_null_stat, ',', effect_n_effect_stat)
paste('Max, Med (STAT) =', max(effect_gnt_res$gnt.stat), median(effect_gnt_res$gnt.stat))

# generate the QUID sub-plot
graphics_conf$title <- 'Qualitative Individual Differences (QUID)'
eps <- 10^-2
bf_criteria <- 3
graphics_conf$annotate_bfs_x_low <- 4.1
graphics_conf$annotate_bfs_x_high <- 5.5
graphics_conf$annotate_bfs_y_space_low <- log10(1.75)
graphics_conf$annotate_bfs_y_space_high <- log10(1.8)
ns_plt_quid <- generate_quid_plot(ns_quid_res,graphics_conf, criteria = bf_criteria)
ns_plt_quid
effect_graphics_conf <- graphics_conf
effect_graphics_conf$annotate_bfs_x_low <- 2.8
effect_graphics_conf$annotate_bfs_x_high <- 3.2
effect_graphics_conf$annotate_bfs_y_space_low <- log10(2.1)
effect_graphics_conf$annotate_bfs_y_space_high <- log10(4)
effect_graphics_conf$seps_length = graphics_conf$seps_length / 2
effect_plt_quid <- generate_quid_plot(effect_quid_res,effect_graphics_conf, criteria = bf_criteria)
effect_plt_quid

ns_n_effect_QUID <- sum(ns_quid_res$quid_bf > bf_criteria)
ns_n_null_QUID <- sum(ns_quid_res$quid_bf < 1/bf_criteria)
paste('NS Effects (QUID):')
paste('Max, Med =', max(ns_quid_res$quid_bf), median(ns_quid_res$quid_bf))
ns_inconclusive <- ns_quid_res$quid_bf[(ns_quid_res$quid_bf >= 1/bf_criteria) &
                                   (ns_quid_res$quid_bf <= bf_criteria)]
effect_n_effect_QUID <- sum(effect_quid_res$quid_bf > bf_criteria)
effect_n_null_QUID <- sum(effect_quid_res$quid_bf < 1/bf_criteria)
paste('Directional Effects (QUID):')
paste('Max, Med =', max(effect_quid_res$quid_bf), median(effect_quid_res$quid_bf))
effect_inconclusive <- effect_quid_res$quid_bf[(effect_quid_res$quid_bf >= 1/bf_criteria) &
                                      (effect_quid_res$quid_bf <= bf_criteria)]
# generate the OANOVA test sub-plot
graphics_conf$title <- 'Omnibus ANOVA Test (OANOVA)'
eps <- 10^-3
ns_plt_OANOVA <- generate_OANOVA_plot(ns_OANOVA_res,graphics_conf, eps = 10^-3)
ns_plt_OANOVA
effect_graphics_conf <- graphics_conf
effect_graphics_conf$seps_length = graphics_conf$seps_length / 2
effect_plt_OANOVA <- generate_OANOVA_plot(effect_OANOVA_res,effect_graphics_conf, eps = 10^-3)
effect_plt_OANOVA

# OANOVA test stats
ns_n_null <- nrow(ns_OANOVA_res) 
ns_n_effect <- nrow(effect_OANOVA_res) 
paste('NS Effects (OANOVA Test):')
paste('N_Total, N_Effect, N_Null =', ns_n_null + ns_n_effect, ',',
      ns_n_effect, ',', ns_n_null)

# generate the signcon NDT sub-plot
graphics_conf$title <- 'Sign-Consistency (%)'
ns_plt_signcon <- generate_signcon_plot(ns_signcon_nondir_res,graphics_conf)
ns_plt_signcon
effect_plt_signcon <- generate_signcon_plot(effect_signcon_nondir_res,graphics_conf)
effect_plt_signcon

# generate the absolute effect size sub-plot
graphics_conf$title <- 'Absolute effect size'
ns_plt_abs_es <- generate_abs_es_plot(ns_abs_es_nondir_res,graphics_conf)
ns_plt_abs_es
effect_plt_abs_es <- generate_abs_es_plot(effect_abs_es_nondir_res,graphics_conf)
effect_plt_abs_es


# aggregate together the available tests results (GNT, QUID, OANOVA)
# ns directional
ns_plt_gnt <- ns_plt_gnt + labs(tag = "A") + theme(plot.tag = element_text(size = 30))
ns_plt_quid <- ns_plt_quid + labs(tag = "B")  + theme(plot.tag = element_text(size = 30))
ns_plt_OANOVA <- ns_plt_OANOVA + labs(tag = "C")  + theme(plot.tag = element_text(size = 30))

ns_plt_available <- grid.arrange(arrangeGrob(ns_plt_gnt, nrow = 1, widths = c(1)),
                          arrangeGrob(ns_plt_quid, nrow = 1, 
                                      widths = c(ns_n_quid/n_ns + .025,1-ns_n_quid/n_ns - .025)),
                          arrangeGrob(ns_plt_OANOVA, nrow = 1, 
                                      widths = c(ns_n_OANOVA/n_ns + .025,1-ns_n_OANOVA/n_ns - .025)))
ggsave(paste(plots_fld, 'ns_available_methods_res.svg', sep = .Platform$file.sep),
       width=15, height=15,plot = ns_plt_available)
ggsave(paste(plots_fld, 'ns_available_methods_res.png', sep = .Platform$file.sep),
       width=15, height=15,plot = ns_plt_available, dpi = 500)
# effect plot
effect_plt_gnt <- effect_plt_gnt + labs(tag = "A") + theme(plot.tag = element_text(size = 20))
effect_plt_quid <- effect_plt_quid + labs(tag = "B")  + theme(plot.tag = element_text(size = 20))
effect_plt_OANOVA <- effect_plt_OANOVA + labs(tag = "C")  + theme(plot.tag = element_text(size = 20))
effect_plt_available <- grid.arrange(arrangeGrob(effect_plt_gnt, nrow = 1, widths = c(1)),
                             arrangeGrob(effect_plt_quid, nrow = 1, 
                                         widths = c(effect_n_quid/n_effect + .025,1-effect_n_quid/n_effect - .025)),
                             arrangeGrob(effect_plt_OANOVA, nrow = 1, 
                                         widths = c(effect_n_quid/n_effect + .025,1-effect_n_quid/n_effect - .025)))
ggsave(paste(plots_fld, 'effect_available_methods_res.svg', sep = .Platform$file.sep),
       width=15, height=15,plot = effect_plt_available)
ggsave(paste(plots_fld, 'effect_available_methods_res.png', sep = .Platform$file.sep),
       width=15, height=15,plot = effect_plt_available, dpi = 500)

# save the signcon test results
ggsave(paste(plots_fld, 'ns_plt_sign_con__res.svg', sep = .Platform$file.sep),
       width=20, height=9,plot = ns_plt_signcon)
ggsave(paste(plots_fld, 'ns_plt_sign_con__res.png', sep = .Platform$file.sep),
       width=15, height=6,plot = ns_plt_signcon, dpi = 1000)
ggsave(paste(plots_fld, 'effect_plt_sign_con__res.svg', sep = .Platform$file.sep),
       width=15, height=6,plot = effect_plt_signcon)
ggsave(paste(plots_fld, 'effect_plt_sign_con__res.png', sep = .Platform$file.sep),
       width=15, height=6,plot = effect_plt_signcon, dpi = 1000)

# save the absolute effect size test results
ggsave(paste(plots_fld, 'ns_plt_abs_es_res.svg', sep = .Platform$file.sep),
       width=20, height=9,plot = ns_plt_abs_es)
ggsave(paste(plots_fld, 'ns_plt_abs_es__res.png', sep = .Platform$file.sep),
       width=15, height=6,plot = ns_plt_abs_es, dpi = 1000)
ggsave(paste(plots_fld, 'effect_plt_abs_es__res.svg', sep = .Platform$file.sep),
       width=15, height=6,plot = effect_plt_abs_es)
ggsave(paste(plots_fld, 'effect_plt_abs_es__res.png', sep = .Platform$file.sep),
       width=15, height=6,plot = effect_plt_abs_es, dpi = 1000)
