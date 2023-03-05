library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)
library(ggtext)

figures_fld <- 'figures'
source(paste(figures_fld, 'plotting_utils.R', sep = .Platform$file.sep))

# set alpha value
alpha <- .05
invalid_quid_res <- INVALID_VALUE_CODE

#' generate_NDT_plot
#' The function generates the sign-consistency (non-directional test) results sub-plot
#' @param data a dataframe with the results of the sign-consistency solution. The shape of 
#' the dataframe is (#Datasets) X (exp, p), where 'exp' is the name of the experiment (and
#' respective dataset), and 'p' is the p-value according to the sign-consistency solution. 
#' @param graphics_conf a list with different graphics configurations to be used by
#' @param alpha an alpha value highlight significant results.
#' @param is_sc a binary argument indicating whether to plot a sign-consistency (true)
#' or directional (false) plot.
#' @param quid_n the number of experiments that can be analyzed with quid

#' @return the plot describing the results of the (non-directional) sign-consistency test
generate_nhst_plot <- function(data, graphics_conf, alpha = .05, is_sc = TRUE, 
                               quid_n = invalid_quid_res) {
  if(is_sc) {
    title <- 'Sign-Consistency Test'
    scale <- 100
    data <- data %>%
      mutate(ci.low = stat, ci.high = stat)
  } else {
    title <- 'Directional Test'
    scale <- 1
    data <- data %>% mutate(scores = stat)
  }
  data <- data %>%
    mutate(effect = p <= alpha, stat = stat * scale, ci.low = ci.low * scale, ci.high = ci.high * scale)
  plt <- ggplot(data, aes(x = exp, y = stat, fill = effect)) +
    xlab('Experiment') +
    ylab(graphics_conf$y_title) +
    ggtitle(title) +
    geom_jitter(aes(x = exp, y = scores), size = 1.5, shape=16, 
                fill = 'black', height = 0, width = .2, alpha = ifelse(is_sc,.5,0)) +
    geom_errorbar(aes(ymin = ci.low, ymax = ci.high, width = ifelse(is_sc, 0, 0.5))) +
    geom_point(size = 5, shape=21, 
               colour = 'black', stroke =2) +
    ylim(min(data$scores) - .1, max(data$scores) + .1) +
    geom_vline(xintercept = ifelse(quid_n == invalid_quid_res, 1,quid_n + .5),
               linewidth = ifelse(quid_n == invalid_quid_res, 0, 2), linetype='dotted',
               color = graphics_conf$split_color) +
    theme_classic() +
    scale_fill_manual(breaks = c(TRUE,FALSE),
                       values=c(graphics_conf$significant_color,
                                graphics_conf$ns_color))+
    theme(legend.position = 'none',
          plot.title = element_text(size = graphics_conf$title_size, hjust = .5),
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
  
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
    ylab('log<sub>10</sub>(BF)<br><i>global null</i> ↓') +
    geom_hline(yintercept = 0, linetype='dotted', linewidth = 2) +
    geom_hline(yintercept = c(log10(criteria),log10(1/criteria)), 
               linetype='solid', linewidth = 1.5, color = graphics_conf$pale_color) +
    geom_point(size = 5.5, shape=21, 
               colour = 'black', stroke =2) +
    ggtitle('Qualitative Individual Differences (QUID)') +
    theme_classic() +
    scale_fill_manual(breaks = c('effect','uncertain','ns'),
                       values=c(graphics_conf$significant_color,
                                graphics_conf$med,
                                graphics_conf$ns_color))+
    theme(legend.position = 'none',
          plot.title = element_text(size = graphics_conf$title_size, hjust = .5),
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.title.y = element_markdown(margin = margin(t = 0, r = 5, b = 0, l = 0))) +
    ylim(min(c(data$log_bf, log10(1/criteria))) - .1,
         max(c(data$log_bf, log10(criteria))) + .1)


  return (plt)
}

#' generate_pbt_plot
#' The function generates the PBT results sub-plot
#' @param data a dataframe with the results of the QUID solution. The shape of 
#' the dataframe is (#Datasets) X (exp, pbt.low, pbt.high), where 'exp' is the name of 
#' the experiment (and respective dataset), 'pbt.low' and 'pbt.high' are the lower,
#' and higher bounds of the HDI according to the solution. 
#' @param graphics_conf a list with different graphics configurations to be used by
#' @param quid_n the number of experiments that can be analyzed with quid
#'
#' @return the plot describing the results of the PBT test
generate_pbt_plot <- function(data, graphics_conf, quid_n = invalid_quid_res) {
  data$effect <- data$pbt.low > 0
  plt <- ggplot(data, aes(x = exp, fill = effect)) +
    geom_errorbar(aes(ymin  = pbt.low * 100, ymax  = pbt.high*100),
                  width = .15, linewidth  = 1.5) +
    geom_point(aes(y = pbt.MAP*100), size = 5, shape=21, 
               colour = 'black', stroke =2,
               fill = ifelse(data$effect, graphics_conf$significant_color,
                             ifelse(data$pbt.MAP > 0,graphics_conf$med_color,
                              graphics_conf$ns_color))) +
    xlab('Experiment') +
    ylab('Estimated prevalence\nof within-subject effects (%)') +
    ggtitle('Prevalence Bayesian Test (PBT)') +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = ifelse(quid_n == invalid_quid_res, 1,quid_n + .5),
               linewidth = ifelse(quid_n == invalid_quid_res, 0, 2), linetype='dotted',
              color = graphics_conf$split_color) +
    theme_classic() +
    scale_fill_manual(breaks = c(TRUE,FALSE),
                       values=c(graphics_conf$med_color,
                                graphics_conf$ns_color))+
    
    theme(legend.position = 'none',
          plot.title = element_text(size = graphics_conf$title_size, hjust = .5),
          axis.text = element_text(size = graphics_conf$x_text_size),
          axis.title = element_text(size = graphics_conf$x_title_size),
          axis.title.y = element_text(margin = margin(t = 0, r = 17, b = 0, l = 0)),
          ) +
  ylim(0, plyr::round_any(max(data$pbt.high) * 100, accuracy = 0.1, f = ceiling))
  
  return (plt)
}

get_initials <- function(name, nchar=1, sep = '_', co_sep = '&') {
  only_name <- str_split(name, sep)[[1]][1]
  names <- str_split(only_name, co_sep)[[1]]
  return(paste(sapply(names, function(s) toupper(substr(trimws(s), 1, nchar))), collapse = ''))
} 

# configure the graphics of the figure
graphics_conf <- list(title_size = 40, size_seg = 2, color_spreading_lines = '#71E9CC',
                      margin_y_subj = 0.5, margin_y_conds = 0.125, legnth_med = 2,
                      ns_color = 'white', significant_color = 'red', 
                      med_color = 'gray', pale_color = "#E9CB9A", split_color = 'black',
                      vline_size = 1, x_title_size = 22, x_text_size = 16)
# read the results of the UC database 
res_summary_fn <- 'Unconscious Processing_Results.csv'
results <- read.csv(paste(results_fld, res_summary_fn, sep=.Platform$file.sep))
results$long_exp_name <- results$exp
short_exp_name <- sapply(unique(results$exp), get_initials)
exp_reps <- results %>% group_by(exp) %>% summarise(n=n())
transformed_exp_name <- paste0(short_exp_name, data.table::rowid(short_exp_name))
results$exp <-rep(transformed_exp_name, exp_reps$n)  
results$is_quid_valid <- results$quid_bf != invalid_quid_res
results$exp <- factor(results$exp,
                      levels = unique(results[order(-results$is_quid_valid),'exp']))
ns_results <- results[results$directional_effect.p > alpha,]
effect_results <- results[results$directional_effect.p <= alpha,]

# get only n.s results in the directional test for RT effects, and no interaction (valid for quid)
effect_results_quid <- effect_results[ns_results$is_quid_valid == TRUE,]
n_effect <- length(unique(effect_results$exp))
n_ns <- length(unique(ns_results$exp))

# generate dataframes for analyses
# PBT
ns_pbt_res <- ns_results %>% group_by(exp) %>% summarise_all(first) %>%
  dplyr::select(exp, pbt.high,pbt.low, pbt.MAP)
effect_pbt_res <- effect_results %>% group_by(exp) %>% summarise_all(first) %>%
  dplyr::select(exp, pbt.high,pbt.low, pbt.MAP)
# QUID
ns_results_quid <- ns_results[ns_results$is_quid_valid == TRUE,] %>% 
  group_by(exp) %>% summarise_all(first)
effect_results_quid <- effect_results[effect_results$is_quid_valid == TRUE,] %>% 
  group_by(exp) %>% summarise_all(first)

ns_quid_res <- ns_results_quid %>% 
  dplyr::select(exp, quid_bf) %>%
  mutate(quid_bf = 1/quid_bf)
ns_n_quid <- nrow(ns_quid_res)
effect_quid_res <- effect_results_quid %>% 
  dplyr::select(exp, quid_bf) %>%
  mutate(quid_bf = 1/quid_bf)
effect_n_quid <- nrow(effect_quid_res)
# Non-directional
ns_nondir_res <- ns_results %>% 
  dplyr::select(exp, non_directional.p, non_directional.statistic, non_directional.consistency_per_id.score) %>% 
  rename(p = non_directional.p, stat = non_directional.statistic,
         scores = non_directional.consistency_per_id.score) %>% mutate(scores =  scores * 100)
nondir_res_all <- results %>% 
  dplyr::select(exp, non_directional.p, non_directional.statistic, non_directional.consistency_per_id.score) %>% 
  rename(p = non_directional.p, stat = non_directional.statistic,
         scores = non_directional.consistency_per_id.score) %>% mutate(scores =  scores * 100)
effect_nondir_res <- effect_results %>% 
  dplyr::select(exp, non_directional.p, non_directional.statistic, non_directional.consistency_per_id.score) %>% 
  rename(p = non_directional.p, stat = non_directional.statistic,
         scores = non_directional.consistency_per_id.score) %>% mutate(scores =  scores * 100)
# Directional
dir_res_all <- results %>% dplyr::select(exp, directional_effect.p, directional_effect.statistic, directional_effect.ci_low, directional_effect.ci_high) %>% 
  rename(p = directional_effect.p, stat = directional_effect.statistic,
         ci.low = directional_effect.ci_low, ci.high = directional_effect.ci_high)

# generate the PBT sub-plot
ns_plt_pbt <- generate_pbt_plot(ns_pbt_res,graphics_conf, quid_n = ns_n_quid)
ns_plt_pbt
effect_plt_pbt <- generate_pbt_plot(effect_pbt_res,graphics_conf, quid_n = effect_n_quid)
effect_plt_pbt
# PBT stats
ns_n_effect_MAP <- sum(ns_pbt_res$pbt.MAP > 0)
ns_n_null_MAP <- nrow(ns_pbt_res) - ns_n_effect_MAP 
paste('NS Effects (PBT):')
paste('N_Total, N_Effect, N_Null =', ns_n_effect_MAP + ns_n_null_MAP, ',',
      ns_n_effect_MAP, ',', ns_n_null_MAP)
paste('Max, Med, IQR (MAP) =', max(ns_pbt_res$pbt.MAP), median(ns_pbt_res$pbt.MAP),
      IQR(ns_pbt_res$pbt.MAP))
effect_n_effect_MAP <- sum(effect_pbt_res$pbt.MAP > 0)
effect_n_null_MAP <- nrow(effect_pbt_res) - effect_n_effect_MAP 
paste('Directional Effects (PBT):')
paste('N_Total, N_Effect, N_Null =', effect_n_null_MAP + effect_n_effect_MAP, ',',
      effect_n_null_MAP, ',', effect_n_effect_MAP)
paste('Max, Med (MAP) =', max(effect_pbt_res$pbt.MAP), median(effect_pbt_res$pbt.MAP))

# generate the QUID sub-plot
bf_criteria <- 3
ns_plt_quid <- generate_quid_plot(ns_quid_res,graphics_conf, criteria = bf_criteria)
ns_plt_quid
effect_plt_quid <- generate_quid_plot(effect_quid_res,graphics_conf, criteria = bf_criteria)
effect_plt_quid
ns_n_effect_QUID <- sum(ns_quid_res$quid_bf > bf_criteria)
ns_n_null_QUID <- sum(ns_quid_res$quid_bf < 1/bf_criteria)
paste('NS Effects (QUID):')
paste('Max, Med =', max(ns_quid_res$quid_bf), median(ns_quid_res$quid_bf))
inconclusive <- ns_quid_res$quid_bf[(ns_quid_res$quid_bf >= 1/bf_criteria) &
                                   (ns_quid_res$quid_bf <= bf_criteria)]
effect_n_effect_QUID <- sum(effect_quid_res$quid_bf > bf_criteria)
effect_n_null_QUID <- sum(effect_quid_res$quid_bf < 1/bf_criteria)
paste('Directional Effects (QUID):')
paste('Max, Med =', max(effect_quid_res$quid_bf), median(effect_quid_res$quid_bf))
inconclusive <- effect_quid_res$quid_bf[(effect_quid_res$quid_bf >= 1/bf_criteria) &
                                      (effect_quid_res$quid_bf <= bf_criteria)]

# generate the NDT sub-plot
graphics_conf$y_title <- 'Sign-Consistency (%)'
ns_plt_nondir <- generate_nhst_plot(ns_nondir_res,graphics_conf, quid_n = ns_n_quid)
ns_plt_nondir
effect_plt_nondir <- generate_nhst_plot(effect_nondir_res,graphics_conf, quid_n = effect_n_quid)
effect_plt_nondir


# generate the non / directional test figure
graphics_conf$y_title <- 'Sign-Consistency (%)'
plt_nondir <- generate_nhst_plot(nondir_res_all,graphics_conf)
plt_nondir

graphics_conf$y_title <- 'Statistic'
plt_dir <- generate_nhst_plot(dir_res_all,graphics_conf, is_sc = FALSE)
plt_dir <- plt_dir + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank())

# aggregate together the Bayesian tests results (PBT & QUID)
# ns directional
ns_plt_pbt <- ns_plt_pbt + labs(tag = "A") + theme(plot.tag = element_text(size = 30))
ns_plt_quid <- ns_plt_quid + labs(tag = "B")  + theme(plot.tag = element_text(size = 30))

ns_plt_bayes <- grid.arrange(arrangeGrob(ns_plt_pbt, nrow = 1, widths = c(1)),
                          arrangeGrob(ns_plt_quid, nrow = 1, 
                                      widths = c(ns_n_quid/n_ns + .025,1-ns_n_quid/n_ns - .025)))
ggsave(paste(plots_fld, 'ns_bayes_methods_res.svg', sep = .Platform$file.sep),
       width=15, height=12,plot = ns_plt_bayes)
# directional effect
effect_plt_pbt <- effect_plt_pbt + labs(tag = "a.") + theme(plot.tag = element_text(size = 20))
effect_plt_quid <- effect_plt_quid + labs(tag = "b.")  + theme(plot.tag = element_text(size = 20))

effect_plt_bayes <- grid.arrange(arrangeGrob(effect_plt_pbt, nrow = 1, widths = c(1)),
                             arrangeGrob(effect_plt_quid, nrow = 1, 
                                         widths = c(effect_n_quid/n_effect + .025,1-effect_n_quid/n_effect - .025)))
ggsave(paste(plots_fld, 'effect_bayes_methods_res.svg', sep = .Platform$file.sep),
       width=15, height=12,plot = effect_plt_bayes)

# aggregate together the NHST tests results (Sign-Consistency & Directional permutations)
ggsave(paste(plots_fld, 'ns_plt_sign_con__res.svg', sep = .Platform$file.sep),
       width=15, height=6,plot = ns_plt_nondir)
ggsave(paste(plots_fld, 'effect_plt_sign_con__res.svg', sep = .Platform$file.sep),
       width=15, height=6,plot = effect_plt_nondir)
plt_nhst <- grid.arrange(plt_dir, plt_nondir, ncol = 1, heights = c(0.5, 1))
ggsave(paste(plots_fld, 'plt_nhst_methods_res.svg', sep = .Platform$file.sep),
       width=15, height=12,plot = plt_nhst)
