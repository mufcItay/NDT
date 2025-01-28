library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)
library(tidyr)
library(scales)
library(ggh4x)

# define figures directory, to store the saved plots
figures_fld <- 'figures'
source(paste(figures_fld, 'plotting_utils.R', sep = .Platform$file.sep))
# define constants for later processing and plotting
field_to_color_pal <- c("Metacognitive Sensitivity" = "#117733", 
           "Confidence" = "#332288", 
           "Cognitive Psychology" = "#DDCC77", 
           "Unconscious Processing" = "#CC6677", 
           "Null" = '#AAAAAA')
field_to_shape <- c("Metacognitive Sensitivity" = 22, 
                        "Confidence" = 23, 
                        "Cognitive Psychology" = 24, 
                        "Unconscious Processing" = 21, 
                        "Null" = 7)

# transform values to get proper labels for the p-values
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format(digits = 2)(x)))
}

# set functions and variable names for each test:
# we have different 'significance' rules, and result column names
# for each test, so we store a mapping between test names and respective functions
test_types <- c("Sign-Consistency", "QUID", "OANOVA", "GNT", "Absolute effect size")
# mapps between results column name to the test name
test_type_to_column_name <- list("signcon.p", "quid_bf", "oanova.p", "gnt.p", "absolute_es.p")
# map results to statistic of tests
test_type_to_statistic_column_name <- list("signcon.statistic", "quid_bf", "oanova.F", "gnt.stat", "absolute_es.statistic")
names(test_type_to_column_name) <- test_types
names(test_type_to_statistic_column_name) <- test_types
nhst_sig <- function(p, alpha = .05) p < alpha
bf_sig <- function(bf, criterion = 3) 1/bf > criterion
test_type_to_sig_f <- list(nhst_sig, bf_sig, nhst_sig, nhst_sig, nhst_sig)
names(test_type_to_sig_f) <- test_types

## DEFINE HELPER FUNCTIONS
# define a function to fetch all results, and fit them into a standard dataframe
read_res_df <- function(fns, result_col_names = c('signcon.p', 'signcon.statistic')) {
  result_col_names <- c(result_col_names, 'directional_test.p')
  all_results <- data.frame()
  # iterate over the result files (one per field) and aggregate them into a large dataframe
  for (fn in fns) {
    cur_results <- read.csv(fn) %>% 
      # we determine the 'type' of each result according to the file name (type = prefix of the file name).
      # in the aggregated dataframe, type = field label (e.g., "Cognitive Psychology"/"Metacognitive Sensitivity"/etc...)
      mutate(type = gsub(basename(fn), pattern="_Results.csv$", replacement="")) %>%
      mutate(oanova.p = ifelse(type == 'Metacognitive Sensitivity', INVALID_VALUE_CODE, oanova.p),
             quid_bf = ifelse(type %in% c('Confidence','Metacognitive Sensitivity'), INVALID_VALUE_CODE, quid_bf)) %>%
      group_by(type,exp) %>%
      # since for some tests we have more than one row per experiment, we get the first one only
      # (here we are interested only in the test final results which is the same across experiment repetitions in the results file)
      summarise_at(result_col_names, first)
    all_results <- rbind(all_results, cur_results)
  }
  return(all_results)
}

# aggreagte the scatter and density plots and save the plot to file
generate_aggregated_plot <- function(test_type, scatter_plt, density_plt, save = TRUE) {
  # create an aggregate plot with both the scatter plot and the density plot
  aggreagated_plt <-  density_plt + scatter_plt +
    plot_layout(ncol = 1, nrow = 2, heights = c(.5,1))
  ggsave(paste(plots_fld,paste0(test_type,'_figure4', '.svg'), sep = .Platform$file.sep),
         plot = aggreagated_plt, width = 10, height = 10)
  
  return(aggreagated_plt)  
}

# the function decides which figure to generate based on the argument 'test_type'
generate_fig4 <- function(test_type, results_df, plot_null_line = FALSE, plot_tag = '') {
  # QUID is the only Bayesian test
  if(test_type == 'QUID') {
    return(generate_fig4_BFs(test_type, results_df, plot_null_line, plot_tag))
  } else {
    return(generate_fig4_frequentist(test_type, results_df, plot_null_line, plot_tag))
  }
}

# the function generates a figure for a frequentist test, and saves it as a file
generate_fig4_frequentist <- function(test_type, results_df, plot_null_line, plot_tag, alpha = .05) {
  print(test_type)
  # set alpha level
  alpha <- .05
  # set epsilon - the lowest p value in the plot (we truncate lower p values for presentation purposes)
  null_dist_nsamples <- 10^4
  eps <- 1 / (1 + null_dist_nsamples)
  # manipulate p values according to eps (fixing extreme low p-values to 1/(1 + null_dist_nsamples))
  # (the sign consistency test use a p-value correction according to (B+1)/(M+1), Phipson & Smyth, 2010)
  results_df$test_result <- ifelse(results_df$test_result < eps,
                                   eps, results_df$test_result)
  ## generate a scatter plot of all results 
  # define locations for significant annotations (e.g., p value < alpha and p.value > alpha)
  sig_areas_x_marg <- .1
  sig_areas_y_marg <- .01
  sig_areas_labels <- data.frame( label = paste0(c("p < ", "p > "), alpha),
                                  x = alpha + c(-sig_areas_x_marg/2.5, sig_areas_x_marg),
                                  y = alpha - rep(sig_areas_y_marg, 2))
  # title for the x axis (test type dependent)
  x_title_prefix <- paste(test_type, 'p-value')
  # create the plot
  scatter_plt <- results_df %>%
    mutate(directional_test.p = eps + directional_test.p*(1-eps)) %>%
    ggplot(aes(x=test_result,y=directional_test.p, fill = type, shape = type)) +
    geom_vline(xintercept = alpha, color = 'black', linewidth = 1.5) +
    geom_point(size = 4, colour = 'black', stroke = 1) +
    scale_shape_manual(values = field_to_shape) +
    scale_fill_manual(values = field_to_color_pal) +
    labs(y=expression('Directional p-value ' ~(log[10])),
         x=as.expression(bquote(.(x_title_prefix)~(log[10])))) +
    scale_y_log10(breaks = c(alpha, 1),
                  labels = c(expression(alpha), '1'),
                  limits = c(0.75 * alpha, 1)) +
    scale_x_log10(limits = c(0.9 * eps, 1),
                  breaks = c(null_dist_nsamples^-1, alpha, 1),
                  labels = c(math_format()(log10(null_dist_nsamples^-1)),expression(alpha),'1')) +
    theme_classic() +
    annotate("text", x = sig_areas_labels$x, y = sig_areas_labels$y, 
             label = sig_areas_labels$label, size = 16/.pt) +
    theme(axis.title.x = element_text(size=22),
          axis.title.y = element_text(size=18),
          axis.text = element_text(size=18),
          legend.position = 'none') +
    guides(x = guide_axis_truncated(
      trunc_lower = c(.9 * eps, 1.2 * eps),
      trunc_upper = c(eps, 1)
    ))+
    coord_fixed()
  
  # plot density plot for each database (analysis type)
  # create a uniform distribution of p-vales to simulate H0 density
  # while normalizing p-values to adjust p = 1
  dp = 0.0001
  p=seq(0,1,dp)
  null_p = p/sum(p)/dp
  dp_df <- data.frame(x_p = p, y_null_p = null_p, type = rep('Null',length(p)))
  # create a density plot to summarise the results of the scatter plot
  density_nondir <- results_df %>%
    mutate(directional_test.p = eps+directional_test.p*(1-eps)) %>%
    ggplot(aes(x = test_result, fill = type, colour = type)) + 
    scale_fill_manual(values = field_to_color_pal) +
    geom_density(alpha = .3, linewidth = 1.5) + 
    geom_line(data = dp_df, aes ( x = x_p, y = y_null_p), linewidth = 2.5, linetype = 'dashed') +
    ylab('Density') +
    scale_color_manual(values = field_to_color_pal) +
    scale_x_log10(limits = c(0.9 * eps, 1),
                  breaks = c(null_dist_nsamples^-1,.01,1),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_y_continuous(breaks = c()) +
    geom_vline(xintercept = alpha, color = 'black', linewidth = 1.5) +
    theme_classic() +
    labs(tag = plot_tag) +
    theme(legend.position = "above",
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=18),
          axis.text = element_text(size=18),
          plot.tag = element_text(size = 30))
  
  return(generate_aggregated_plot(test_type,
                                  scatter_plt, density_nondir))
}

# the function generates a figure for a BFs QUID test, and saves it as a file
generate_fig4_BFs <- function(test_type, results_df, plot_null_line, plot_tag, alpha = 0.05) {
  # define the BF criterion
  bf_criterion <- 3
  # set epsilon - the lowest p value in the plot (we truncate lower p values for presentation purposes)
  null_dist_nsamples <- 10^4
  eps <- 1 / (1 + null_dist_nsamples)
  
  # manipulate BF scores, for the figure (fixing extreme
  # BFs to max_BF or max_BF^-1)
  max_BF <- 10^3
  results_df <- results_df %>%
    mutate(test_result = ifelse(test_result > max_BF, max_BF, 
                                ifelse(test_result < max_BF^-1, max_BF^-1, test_result)))
  ## generate a scatter plot of all results 
  # define locations for significant annotations (e.g., BF < BF_Criterion and BF < BF_Criterion^-1)
  sig_areas_y_marg <- .01
  sig_areas_labels <- data.frame( label = c(paste("BF <", round(1/bf_criterion,2)),
                                            paste("BF >", bf_criterion)),
                                  x = c(1/bf_criterion /3, bf_criterion *3),
                                  y = alpha - rep(sig_areas_y_marg, 2))
  # create the scatter plot
  scatter_plt <- results_df %>%
    mutate(directional_test.p = eps + directional_test.p*(1-eps)) %>%
    ggplot(aes(x=test_result,y=directional_test.p, fill = type, shape = type)) +
    geom_vline(xintercept = c(1/bf_criterion, bf_criterion), color = 'black', linewidth = 1.5) +
    geom_point(size = 4, colour = 'black', stroke = 1) +
    scale_shape_manual(values = field_to_shape) +
    scale_fill_manual(values = field_to_color_pal) +
    labs(y=expression('Directional p-value ' ~(log[10])),
         x=as.expression(QUID~BF['01']~(log[10]))) +
    scale_y_log10(breaks = c(alpha, 1),
                  labels = c(expression(alpha), '1'),
                  limits = c(0.75 * alpha, 1)) +
    scale_x_log10(limits = c(0.01, max_BF),
                  breaks = c(.01, 1, max_BF),
                  labels = c(math_format()(log10(.01)),
                             expression(log[10](1)),
                             math_format()(log10(max_BF)))) +
    theme_classic() +
    annotate("text", x = sig_areas_labels$x, y = sig_areas_labels$y, 
             label = sig_areas_labels$label, size = 16/.pt) +
    theme(axis.title.x = element_text(size=22),
          axis.title.y = element_text(size=18),
          axis.text = element_text(size=18),
          legend.position = 'none') +
    coord_fixed()
  
  # create the density plotsig_percent_nsdir_across_types
  density_nondir <- results_df %>%
    ggplot(aes(x = test_result, fill = type, colour = type)) + 
    scale_fill_manual(values = field_to_color_pal) +
    geom_density(alpha = .3, linewidth = 1.5) + 
    ylab('Density') +
    scale_color_manual(values = field_to_color_pal) +
    scale_x_log10(limits = c(.01, max_BF),
                  breaks = c(.01, max_BF),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_y_continuous(breaks = c()) +
    geom_vline(xintercept = c(1/bf_criterion,bf_criterion), color = 'black', linewidth = 1.5) +
    labs(tag = plot_tag) +
    theme_classic() +
    theme(legend.position = "above",
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=18),
          axis.text = element_text(size=18),
          plot.tag = element_text(size = 30))

  return(generate_aggregated_plot(test_type,
                                  scatter_plt, density_nondir))
}

# define a function that analyzed the results and generates the respective plot
# for each test
analyze_test_results <- function(test_type, all_results_df, plot_null_line = TRUE, plot_tag = '', alpha = .05) {
  # get the significane test function for the specific test type
  sig_test <- test_type_to_sig_f[[test_type]]
  # filter the large results dataframe, to get only the relevant results
  # for the specific test type we want to analyze
  results_df <- all_results_df %>%
    rename(test_result = !!dplyr::sym(test_type_to_column_name[[test_type]])) %>%
    filter(test_result != INVALID_VALUE_CODE)
  
  # summarize the number of experiments in each type (field)
  sum_exp_N <- results_df %>%
    group_by(type) %>%
    summarise(N = n()) %>%
    complete(type, fill = list(N = 0))
  
  # summarize non-significant (ns) directional test results (together 'nsdir')
  # filter the resutls and add FDR corrected p values within analysis type
  nsdir_df <- results_df %>%
    filter(directional_test.p > alpha) %>%
    mutate(type = factor(type)) %>%
    group_by(type) %>%
    mutate(test_result_corrected = 
             p.adjust(test_result, method = 'fdr'))
  
  # summarize the number of experiments in each type, for
  # cases where the directional test was not significant (nsdir)
  sum_nsdir_N <- nsdir_df %>%
    group_by(type) %>%
    summarise(N = n()) %>%
    complete(type, fill = list(N = 0))

  # get the percent of significant non-directional effects in each type uncorrected across all types
  sig_percent_nsdir_across_types <- nsdir_df %>%
    filter(type != "Unconscious Processing") %>%
    mutate(non_dir_effect = sig_test(test_result)) %>% 
    group_by(non_dir_effect) %>%
    summarise(N = n()) %>%
    complete(non_dir_effect, fill = list(N = 0)) %>%
    summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N))) %>%
    dplyr::pull(perc_nondir_sig)
  
  # get the percent of significant non-directional test results
  # for uncorrected test results, per type (field)
  sig_percent_nsdir_per_type <- nsdir_df %>%
    mutate(non_dir_effect = factor(sig_test(test_result))) %>%
    group_by(type, non_dir_effect) %>%
    summarise(N = n()) %>%
    complete(non_dir_effect, fill = list(N = 0)) %>%
    group_by(type) %>%
    summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N)), N = sum(N) - first(N))
  
  # run similar analysis to the one above, but for corrected p-values (according to FDR adjustment)
  # (not the the results would not make sense for Bayesian tests)
  corrected_sig_percent_nsdir_per_type <- nsdir_df %>%
    mutate(non_dir_effect = factor(sig_test(test_result_corrected))) %>%
    group_by(type, non_dir_effect) %>%
    summarise(N = n()) %>%
    complete(non_dir_effect, fill = list(N = 0)) %>%
    group_by(type) %>%
    summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N)))
  # generate a plot with both individual level results, and density plot
  plt <-generate_fig4(test_type, nsdir_df, plot_null_line, plot_tag)
  
  return(list(plot = plt,
              all_exp_types_N = sum_exp_N, nsdir_res = nsdir_df,
              nsdir_N = sum_nsdir_N,
              sig_percent_nsdir_across_types = sig_percent_nsdir_across_types,
              sig_percent_nsdir_per_type = sig_percent_nsdir_per_type))
}

## READ ALL RESULTS
# get all result files
results_fns <- list.files(results_fld, pattern = '.csv', full.names = TRUE) 

# get the results of all tests and fields in one dataframe
all_results_df <- read_res_df(results_fns, result_col_names=
                                c(as.character(test_type_to_column_name),
                                               as.character(test_type_to_statistic_column_name)))

# run the analysis on all tests (the results will be used in the Rmd file)
# the output is a list with a summary of the analysis results and a plot saved to
# the figures\plots folder

all_datasets_absES <- analyze_test_results("Absolute effect size", all_results_df)
all_datasets_signcon <- analyze_test_results('Sign-Consistency', all_results_df)
all_datasets_GNT <- analyze_test_results('GNT', all_results_df, plot_tag = 'A')
all_datasets_QUID <- analyze_test_results('QUID', all_results_df, plot_null_line = F, plot_tag = 'B')
all_datasets_OANOVA <- analyze_test_results('OANOVA', all_results_df, plot_tag = 'C')


