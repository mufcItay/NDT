library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)
library(tidyr)
library(scales)
library('ggh4x')

figures_fld <- 'figures'
source(paste(figures_fld, 'plotting_utils.R', sep = .Platform$file.sep))

alpha <- .05
pal <-   c("#B25E9D", "#3ECFC9", "#F4B26A", "#EB5F4A")

# transform values to get proper labels for the p-values
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format(digits = 2)(x)))
}

# read the results of all tests
results_fns <- list.files(results_fld, pattern = '.csv', full.names = TRUE) 
read_res_df <- function(fn) {
  df <- read.csv(fn) %>%
    mutate(type = gsub(basename(fn), pattern="_Results.csv$", replacement="")) %>%
    group_by(type,exp) %>%
    summarise(signcon.p = first(signcon.p),
              directional_test.p = first(directional_test.p),
              signcon.statistic = first(signcon.statistic)) %>%
    dplyr::select(type, exp,signcon.statistic, signcon.p,directional_test.p)
}
# get a summary of the number of studies per data type
all_results <- do.call(rbind, lapply(results_fns, read_res_df))
sum_N <- all_results %>%
  group_by(type) %>%
  summarise(N = n())
# summarise non-significant results
# fitler ns resutls and add FDR corrected p values within analysis type
null_results <- all_results[all_results$directional_test.p > alpha,] %>%
  group_by(type) %>%
  mutate(signcon.p.corrected = p.adjust(signcon.p,method = 'fdr'))
# summarize ns results
ns_sum_Ns <- null_results %>%
  group_by(type) %>%
  summarise(N = n())
Nunique_ns <- length(unique(paste(null_results$exp,null_results$type, sep = '_')))
# uncorrected across all types
sig_nondir_cat_no_uc <- null_results %>%
  filter(type != "Unconscious Processing") %>%
  mutate(non_dir_effect = signcon.p <= alpha) %>% 
  group_by(non_dir_effect) %>%
  summarise(N = n()) %>%
  summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N))) %>%
  dplyr::pull(perc_nondir_sig)

# uncorrected - significant sign-consistency per category
sig_nondir_cat <- null_results %>%
  mutate(non_dir_effect = signcon.p <= alpha) %>%
  group_by(type, non_dir_effect) %>%
  summarise(N = n()) %>%
  group_by(type) %>%
  summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N)))

corrected_sig_nondir_cat <- null_results %>%
  mutate(non_dir_effect = signcon.p.corrected <= alpha) %>%
  group_by(type, non_dir_effect) %>%
  summarise(N = n()) %>%
  group_by(type) %>%
  summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N)))

sig_nondir_all <- null_results %>%
  mutate(non_dir_effect = signcon.p <= alpha) %>%
  group_by(non_dir_effect) %>%
  summarise(N = n()) %>%
  summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N)))

# within Cognitive Psychology
cog_psy_analysis <- null_results %>%
  filter(startsWith(type, 'Cognitive')) %>%
  mutate(non_dir_effect = signcon.p <= alpha, sub_cat = 
           ifelse(startsWith(exp,'Adam'), 'VS', 
                  ifelse(startsWith(exp,'Battich'), 'Social' ,'Reproducibility'))) %>%
  mutate(sub_cat = factor(sub_cat), non_dir_effect = factor(non_dir_effect)) %>%
  group_by(sub_cat, non_dir_effect) %>%
  summarise(N = n()) %>%
  complete(non_dir_effect, fill = list(N=0)) %>%
  group_by(sub_cat) %>%
  summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N)))


## Summarise Ns
all_n <- length(unique(paste(all_results$type,all_results$exp)))
ns_n <- length(unique(paste(null_results$type,null_results$exp)))


##### GENERATE FIGURE
# generate the scatter plot of all results together 
eps <- 10^-5
sig_areas_x_marg <- .1
sig_areas_y_marg <- .01
sig_areas_labels <- data.frame( label = paste0(c("p < ", "p > "), alpha),
  x = alpha + c(-sig_areas_x_marg/2.5, sig_areas_x_marg),
  y = alpha - rep(sig_areas_y_marg, 2))
scatter_plt <- null_results %>%
  mutate(signcon.p = eps + signcon.p*(1-eps),
         directional_test.p = eps + directional_test.p*(1-eps)) %>%
  ggplot(aes(x=signcon.p,y=directional_test.p, fill = type)) +
  geom_vline(xintercept = alpha, color = 'black', size = 1.5) +
  geom_point(size = 4, colour = 'black', stroke = 1, shape = 21) +
  scale_fill_manual(values = pal) +
  labs(y=expression('Directional p-value ' ~(log[10])),
       x=expression('Sign-Consistency p-value ' ~(log[10]))) +
  scale_y_log10(breaks = c(alpha, 1),
                labels = c(expression(alpha), '1'),
                limits = c(0.75 * alpha, 1)) +
  scale_x_log10(breaks = c(eps,alpha, 1),
                labels = c(math_format()(log10(eps)), expression(alpha),'1')) +
  theme_classic() +
  annotate("text", x = sig_areas_labels$x, y = sig_areas_labels$y, 
           label = sig_areas_labels$label, size = 16/.pt) +
  theme(axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=18),
        axis.text = element_text(size=18),
        legend.position = 'none') +
  guides(x = guide_axis_truncated(
    trunc_lower = c(eps, eps * 4/3),
    trunc_upper = c(eps*1.1, 1)
  ))+
  coord_fixed()

# plot density plot for each database (analysis type)
density_nondir <- null_results %>%
  mutate(signcon.p = eps+signcon.p*(1-eps),
         directional_test.p = eps+directional_test.p*(1-eps)) %>%
  ggplot(aes(x = signcon.p, fill = type, colour = type)) + 
  scale_fill_manual(values = pal) +
  geom_density(alpha = .3, size = 1.5) + 
  ylab('Density') +
  scale_color_manual(values = pal) +
  scale_x_log10(breaks = c(eps,.01,1),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_continuous(breaks = c(.5, 1)) +
  geom_vline(xintercept = alpha, color = 'black', size = 1.5) +
  theme_classic() +
  theme(legend.position = "above",
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text = element_text(size=18))

aggreagated_plt <-  density_nondir + scatter_plt +
  plot_layout(ncol = 1, nrow = 2, heights = c(.5,1))
ggsave(paste(plots_fld,paste0('figure4', '.svg'), sep = .Platform$file.sep),
       plot = aggreagated_plt, width = 10, height = 10)