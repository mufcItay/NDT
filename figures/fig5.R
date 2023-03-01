library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)
library(tidyr)

library(scales) # to access break formatting functions
source('datasets_analysis//definitions.R')

alpha <- .05
pal <-   c("#B25E9D", "#3ECFC9", "#F4B26A", "#EB5F4A")

# transform values to get proper labels for the p-values
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format(digits = 2)(x)))
}

# read the results of all tests
results_folder <- 'results'
results_fns <- list.files(results_folder, pattern = '.csv', full.names = TRUE) 
read_res_df <- function(fn) {
  df <- read.csv(fn) %>%
    mutate(type = gsub(basename(fn), pattern="_Results.csv$", replacement="")) %>%
    group_by(type,exp) %>%
    summarise(non_directional.p = first(non_directional.p),
              directional_effect.p = first(directional_effect.p)) %>%
    dplyr::select(type, exp,non_directional.p,directional_effect.p)
}
# get a summary of the number of studies per data type
all_results <- do.call(rbind, lapply(results_fns, read_res_df))
sum_N <- all_results %>%
  group_by(type) %>%
  summarise(N = n())
Nunique <- length(unique(unique(paste(all_results$exp, all_results$type, sep = '_'))))
# summarise non-significant results
ns_sum_Ns <- ns_results %>%
  group_by(type) %>%
  summarise(N = n())
Nunique_ns <- length(unique(paste(ns_results$exp,ns_results$type, sep = '_')))
# uncorrected across all types
sig_nondir_cat <- ns_results %>%
  mutate(non_dir_effect = non_directional.p <= alpha) %>% 
  group_by(non_dir_effect) %>%
  summarise(N = n()) %>%
  summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N)))

# 
# uncorrected - significant sign-consistency per category
sig_nondir_cat <- ns_results %>%
  mutate(non_dir_effect = non_directional.p <= alpha) %>%
  group_by(type, non_dir_effect) %>%
  summarise(N = n()) %>%
  group_by(type) %>%
  summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N)))
# FDR corrected - significant sign-consistency per category
# analyze the remaining non-significant results
ns_results <- all_results[all_results$directional_effect.p > alpha,] %>%
  group_by(type) %>%
  mutate(non_directional.p.corrected = p.adjust(non_directional.p,method = 'fdr'))
corrected_sig_nondir_cat <- ns_results %>%
  mutate(non_dir_effect = non_directional.p.corrected <= alpha) %>%
  group_by(type, non_dir_effect) %>%
  summarise(N = n()) %>%
  group_by(type) %>%
  summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N)))

non_directional.p.corrected
sig_nondir_all <- ns_results %>%
  mutate(non_dir_effect = non_directional.p <= alpha) %>%
  group_by(non_dir_effect) %>%
  summarise(N = n()) %>%
  summarise(perc_nondir_sig = 100 * (1 - first(N) / sum(N)))

# within Cognitive Psychology
cog_psy_analysis <- ns_results %>%
  filter(startsWith(type, 'Cognitive')) %>%
  mutate(non_dir_effect = non_directional.p <= alpha, sub_cat = 
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
ns_n <- length(unique(paste(ns_results$type,ns_results$exp)))


##### GENERATE FIGURE
# generate the scatter plot of all results together 
eps <- 10^-5
sig_areas_x_marg <- .1
sig_areas_y_marg <- .01
sig_areas_labels <- data.frame( label = paste0(c("p < ", "p > "), alpha),
  x = alpha + c(-sig_areas_x_marg/2.5, sig_areas_x_marg),
  y = alpha - rep(sig_areas_y_marg, 2))
scatter_plt <- ns_results %>%
  mutate(non_directional.p = eps + non_directional.p*(1-eps),
         directional_effect.p = eps + directional_effect.p*(1-eps)) %>%
  ggplot(aes(x=non_directional.p,y=directional_effect.p, fill = type)) +
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
  coord_fixed() +
  theme_classic() +
  annotate("text", x = sig_areas_labels$x, y = sig_areas_labels$y, 
           label = sig_areas_labels$label, size = 16/.pt) +
  theme(axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=18),
        axis.text = element_text(size=18),
        legend.position = 'none')

density_nondir <- ns_results %>%
  mutate(non_directional.p = eps+non_directional.p*(1-eps),
         directional_effect.p = eps+directional_effect.p*(1-eps)) %>%
  ggplot(aes(x = non_directional.p, fill = type, colour = type)) + 
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
ggsave(paste('figures',paste0('figure4', '.svg'), sep = .Platform$file.sep),
       plot = aggreagated_plt, width = 10, height = 10)
