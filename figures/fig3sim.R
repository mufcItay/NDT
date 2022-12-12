library(ggplot2)
library(dplyr)
library(scales) # to access break formatting functions

# transform values to get proper labels for the p-values
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}
# read the results of all tests
results_folder <- 'results'
results_fns <- list.files(results_folder, pattern = '.csv', full.names = TRUE) 
read_res_df <- function(fn) {
  df <- read.csv(fn) %>%
    mutate(type = gsub(basename(fn), pattern="_Results.csv$", replacement=""))
}
results <- do.call(rbind, lapply(results_fns, read_res_df))

# generate the scatter plot of all results together 
eps <- 10^-5
alpha_rects <- .2
results %>%
  dplyr::select(type, exp,non_directional.p,directional_effect.p) %>%
  mutate(non_directional.p = eps+non_directional.p*(1-eps),
         directional_effect.p = eps+directional_effect.p*(1-eps)) %>%
  ggplot(aes(x=directional_effect.p,y=non_directional.p)) +
  geom_point(size = 4, mapping = aes(color = type)) +
  # scale_colour_manual(values = pal) +
  annotate('rect',xmin= 0, xmax=0.05, ymin=0,ymax=Inf,fill='orange',alpha=alpha_rects) +
  annotate('rect',ymin=0, ymax=0.05, xmin=0,xmax=Inf,fill='purple',alpha=alpha_rects) +
  labs(x='Directional p-value (log10)',
       y='Non-Directional p-value (log10)',
       title='All Effects') +
  scale_x_log10(breaks = c(eps,.01,1),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = c(eps,.01,1),
                labels = trans_format("log10", math_format(10^.x))) +
  coord_fixed() +
  theme_classic() +
  theme(plot.title = element_text(size=24, hjust = 0.5),
        axis.title = element_text(size=18))
