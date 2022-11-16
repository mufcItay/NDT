library(dplyr)
library(ggplot2)
library(weaknull)
library(MASS) # to access Animals data sets
library(scales) # to access break formatting functions

dir_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_path)

# transform values to get proper labels for the p-values
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

# read the result files and add a type code
confDB_AUC <- read.table("results\\Conf_DB_Results_AUC_two_sided.csv",sep=',', header=TRUE)
confDB_AUC$type <- rep('CONF_AUC', nrow(confDB_AUC))
confDB_Conf <- read.table("results\\Conf_DB_Results_Confidence_two_sided.csv",sep=',', header=TRUE)
confDB_Conf$type <- rep('CONF_Confidence', nrow(confDB_Conf))
# for the priming db add type later
primingDB_SignCon <- read.table("results\\Priming_DB_Results_Median_two_sided.csv",sep=',', header=TRUE)
primingDB_Classification <- read.table("results\\Priming_DB_Results_Median__classification.csv",sep=',', header=TRUE)

# we filter out the datasets about UC, VS and assign specific 'type' code
uc_exps <- c('A','V','AV','E1','E2','E3','MS_1_cong', 'MS_1_incong', 'MS_2_cong', 'MS_2_incong',
             'MS_3_cong', 'MS_3_incong', 'MS_4_cong', 'MS_4_incong')
exps <- unique(primingDB_SignCon$exp)
vs_exps <- c(grep("Variable", exps, value = TRUE), grep("Constant", exps, value = TRUE))
primingDB_SignCon$type <- ifelse(primingDB_SignCon$exp %in% vs_exps, 'VS', ifelse(primingDB_SignCon$exp %in% uc_exps, 'UC', 'Cognition'))
primingDB_SignCon$type <- paste0('SC_',primingDB_SignCon$type)

primingDB_Classification$type <- ifelse(primingDB_Classification$exp %in% vs_exps, 'VS', ifelse(primingDB_Classification$exp %in% uc_exps, 'UC', 'Cognition'))
primingDB_Classification$type <- paste0('CLASS_',primingDB_Classification$type)
primingDB_Classification$directional_effect.p <- primingDB_SignCon$directional_effect.p
primingDB_Classification$directional_effect.statistic <- primingDB_SignCon$directional_effect.statistic

# aggregate datasets
all_res <- rbind(confDB_AUC,confDB_Conf,primingDB_SignCon,primingDB_Classification)
all_res$type <- factor(all_res$type)
# sorted by levels of the type factor:
# [1] "CLASS_Cognition" "CLASS_UC"        "CLASS_VS"        "CONF_AUC"        "CONF_Confidence" "SC_Cognition"   
# [7] "SC_UC"           "SC_VS"
classification_types <- grep("CLASS", unique(all_res$type), value = TRUE)
all_res$is_classification <- all_res$type %in% classification_types
all_res$is_uc <- all_res$exp %in% uc_exps
all_res <- all_res %>% rename(non_directional.p = sign_con.p)
pal <- c('lightblue1', 'firebrick1', 'dodgerblue3',
         'palegreen4', 'palegreen1',
         'lightblue1','firebrick1', 'dodgerblue3')
all_res %>%
  dplyr::select(type, exp,non_directional.p,directional_effect.p) %>%
  mutate(non_directional.p = 0.001+non_directional.p*0.999,
         directional_effect.p = 0.001+directional_effect.p*0.999) %>%
  ggplot(aes(x=directional_effect.p,y=non_directional.p)) +
  geom_point(size = ifelse(all_res$is_uc, ifelse(all_res$exp == 'E1',12,4),ifelse(all_res$exp == 'data_Maniscalco_2017_expt1.csv',5,2)), 
             mapping = aes(color = type, shape = all_res$is_classification)) +
  scale_shape_manual(values=c(16, 15))+
  scale_colour_manual(values = pal) +
  annotate('rect',xmin= 0, xmax=0.05, ymin=0,ymax=Inf,fill='orange',alpha=0.2) +
  annotate('rect',ymin=0, ymax=0.05, xmin=0,xmax=Inf,fill='purple',alpha=0.2) +
  labs(x='Directional p-value (log10)',
       y='Non-Directional p-value (log10)',
       title='All Effects') +
  scale_x_log10(breaks = c(.001,.01,1),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = c(.001,.01,1),
                labels = trans_format("log10", math_format(10^.x))) +
  coord_fixed() +
  theme_classic() +
  theme(plot.title = element_text(size=24, hjust = 0.5),
        axis.title = element_text(size=18))
# ggsave('C:\\U\\LiadLab\\Conferences\\ASSC\\2022\\Revisting Null Results\\Poster\\Plots\\ALL_EFFECTS.svg')
# ggsave('C:\\U\\LiadLab\\Conferences\\ASSC\\2022\\Revisting Null Results\\Poster\\Plots\\ALL_EFFECTS.png')
