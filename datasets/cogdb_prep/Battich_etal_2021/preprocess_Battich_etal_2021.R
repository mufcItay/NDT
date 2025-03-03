library(dplyr)

# read the dataset
data <- read.csv('jaSifiData.csv')
subjs <- unique(data$subject)
data <- data %>%
  rename(idv = subject) %>%
  mutate(idv = as.numeric(factor(idv)))

# exclude low performing participants (as was done in Battich et al. 2021)
min_perf <- 1 - .35
exc_participants <- data %>%
  filter(beeps == target) %>%
  group_by(idv,target) %>%
  summarise(perf = mean(correct)) %>%
  filter(perf <= min_perf) %>%
  dplyr::pull(idv)

# RT outliers (trial level) + low preforming ps (subj level)
data <- data %>%
  filter(rt >= .1, rt <= 3, !idv %in% exc_participants, soc_condition != 'C') %>%
  group_by(idv) %>%
  mutate(first = first(soc_condition), half = 2 - (soc_condition == first))

# get response data, preregistered analysis
data_response <- data %>%
  filter(soc_condition != 'C') %>%
  rename(dv =  response, iv2 = beeps, iv = soc_condition) %>%
  mutate(exp = paste('Battich_etal_2021', 'resp', target, sep = '_')) %>%
  dplyr::select(idv,iv,iv2,dv,exp, order, half)

# get RT data, exploratory analysis
data_rt <- data %>%
  filter(soc_condition != 'C') %>%
  rename(dv =  rt, iv2 = beeps, iv = soc_condition) %>%
  mutate(exp = paste('Battich_etal_2021', 'rt',target, sep = '_')) %>%
  dplyr::select(idv,iv,iv2,dv,exp,order, half)

# bind RT and response data together
data_all <- rbind(data_rt, data_response)
write.csv(data_all, 'Battich_etal_2021.csv')
