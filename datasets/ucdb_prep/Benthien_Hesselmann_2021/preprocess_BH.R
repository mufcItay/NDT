library(dplyr)


#create function to exclude observations
#d takes a data frame 
#subj are the subject to exclude - integer values
exclude_obs <- function(d, subj){
  return(d[!(d$subj %in% subj),])}

# IQR outlier
identifyOutliers <-  function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm=TRUE, type = 5)
  H <- 1.5 * (quantile(x, .75, na.rm=TRUE, type = 5) - quantile(x, .25, na.rm=TRUE, type = 5))
  outlier <- ((x) < (qnt[1] - H)) | ((x) > qnt[2] + H) | x < .10
  outlier
}

#conditions (cond)
#visible certain (1), visible uncertain (2), invisible certain (3), invisible uncertain (4)
# certain = centrally presented primes
# uncertain = (random) peripherally presented primes

#read the data set 
data <- read.csv("CFSlocation_data.csv", header=F, sep=",", dec=".")
#column names
colnames(data) <- c("exp","subj","trial","prime","cond","pos","probe","alpha","RT","resp1","pre","RT2","resp2")

#split
data_main <- subset(data, select=c(exp, subj, trial, prime, cond, pos, probe, alpha, RT, resp1)) #without task2
data_main <- filter(data_main, exp ==1)

#main: remove training trials (first 16 of each block)
data_main$train <- ifelse ((data_main$trial>0 & data_main$trial<17) | (data_main$trial>144 & data_main$trial<161) |
                             (data_main$trial>288 & data_main$trial<305) | (data_main$trial>432 & data_main$trial<449), 1, 0)
data_main <- filter(data_main, train == 0)

#exclude subj 18, 19, 29, 30, 31 (due to slopes and discrimination performance) from further analyses
data_main <- exclude_obs(d = data_main, subj =  c(18, 19, 29, 30, 31))   # valid measures n = 26 participants
data_main$correct <- ifelse((data_main$probe<5 & data_main$resp1==1) | (data_main$probe>5 & data_main$resp1==2), 1, 0)
data_main = filter(data_main, correct == 1)
#identify anticipatory responses (RT < 100ms, none)
data_main$toofast <- ifelse(data_main$RT < 0.1, 1, 0)
data_main = filter(data_main, toofast ==0)

#data_main_cfs <- filter(data_main, cond>2)
#identify outliers (Tukey, IQR), per participant
data_main <- data_main %>%
  group_by(subj) %>%
  mutate(outlier = identifyOutliers(RT))
data_main <- data_main %>% mutate(outlier = as.numeric(outlier)) # 1 = outlier, 0 = non-outlier
#remove outliers
data_main = filter(data_main, outlier == 0)

#create variable for congruency, visibility and location un/certainty
data_main$congruency <- ifelse ((data_main$prime<5 & data_main$probe<5) | (data_main$prime>5 & data_main$probe>5), 1, 0)
data_main$visibility <- ifelse ((data_main$cond==1) | (data_main$cond==2), 1, 0)
data_main$certainty <- ifelse ((data_main$cond==1) | (data_main$cond==3), 1, 0)
study_name <- 'Benthien & Hesselmann_2021'

data <- data_main %>% 
  filter(visibility==0) %>%
  rename(idv = subj, iv = congruency, iv2 = certainty, dv = RT) %>%
  mutate(exp = study_name, dv = dv * 1000) %>% 
  dplyr::select(idv, iv, iv2, exp, dv)

# validate against Table. 1 in the paper
data %>% group_by(idv, iv2, iv) %>% summarise(m = mean(dv)) %>%
  group_by(iv2, iv) %>% summarise(m = mean(m))


write.csv(data, 'Benthien & Hesselmann_2021.csv')

