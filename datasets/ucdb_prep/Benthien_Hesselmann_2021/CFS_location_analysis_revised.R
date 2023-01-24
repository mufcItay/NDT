#CFS - Location Uncertainty Project, 2019
# revised 10/2020

#install.packages("dplyr")
#install.packages("MBESS")
#install.packages("effects")
#install.packages("afex")
#install.packages("sjPlot")
#install.packages("sjmisc")
#install.packages("Rmisc")
#install.packages("tidyverse")

library(dplyr)
library(MBESS)
library(effects)
library(afex)
library(sjPlot)
library(sjmisc)
library(Rmisc)
library(tidyverse) 
library(ggplot2)
library(psych)
############################################################################

#clean workspace
rm(list=ls())

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

#set the working directory
setwd('...')

############################################################################

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
data_check <- filter(data, exp ==2)

#main: remove training trials (first 16 of each block)
data_main$train <- ifelse ((data_main$trial>0 & data_main$trial<17) | (data_main$trial>144 & data_main$trial<161) |
                          (data_main$trial>288 & data_main$trial<305) | (data_main$trial>432 & data_main$trial<449), 1, 0)

data_main <- filter(data_main, train == 0)

#split further for plotting "awareness-check"
data_check_cfs <- filter(data_check, cond>2)
data_check_nocfs <- filter(data_check, cond<3)

  #visibility cfs
  ggplot(data_check_cfs, aes(x=as.factor(resp2), y=..count../max(..count..), group=subj, color=as.factor(subj))) + 
    geom_point(stat='count') +
    geom_line(stat='count') + 
    labs(x='PAS levels',y='relative frequency',title='visibility under CFS') + theme_bw() +
         theme(legend.position = "none")

  #visibility no-cfs
  ggplot(data_check_nocfs, aes(x=as.factor(resp2), y=..count../max(..count..), group=subj, color=as.factor(subj))) + 
    geom_point(stat='count') +
    geom_line(stat='count') + 
    labs(x='PAS levels',y='relative frequency',title='visibility without CFS') + theme_bw() +
         theme(legend.position = "none")

#calculate relative frequency of PAS level per subject with / without CFS
PAS_CFS = dplyr::summarise(group_by(data_check_cfs, 
                                    subj,resp2), 
                           freq = length(resp2)/128)
  
PAS_noCFS = dplyr::summarise(group_by(data_check_nocfs, 
                                      subj,resp2), 
                             freq = length(resp2)/128)    
  
#regression slopes for each subject in CFS and noCFS conditions
library(broom)
  
PAS_CFS <- group_by(PAS_CFS, subj)   # defines grouping variable with package broom
do(PAS_CFS, tidy( lm(freq ~ resp2, data = .))) %>%
  print(n=Inf, width = Inf)
  
PAS_noCFS <- group_by(PAS_noCFS, subj)
do(PAS_noCFS, tidy( lm(freq ~ resp2, data = .))) %>%
  print(n=Inf, width = Inf)
  
##### subj. 18: positive slope in CFS condition
##### subj. 19 + 29: negative slope in noCFS condition

#correct responses in check experiment (across 4 conditions)
data_check$correct <- ifelse((data_check$prime<5 & data_check$resp1==1) |
                              (data_check$prime>5 & data_check$resp1==2), 1, 0)

data_check_correct <- dplyr::summarise(group_by(data_check,
                                                subj,cond),
                                       correct = sum(correct)/length(correct))

##### subj. 30: only 50% correct responses in all conditions (resp2 = 2 in all trials)
##### subj. 31: only 50% correct responses in visible certain condition (resp2 = 1 until trial 149)

#exclude subj 18, 19, 29, 30, 31 (due to slopes and discrimination performance) from further analyses
data_main <- exclude_obs(d = data_main, subj =  c(18, 19, 29, 30, 31))   # valid measures n = 26 participants
data_check <- exclude_obs(d = data_check, subj = c(18, 19, 29, 30, 31))

#calculate median PAS rating (resp2) per subj, then mean across all subj
median <- data_check %>% group_by(subj, cond) %>%
        summarise_at(vars(resp2), funs(median(., na.rm=TRUE)))  

describeBy(median$resp2, median$cond)# means in 4 conditions
  median_nocfs <- filter(median, cond<3)
  describe(median_nocfs$resp2)
  median_cfs <- filter(median, cond>2)
  describe(median_cfs$resp2)

##### Plotting PAS ratings per condition
# CFS conditions    
data_PAS_cfs <- dplyr::summarise(group_by(median_cfs,
                                          subj, cond),
                                 PAS = mean(resp2))
mystats_cfs = summarySE(data_PAS_cfs, measurevar = 'PAS', groupvars='cond')
  
p = ggplot() +
    
geom_bar(data=mystats_cfs, aes(x=cond, y=PAS, fill = as.factor(cond)), position=position_dodge(),
         stat="identity", width = 0.6, color="black") +
    
geom_errorbar(data=mystats_cfs, aes(x=cond, y=PAS,ymin=PAS-se, ymax=PAS+se),
              width=.1,
              position=position_dodge(.1)) +
    
geom_point(data=data_PAS_cfs,aes(x=cond-0.2,y=PAS),
            stat="identity",
            position=position_jitter(w = 0.1, h = 0),
            alpha=.6,
            size=1)
  
p = p + scale_x_continuous(breaks=seq(2,5,1)) +scale_y_continuous(breaks=seq(0,4,0.5)) + ylim(0, 4) +
    theme_bw() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none")
  
p 
  
# visible conditions    
data_PAS_nocfs <- dplyr::summarise(group_by(median_nocfs,
                                           subj, cond),
                                    PAS = mean(resp2))
mystats_nocfs = summarySE(data_PAS_nocfs, measurevar = 'PAS', groupvars='cond')
  
p = ggplot() +
    
geom_bar(data=mystats_nocfs, aes(x=cond, y=PAS, fill = as.factor(cond)), position=position_dodge(),
         stat="identity", width = 0.6, color="black") +
    
geom_errorbar(data=mystats_nocfs, aes(x=cond, y=PAS,ymin=PAS-se, ymax=PAS+se),
              width=.1,
              position=position_dodge(.1)) +
    
geom_point(data=data_PAS_nocfs,aes(x=cond-0.2,y=PAS),
            stat="identity",
            position=position_jitter(w = 0.1, h = 0),
            alpha=.6,
            size=1)
  
p = p + scale_x_continuous(breaks=seq(0,3,1)) +scale_y_continuous(breaks=seq(0,4,0.5)) +
    theme_bw() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none")

p

###
#split check into visible and invisible conditions after excluding subjects
data_check_cfs <- filter(data_check, cond>2)
data_check_nocfs <- filter(data_check, cond<3)

#correct responses in check invisible conditions:
data_check_cfs$correct <- ifelse((data_check_cfs$prime<5 & data_check_cfs$resp1==1) |
                                  (data_check_cfs$prime>5 & data_check_cfs$resp1==2), 1, 0)

data_check_cfs_correct <- dplyr::summarise(group_by(data_check_cfs,
                                                    subj,cond),
                                       correct = sum(correct)/length(correct))
mystats_cfs = summarySE(data_check_cfs_correct, measurevar = 'correct')
CI(data_check_cfs_correct$correct, ci = 0.95)

#correct responses for PAS=1 trials (rated "completely invisible"):
data_check_correct_1 <- filter(data_check_cfs, resp2 == 1)
data_check_correct_1 <- dplyr::summarise(group_by(data_check_correct_1,
                                                  subj,cond),
                                       correct = sum(correct)/length(correct))

mystats = summarySE(data_check_correct_1, measurevar = 'correct')
CI(data_check_correct_1$correct, ci = 0.95)
  
    #correct responses in invisible certain
    correct_invis_certain <- filter(data_check_cfs_correct, cond==3)
    CI(correct_invis_certain$correct, ci = 0.95)

    #correct responses in invisibe uncertain
    correct_invis_uncertain <- filter(data_check_cfs_correct, cond==4)
    CI(correct_invis_uncertain$correct, ci = 0.95)

#correct responses in check visible conditions:
data_check_nocfs$correct <- ifelse((data_check_nocfs$prime<5 & data_check_nocfs$resp1==1) |
                                    (data_check_nocfs$prime>5 & data_check_nocfs$resp1==2), 1, 0)

data_check_nocfs_correct <- dplyr::summarise(group_by(data_check_nocfs,
                                                subj,cond),
                                       correct = sum(correct)/length(correct))
mystats_nocfs = summarySE(data_check_nocfs_correct, measurevar = 'correct')
CI(data_check_nocfs_correct$correct, ci = 0.95)

    #correct responses in visible certain
    correct_vis_certain <- filter(data_check_nocfs_correct, cond==1)
    CI(correct_vis_certain$correct, ci = 0.95)

    #correct responses in visibe uncertain
    correct_vis_uncertain <- filter(data_check_nocfs_correct, cond==2)
    CI(correct_vis_uncertain$correct, ci = 0.95)

##### Plotting the discrimination performance
# CFS conditions    
mystats_cfs = summarySE(data_check_cfs_correct, measurevar = 'correct', groupvars='cond')

p = ggplot() +
      
geom_bar(data=mystats_cfs, aes(x=cond, y=correct, fill = as.factor(cond)), position=position_dodge(),
         stat="identity", width = 0.6, color="black") +
      
geom_errorbar(data=mystats_cfs, aes(x=cond, y=correct,ymin=correct-se, ymax=correct+se),
              width=.1,
              position=position_dodge(.1)) +
      
geom_point(data=data_check_cfs_correct,aes(x=cond-0.2,y=correct),
             stat="identity",
             position=position_jitter(w = 0.05, h = 0),
             alpha=.6,
             size=1)
    
p = p + scale_x_continuous(breaks=seq(2,5,1)) +scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme_bw() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none")

p 

# visible conditions    
mystats_nocfs = summarySE(data_check_nocfs_correct, measurevar = 'correct', groupvars='cond')

p = ggplot() +
  
  geom_bar(data=mystats_nocfs, aes(x=cond, y=correct, fill = as.factor(cond)), position=position_dodge(),
           stat="identity", width = 0.6, color="black") +
  
  geom_errorbar(data=mystats_nocfs, aes(x=cond, y=correct,ymin=correct-se, ymax=correct+se),
                width=.1,
                position=position_dodge(.1)) +
  
  geom_point(data=data_check_nocfs_correct,aes(x=cond-0.2,y=correct),
             stat="identity",
             position=position_jitter(w = 0.05, h = 0),
             alpha=.6,
             size=1)

p = p + scale_x_continuous(breaks=seq(0,3,1)) +scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme_bw() + scale_fill_brewer(palette="Set2")+ theme(legend.position = "none")

p

###
#correct responses in main experiment (across 4 conditions)
data_main$correct <- ifelse((data_main$probe<5 & data_main$resp1==1) | (data_main$probe>5 & data_main$resp1==2), 1, 0)

data_main_correct <- dplyr::summarise(group_by(data_main, 
                                   subj,cond), 
                          correct = sum(correct)/length(correct))
mystats = summarySE(data_main_correct, measurevar = 'correct')
CI(data_main_correct$correct, ci = 0.95)

data_main_cfs <- filter(data_main, cond>2)
data_main_nocfs <- filter(data_main, cond<3)

#correct repsonses in invisible trials
data_main_cfs_correct <- dplyr::summarise(group_by(data_main_cfs, 
                                           subj,cond), 
                                  correct = sum(correct)/length(correct))
mystats_cfs = summarySE(data_main_cfs_correct, measurevar = 'correct')
CI(data_main_cfs_correct$correct, ci = 0.95)

    #correct responses in invisible certain
    correct_invis_certain <- filter(data_main_cfs_correct, cond==3)
    CI(correct_invis_certain$correct, ci = 0.95)

    #correct responses in invisibe uncertain
    correct_invis_uncertain <- filter(data_main_cfs_correct, cond==4)
    CI(correct_invis_uncertain$correct, ci = 0.95)

#correct responses in noCFS trials:
data_main_nocfs_correct <- dplyr::summarise(group_by(data_main_nocfs, 
                                                     subj,cond), 
                                             correct = sum(correct)/length(correct))
mystats_nocfs = summarySE(data_main_nocfs_correct, measurevar = 'correct')
CI(data_main_nocfs_correct$correct, ci = 0.95)
    
    #correct responses in visible certain
    correct_vis_certain <- filter(data_main_nocfs_correct, cond==1)
    CI(correct_vis_certain$correct, ci = 0.95)
    
    #correct responses in visibe uncertain
    correct_vis_uncertain <- filter(data_main_nocfs_correct, cond==2)
    CI(correct_vis_uncertain$correct, ci = 0.95)
  
#exclude incorrect trials
data_main = filter(data_main, correct == 1)

#identify anticipatory responses (RT < 100ms, none)
data_main$toofast <- ifelse(data_main$RT < 0.1, 1, 0)
data_main = filter(data_main, toofast ==0)
      
#identify outliers (Tukey, IQR), per participant
data_main <- data_main %>%
      group_by(subj) %>%
      mutate(outlier = identifyOutliers(RT))
    
data_main <- data_main %>% mutate(outlier = as.numeric(outlier)) # 1 = outlier, 0 = non-outlier
    
data_outlier <- dplyr::summarise(group_by(data_main, 
                                              subj), 
                                     outlier = sum(outlier)/length(outlier))
mean(data_outlier$outlier)
range(data_outlier$outlier) 

#remove outliers
data_main = filter(data_main, outlier == 0)
  
####################################
###### RT analysis

#create variable for congruency, visibility and location un/certainty
data_main$congruency <- ifelse ((data_main$prime<5 & data_main$probe<5) | (data_main$prime>5 & data_main$probe>5), 1, 0)
data_main$visibility <- ifelse ((data_main$cond==1) | (data_main$cond==2), 1, 0)
data_main$certainty <- ifelse ((data_main$cond==1) | (data_main$cond==3), 1, 0)

#mean RT per condition
    # Function to calculate the mean, standard deviation and CI for each group
    # data : a data frame
    # varname : the name of a column containing the variable to be summarized
    # groupnames : vector of column names to be used as grouping variables
  
  data_summary <- function(data, varname, groupnames){
    require(plyr)
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        sd = sd(x[[col]], na.rm=TRUE),
        CI = CI(x[[col]]))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }

#mean RT separately for all conditions (REVISED)
means <- data_summary(data_main, varname="RT", 
                      groupnames=c("visibility", "certainty", "congruency", "subj"))

means = data_summary(means, varname = "RT",groupnames =c("visibility", "certainty", "congruency" )) 

# PLOT RT means  
#split for plotting
means$congruency <- factor(means$congruency, levels <- c(1,0), labels <- c( "congruent", "incongruent"))
means$visibility <- factor(means$visibility, levels <- c(1,0), labels <- c( "visible", "invisible"))
means$certainty <- factor(means$certainty, levels <- c(1,0), labels <- c( "certain", "uncertain"))

means_cfs <- filter(means, visibility=="invisible")
means_nocfs <- filter (means, visibility=="visible")

#RT invisible
p <- ggplot(means_cfs, aes(x=certainty, y=RT, fill=congruency)) +
  geom_bar(stat="identity", position=position_dodge(), width = .9, color="black") +
  geom_errorbar(aes(x=certainty, ymin=RT-sd/sqrt(26), ymax=RT+sd/sqrt(26)), width=.1,
                position=position_dodge(.9))

p + labs(title="CFS", x="Condition", y = "RT (ms)") +
  scale_y_continuous(breaks=seq(0,1,0.025)) +
  scale_fill_brewer(palette="Set2") +
  theme_bw(base_size = 18) +
  theme(legend.position="none") +  coord_cartesian(ylim=c(0.425,0.525))  # export 350 450

#RT visible
p <- ggplot(means_nocfs, aes(x=certainty, y=RT, fill=congruency)) +
  geom_bar(stat="identity", position=position_dodge(), width = .9, color="black") +
  geom_errorbar(aes(x=certainty, ymin=RT-sd/sqrt(26), ymax=RT+sd/sqrt(26)), width=.1,
                position=position_dodge(.9))

p + labs(title="Visible", x="Condition", y = "RT (ms)")+ 
  scale_y_continuous(breaks=seq(0,1,0.025)) +
  scale_fill_brewer(palette="Set2") +
  theme_bw(base_size=18) +
  theme(legend.position="none") +  coord_cartesian(ylim=c(0.425,0.525)) # export 350 450

####    
# VISIBLE CONDITION (ie, cond < 3)
# INVISIBLE CONDITION (ie, cond > 2)
# calculate mean differences for main effects and interaction
    
#Calculate main effect "congruency" and export to *csv in visible condition
data.vis = filter(data_main, cond < 3)
    
data.RT = dplyr::summarise(group_by(data.vis, 
                                  subj, congruency), 
                           RT = mean(RT))

data.RT$diff = data.RT$RT[data.RT$congruency==0]-data.RT$RT[data.RT$congruency==1] # incongruent - congruent
data.RT = data.RT[1:length(unique(data.RT$subj)),]
          
      write.csv(data.RT$diff, "main_vis_congruency.csv")

  #Calculate main effect "congruency" for visible "location certain" trials
  data.vis.cert = filter(data.vis, cond == 1)
  data.RT = dplyr::summarise(group_by(data.vis.cert,
                                      subj, congruency),
                             RT = mean(RT))
  data.RT$diff = data.RT$RT[data.RT$congruency==0]-data.RT$RT[data.RT$congruency==1] # incongruent - congruent
  data.RT = data.RT[1:length(unique(data.RT$subj)),]
  
        write.csv(data.RT$diff, "main_vis_congruency_cert.csv")
  
  #Calculate main effect "congruency" for visible "location uncertain" trials
  data.vis.uncert = filter(data.vis, cond == 2)
  data.RT = dplyr::summarise(group_by(data.vis.uncert,
                                      subj, congruency),
                             RT = mean(RT))
  data.RT$diff = data.RT$RT[data.RT$congruency==0]-data.RT$RT[data.RT$congruency==1] # incongruent - congruent
  data.RT = data.RT[1:length(unique(data.RT$subj)),]
        
        write.csv(data.RT$diff, "main_vis_congruency_uncert.csv")      
               
#Calculate main effect "congruency" and export to *csv in invisible condition
data.invis = filter(data_main, cond > 2)
     
data.RT = dplyr::summarise(group_by(data.invis,
                                  subj, congruency), 
                           RT = mean(RT))
     
data.RT$diff = data.RT$RT[data.RT$congruency==0]-data.RT$RT[data.RT$congruency==1] # incongruent - congruent
data.RT = data.RT[1:length(unique(data.RT$subj)),]
     
          write.csv(data.RT$diff, "main_invis_congruency.csv")
    
    #Calculate main effect "congruency" for invisible location certain trials
    data.invis.cert = filter(data.invis, cond == 3)
    data.RT = dplyr::summarise(group_by(data.invis.cert,
                                              subj, congruency),
                                     RT = mean(RT))
    data.RT$diff = data.RT$RT[data.RT$congruency==0]-data.RT$RT[data.RT$congruency==1] # incongruent - congruent
    data.RT = data.RT[1:length(unique(data.RT$subj)),]
          
        write.csv(data.RT$diff, "main_invis_congruency_cert.csv")
                
### Calculate interaction "congruency x uncertainty" and export to *csv in visible condition
data.RT = dplyr::summarise(group_by(data.vis, 
                                  subj, cond, congruency), 
                          RT = mean(RT))
    
data.RT$diff = (data.RT$RT[data.RT$congruency==0 & data.RT$cond==2]-data.RT$RT[data.RT$congruency==1 & data.RT$cond==2]) - # [incongruent - congruent]uncertain MINUS 
              (data.RT$RT[data.RT$congruency==0 & data.RT$cond==1]-data.RT$RT[data.RT$congruency==1 & data.RT$cond==1])   # [incongruent - congruent]certain
    
data.RT = data.RT[1:length(unique(data.RT$subj)),]
    
          write.csv(data.RT$diff, "interaction_vis.csv")    

### Calculate interaction "congruency x uncertainty" and export to *csv in invisible condition
data.RT = dplyr::summarise(group_by(data.invis, 
                                  subj, cond, congruency), 
                          RT = mean(RT))
    
data.RT$diff = (data.RT$RT[data.RT$congruency==0 & data.RT$cond==4]-data.RT$RT[data.RT$congruency==1 & data.RT$cond==4]) - # [incongruent - congruent]uncertain MINUS 
               (data.RT$RT[data.RT$congruency==0 & data.RT$cond==3]-data.RT$RT[data.RT$congruency==1 & data.RT$cond==3])   # [incongruent - congruent]certain
    
data.RT = data.RT[1:length(unique(data.RT$subj)),]
    
          write.csv(data.RT$diff, "interaction_invis.csv")    

          
#### exploratory analysis "conflict hypothesis"

#create "conflict" variable
data_main$conflict <- ifelse ((data_main$probe>data_main$prime & data_main$probe<5) |
                                       (data_main$probe<data_main$prime & data_main$probe>5), 1, 0)

#calculate effect of "conflict" visible
data_main_vis = filter(data_main, cond < 3)

data.RT = dplyr::summarise(group_by(data_main_vis, 
                                      subj, conflict), 
                             RT = mean(RT))
 
    #RTs in conflict and no-conflict trials
    data.RT.conflict <- filter(data.RT, conflict==1)
    data.RT.non <- filter(data.RT, conflict==0)
    round(CI(data.RT.conflict$RT),4)
    round(CI(data.RT.non$RT),4)

data.RT$diff = data.RT$RT[data.RT$conflict==1]-data.RT$RT[data.RT$conflict==0] # conflict -  no-conflict (visible)
data.RT = data.RT[1:length(unique(data.RT$subj)),]

    write.csv(data.RT$diff, "conflict_vis.csv")

#conflict for large/extreme distance prime-target pairs (1-4, 9-6) in visible
data_main$ex.dis <- ifelse((data_main$prime==1 & data_main$probe==4) | (data_main$prime==9 & data_main$probe==6),1, 0)

data_main_vis <- filter(data_main, cond < 3)

data.RT = dplyr::summarise(group_by(data_main_vis, 
                                    subj, ex.dis), 
                           RT = mean(RT))

data.RT.ex.dis <- filter(data.RT, ex.dis==1)
data.RT.non <- filter(data.RT, ex.dis==0)
round(CI(data.RT.ex.dis$RT),4)
round(CI(data.RT.non$RT),4)

data.RT$diff = data.RT$RT[data.RT$ex.dis==1]-data.RT$RT[data.RT$ex.dis==0] # largest distance - "rest" (visible)
data.RT = data.RT[1:length(unique(data.RT$subj)),]
    
    write.csv(data.RT$diff, "ex_dis_conflict_vis.csv")

#conflict for large/extreme distance prime-target pairs in invisible
data_main_invis <- filter(data_main, cond > 2)

data.RT = dplyr::summarise(group_by(data_main_invis, 
                                    subj, ex.dis), 
                           RT = mean(RT))

data.RT.ex.dis <- filter(data.RT, ex.dis==1)
data.RT.non <- filter(data.RT, ex.dis==0)
round(CI(data.RT.ex.dis$RT),4)
round(CI(data.RT.non$RT),4)

data.RT$diff = data.RT$RT[data.RT$ex.dis==1]-data.RT$RT[data.RT$ex.dis==0] # largest distance pairs - "rest" (invisible)
data.RT = data.RT[1:length(unique(data.RT$subj)),]
    
      write.csv(data.RT$diff, "ex_dis_conflict_invis.csv")

#priming in visible conditions without largest distance pairs
data_main_vis <- filter (data_main_vis, ex.dis == 0)

data.RT = dplyr::summarise(group_by(data_main_vis, 
                                    subj, congruency), 
                           RT = mean(RT))

data.RT$diff = data.RT$RT[data.RT$congruency==0]-data.RT$RT[data.RT$congruency==1] # incongruent - congruent
data.RT = data.RT[1:length(unique(data.RT$subj)),]
    
    write.csv(data.RT$diff, "vis_congruency_ohne_ex_dis.csv")

#priming in invisible conditions without largest distance pairs
data_main_invis <- filter (data_main_invis, ex.dis == 0)
    
data.RT = dplyr::summarise(group_by(data_main_invis, 
                                    subj, congruency), 
                            RT = mean(RT))
    
data.RT$diff = data.RT$RT[data.RT$congruency==0]-data.RT$RT[data.RT$congruency==1] # incongruent - congruent
data.RT = data.RT[1:length(unique(data.RT$subj)),]
    
    write.csv(data.RT$diff, "invis_congruency_ohne_ex_dis.csv")

#interaction effect invisible without largest distance pairs
data.RT = dplyr::summarise(group_by(data_main_invis, 
                                    subj, cond, congruency), 
                            RT = mean(RT))
    
data.RT$diff = (data.RT$RT[data.RT$congruency==0 & data.RT$cond==4]-data.RT$RT[data.RT$congruency==1 & data.RT$cond==4]) - # [incongruent - congruent]uncertain MINUS 
(data.RT$RT[data.RT$congruency==0 & data.RT$cond==3]-data.RT$RT[data.RT$congruency==1 & data.RT$cond==3])   # [incongruent - congruent]certain
    
data.RT = data.RT[1:length(unique(data.RT$subj)),]
    
    write.csv(data.RT$diff, "NEW_interaction_invis.csv")

    
######## EXPLORATORY ANALYSIS (REVISION)    
    
    # separately for visible and invisible trials (i.e., no-CFS and CFS trials)
    
    data_main =  data_main %>%
      mutate(
        conflict = case_when(
          prime == 1 & probe == 2 ~ "1",
          prime == 1 & probe == 4 ~ "2",
          prime == 1 & probe == 6 ~ "0",
          prime == 1 & probe == 8 ~ "0",
          prime == 3 & probe == 2 ~ "0",
          prime == 3 & probe == 4 ~ "1",
          prime == 3 & probe == 6 ~ "0",
          prime == 3 & probe == 8 ~ "0",
          prime == 7 & probe == 2 ~ "0",
          prime == 7 & probe == 4 ~ "0",
          prime == 7 & probe == 6 ~ "1",
          prime == 7 & probe == 8 ~ "0",
          prime == 9 & probe == 2 ~ "0",
          prime == 9 & probe == 4 ~ "0",
          prime == 9 & probe == 6 ~ "2",
          prime == 9 & probe == 8 ~ "1",
          TRUE                      ~ "-999" # dummy
        )
      )     
    
    data_main_cfs <- filter(data_main, cond>2)
    data_main_nocfs <- filter(data_main, cond<3)
    
    # convert relevant variables to factors
    
    data_main_cfs$subj = as.factor(data_main_cfs$subj)
    data_main_cfs$probe = as.factor(data_main_cfs$probe)
    data_main_cfs$congruency = as.factor(data_main_cfs$congruency)
    data_main_cfs$conflict = as.factor(data_main_cfs$conflict)
    
    data_main_nocfs$subj = as.factor(data_main_nocfs$subj)
    data_main_nocfs$probe = as.factor(data_main_nocfs$probe)
    data_main_nocfs$congruency = as.factor(data_main_nocfs$congruency)
    data_main_nocfs$conflict = as.factor(data_main_nocfs$conflict)
    
    
    # BF analysis for data_main_cfs or data_main_nocfs
    
    library(BayesFactor)
    
    bf1 <- generalTestBF(RT ~ congruency * conflict  
                         + subj + probe, 
                         whichRandom = c("subj", "probe"), 
                         data = as.data.frame(data_main_nocfs) )
    
    x <- extractBF(head(bf1) / max(bf1))
    x$bf <- 1/x$bf
    x <- x[x$bf < 100,]
    knitr::kable(x[1:2])    
    
    
#############################
#participants
   #excluded: #18, #19, #29, #30, #31 -> n = 26

#mean age:
a <- c(20,19,29,26,29,25,19,24,25,26,21,28,21,25,23,28,26,26,20,23,26,26,29,27,36,31)
describe(a)

#sex:
20/26

#left handed:
3/26

#left dominant eye:
7/26

#alpha (contrast/transparency)
c(0.1, 0.15, 0.2, 0.2803, 0.35, 0.4821, 0.4821, 0.4821, 0.4821, 0.5, 0.51, 0.56, 0.69, 0.7317,
  0.77, 0.8069, 0.8069, 0.8069, 0.83, 0.83, 0.834, 0.834, 0.8611, 0.9, 0.9, 0.94)
#############################