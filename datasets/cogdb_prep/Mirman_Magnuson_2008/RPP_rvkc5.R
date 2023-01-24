################################################################################
##################          REPLICATION R CODES               ##################
################################################################################
#
##@@ INSTRUCTIONS @@##
# Please do not remove any comments from this file, but feel free to add as many
# comments to R code as you like!
#
# Replace all ---USER INPUT--- sections with relevant input (including the - 
# symbols).
#
# When you are done, save this file as RPP_YYYY.R, where YYYY is the OSF code of 
# the project.
# e.g., The first study, Tracing attention and the activation...., should be
# called RPP_qwkum.R (since https://osf.io/qwkum/ is the project site)
#
# For any questions, please contact me at sacha.epskamp@gmail.com
#
##@@ GENERAL INFORMATION @@##
#@ Study Title: Attractor dynamics and semantic neighborhood density: Processing is slowed by near neighbors and speeded by distant neighbors.
#@ Coder name: John Hodsoll
#@ Coder e-mail: john.hodsoll@kcl.ac.uk
#@ Type of statistic: F
#@ Type of effect-size: partial eta squared
#@ OSF link project: https://osf.io/rvkc5/
#@ OSF link replication report: https://osf.io/r57hu/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library("foreign")
library("reshape2")

source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.

#Data available in SPSS .sav format

file <- getOSFfile("https://osf.io/etbhm/")
mirm.data <- read.spss(file, to.data.frame=TRUE) 

##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

#Data in spss wide format - reshape
mirm.long <- melt(mirm.data, 
                  id.vars="SUBJECT",
                  variable.name="cond",
                  value.name="RT")

mirm.long$nearness <- 1
mirm.long$nearness[mirm.long$cond == "COND1" | mirm.long$cond == "COND2"] <- 2

mirm.long$distance <- 1
mirm.long$distance[mirm.long$cond == "COND1" | mirm.long$cond == "COND3"] <- 2

mirm.long <- transform(mirm.long, distance=factor(distance))
mirm.long <- transform(mirm.long, nearness=factor(nearness))

##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object

d <- data.long %>% group_by(SUBJECT, COND, nearness, distance) %>% summarise(m=mean(RT))

mirm.aov1 = aov(RT ~ nearness*distance + Error(SUBJECT / (nearness*distance)), data=mirm.long)
summ.mirm.aov1 <- summary(mirm.aov1)


# As balanced ANOVA, type 1 Sum of Squares will be same as type 3 Sum of Squares. 

##@@ STATISTIC @@##
#F from interaction term
summ.mirm.aov1[["Error: SUBJECT:nearness"]][[1]][["F value"]][[1]]

##@@ P-VALUE @@##
summ.mirm.aov1[["Error: SUBJECT:nearness"]][[1]][["Pr(>F)"]][[1]]

##@@ SAMPLE SIZE @@##
nrow(mirm.data)

##@@ EFFECT SIZE @@##
sumsq.error <- summ.mirm.aov1[["Error: SUBJECT:nearness"]][[1]][["Sum Sq"]][[2]]
sumsq.effect <- summ.mirm.aov1[["Error: SUBJECT:nearness"]][[1]][["Sum Sq"]][[1]]

effect.size <- sumsq.effect / (sumsq.effect + sumsq.error)
print(effect.size)  
##@@ AGREEMENT WITH AUTHORS @@##

TRUE

#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---