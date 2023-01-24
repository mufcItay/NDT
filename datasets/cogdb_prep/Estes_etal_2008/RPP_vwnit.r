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
#@ Study Title: Head up, foot down. Object words orient attention to the objects’ typical location.
#@ Coder name: Matthias Lippold
#@ Coder e-mail: matthias.lippold1@stud.uni-goettingen.de
#@ Type of statistic: F
#@ Type of effect-size: n^2
#@ OSF link project: https:https:https://osf.io/vwnit/
#@ OSF link replication report:https://osf.io/b7zek/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library("foreign")
library("ez")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file<-getOSFfile("https://osf.io/nk74v/") 
data<-read.spss(file, to.data.frame =TRUE)
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

#Variable "Subj" needs to be a factor to perform ezAnova-function
data$Subj<-as.factor(data$Subj)

#Remove all Subjects with an reactiontime beyond 1000ms
data$RT.cleaned<-data$RT
data$RT.cleaned[data$RT>1000]=NA
data.cleaned<-na.omit(data)

#errorrate needs to be numeric to perform analysis
data$errorate<-as.numeric(data$Correct)

##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
Result.RT<-ezANOVA(data = data.cleaned,dv=RT,wid=Subj,within=typisch,detailed = TRUE,type=3)
Result.errorrate<-ezANOVA(data = data,dv=errorate,wid=Subj, within=typisch ,detailed = TRUE,type=3)
##@@ STATISTIC @@##

Result.errorrate$ANOVA$F[2] # error rate
Result.RT$ANOVA$F[2] #reactiontime

  ##@@ P-VALUE @@##
Result.errorrate$ANOVA$p[2] # error rate
Result.RT$ANOVA$p[2]#reactiontime

  ##@@ SAMPLE SIZE @@##
length(unique(data$Subj))
  ##@@ EFFECT SIZE @@##
Result.errorrate$ANOVA$ges[2] # error rate
Result.RT$ANOVA$ges[2] #reactiontime

  ##@@ AGREEMENT WITH AUTHORS @@##
TRUE  
  #@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---