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
#@ Study Title: On the additive effects of stimulus quality and word frequency in lexical decision: 
#               evidence for opposing interactive influences revealed by RT distributional analyses.
#@ Coder name: Frits Traets
#@ Coder e-mail: frits.traets@student.kuleuven.be
#@ Type of statistic: F
#@ Type of effect-size: none reported (calculated partial eta squared)
#@ OSF link project: https://osf.io/ahpik/
#@ OSF link replication report: https://osf.io/dh4jx/
#
##@@ REQUIRED PACKAGES @@##
library("httr")       #to Read in data
library("RCurl")
library("xlsx")
library("gdata")
library("tidyr")      #to manipulate data
library("ez")         #to analyze data
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file<-getOSFfile('https://osf.io/6kaw2/')
d<-read.xlsx(file,1)  


##@@ DATA MANIPULATION @@##
  #@ NOTE: Include here ALL difference between OSF data and data used in analysis
  #@ TIP: You will want to learn all about dplyr for manipulating data.

    #Go to long data
    d.long<-gather(d,condition,score, Highfrequencyclear:	Lowfrequencydegraded)

    #Make factors
    d.long$quality<-as.factor(rep(c("clear","degraded"),each=142))
    d.long$frequency<-as.factor(rep(c("high","low"),each=71))
    d.long$sub<-as.factor(rep(c(1:71)))


##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
  result<-ezANOVA(dv=score,within=.(quality,frequency),data=d.long,wid=sub,return_aov=T)
  print(result)
  
  ##@@ STATISTIC @@##
  result$ANOVA$F[3]
  
  ##@@ P-VALUE @@##
  result$ANOVA$p[3]
  
  ##@@ SAMPLE SIZE @@##
  nrow(d)
  
  ##@@ EFFECT SIZE @@##
  #(eta squared partial = SS factor / (SS factor + SS error) )
  #Interaction
  eta.i= 115052.2/(115052.2+683497.9)
  eta.i
  

  ##@@ AGREEMENT WITH AUTHORS @@##
  TRUE
  
  #@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---