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
#@ Study Title: Forti (Sensitivity to object viewpoint)
#@ Coder name: David Dobolyi
#@ Coder e-mail: ddobolyi@virginia.edu
#@ Type of statistic: repeated measures ANOVA
#@ Type of effect-size: partial eta squared (unclear in author's report if it's partial, standard, or generalized)
#@ OSF link project: https://osf.io/tf8ky/
#@ OSF link replication report: https://osf.io/nhqgs/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library("readstata13") # read dta Stata 13 file
library("doBy") # organize data
library("plyr") # more data organization
library("schoRsch") # compute p-eta-sq more easily
library("ez") # required for ezANOVA repeated measures
library("lme4") # required to handle missing data repeated measures
library("car") # required for creating an F table of lme4 results
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
rawDat <- getOSFfile("https://osf.io/giwbt/")

##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL differences between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

# convert Stata 13 file
dat <- read.dta13(rawDat)
str(dat)
dat$subject <- factor(dat$subject)
head(dat)
nrow(dat)

# from here on, we follow S Forti, GW Humphreys_Analysis.do as a guide

# code targetPosition variable based on screen position
dat$targetPosition <- NA
dat[which(dat$position == 5),]$targetPosition <- 1
dat[which(dat$position == 4),]$targetPosition <- 2
dat[which(dat$position == 3),]$targetPosition <- 3
dat[which(dat$position == 2),]$targetPosition <- 4
dat[which(dat$position == 1),]$targetPosition <- 5
dat[which(dat$position == 8),]$targetPosition <- 6
dat[which(dat$position == 7),]$targetPosition <- 7
dat[which(dat$position == 6),]$targetPosition <- 8
table(dat$targetPosition)

# drop trials missing trigger and those where trigger is less than 4000 (i.e., blank screens & fixation cross slides)
datClean <- subset(dat, !is.na(trigger) & trigger >= 4000)
nrow(datClean)

# keep only correct trials in the analyses, i.e., those on which the target was eventually fixated upon
datClean$AOITarget <- FALSE
datClean[which(datClean$targetPosition == datClean$AOI),]$AOITarget <- 1

# exclude all trials where subject did not fixate on target at least once
didFixate <- summaryBy(AOITarget ~ subject + trigger, FUN=c(max, length), data=datClean)

# the following function accomplishes the above; note this takes a while, so we include a progress bar...
datClean$exclusion <- FALSE

computeExclusion <- function (didFixate, datClean) {

	pb <- txtProgressBar(style=3)
	for (i in 1:nrow(didFixate)) {
		if (didFixate[i,]$AOITarget.max != 1)
			datClean[which(datClean$subject == didFixate$subject[i] & datClean$trigger == didFixate$trigger[i]),]$exclusion <- TRUE
		
		setTxtProgressBar(pb, i/nrow(didFixate))
		}
	
	return(datClean)
	}

datCleanExclusion <- computeExclusion(didFixate, datClean)

# author notes the number of dropped trials for targetPosition 3 and 7 (middle of screen, left and right) to be 2158, so I check this
nrow(subset(summaryBy(exclusion ~ subject + trigger + targetPosition, FUN=mean, data=datCleanExclusion), targetPosition == 3 | targetPosition == 7)) # 2158

# calculate amount of data removed (noted in author script as 23.66%)
nrow(subset(summaryBy(exclusion ~ subject + trigger + targetPosition, FUN=mean, data=datCleanExclusion), targetPosition == 3 | targetPosition == 7))/nrow(summaryBy(exclusion ~ subject + trigger + targetPosition, FUN=mean, data=datCleanExclusion)) # 23.66% matches up

# drop trials within the center left/right from the analysis
datCleanExclusionDrop <- subset(datCleanExclusion, targetPosition != 3 & targetPosition != 7)

# author notes 624 trials should be dropped by this action, so we check this
table(summaryBy(exclusion ~ subject + trigger + targetPosition, data = datCleanExclusionDrop)$exclusion.mean) # verified

# drop excluded trials
datCleanExclusionDropMore <- subset(datCleanExclusionDrop, exclusion != TRUE)

# drop trials where AOI equals 0
datCleanExclusionDropMoreAOI <- subset(datCleanExclusionDropMore, AOI != 0)
nrow(datCleanExclusionDropMoreAOI)

# sort data
datCleanExclusionDropMoreAOISorted <- datCleanExclusionDropMoreAOI[order(datCleanExclusionDropMoreAOI$subject, datCleanExclusionDropMoreAOI$trigger, datCleanExclusionDropMoreAOI$gazetime),]
head(datCleanExclusionDropMoreAOISorted)

# code trials by first fixation, using sort
datCleanExclusionDropMoreAOISorted$firstfix <- NA
firstfix <- ddply(datCleanExclusionDropMoreAOISorted,.(subject, trigger),.fun = function(x){
                                         x$firstfix <- 1:nrow(x)
                                         return(x)})

# keep only first fixation trials
firstfixOnly <- subset(firstfix, firstfix == 1)
nrow(firstfixOnly)

# verify a line from the writeup to make sure everything so far is correct:
# The probability of the first fixation being on the target was calculated for each cell as
# follows: 36.97% (top middle), 34.39% (top right), 40.54% (low right), 43.96% (low middle),
# 42.49% (low left) and 32.44% (top left).
summaryBy(AOITarget ~ targetPosition, data=firstfixOnly) # verified

# create coding for up factor
firstfixOnly$up <- 0
firstfixOnly[which(firstfixOnly$targetPosition == 1 | firstfixOnly$targetPosition == 2 | firstfixOnly$targetPosition == 8),]$up <- 1
table(firstfixOnly$up)

# create coding for action factor
firstfixOnly$action <- 0
firstfixOnly[which(firstfixOnly$block == 1),]$action <- 1
table(firstfixOnly$action)

# create coding for prototypical (proto) factor
firstfixOnly$prototypical <- firstfixOnly$target %% 2
table(firstfixOnly$prototypical)

# compute means within factors of interest for within subjects ANOVA
firstfixOnlyMeans <- summaryBy(AOITarget ~ subject + prototypical + up + action, FUN=mean, data=firstfixOnly)
nrow(firstfixOnlyMeans)
str(firstfixOnlyMeans)
firstfixOnlyMeans$subject <- factor(firstfixOnlyMeans$subject)
firstfixOnlyMeans$prototypical <- factor(firstfixOnlyMeans$prototypical)
firstfixOnlyMeans$up <- factor(firstfixOnlyMeans$up)
firstfixOnlyMeans$action <- factor(firstfixOnlyMeans$action)

# notice that subject 8 is missing some combinations of up*prototypical*action
summaryBy(up + prototypical + action ~ subject, FUN=length, data = firstfixOnlyMeans)

# create a separate dataset dropping subject 8
firstfixOnlyMeansFix <- subset(firstfixOnlyMeans, subject != 8)
firstfixOnlyMeansFix$subject <- factor(firstfixOnlyMeansFix$subject)


##@@ DATA ANALYSIS @@##
#@ NOTE: Include a print or summary call on the resulting object

# this repeated measure anova fails because subject 8 is missing observations
# prototypicalByUp <- ezANOVA(
#     data = firstfixOnlyMeans
#     , dv = AOITarget.mean
#     , wid = subject
#     , within = .(up, prototypical, action)
#     , detailed = TRUE
#     , return_aov=TRUE
# 	)
# prototypicalByUp # not working due to missing data
# 
# this works however, but it is not identical to what the authors report
prototypicalByUpFix <- ezANOVA(
    data = firstfixOnlyMeansFix
    , dv = AOITarget.mean
    , wid = subject
    , within = .(up, prototypical, action)
    , detailed = TRUE
    , return_aov=TRUE
	)
prototypicalByUpFix
result <- anova_out(prototypicalByUpFix, print=TRUE)
#str(prototypicalByUpFix)

# note we are interested in the interaction of prototypical:up

##@@ STATISTIC @@##
prototypicalByUpFix$ANOVA$F[5]

##@@ P-VALUE @@##
prototypicalByUpFix$ANOVA$p[5]
  
##@@ SAMPLE SIZE @@##
prototypicalByUpFix$ANOVA$DFd[5] + prototypicalByUpFix$ANOVA$DFn[5]

##@@ EFFECT SIZE @@##
result[[1]][5,7] # using package
# 
# # alternatively
# summary(prototypicalByUpFix$aov)$"Error: subject:up:prototypical"[[1]]$"Sum Sq"[1]/(summary(prototypicalByUpFix$aov)$"Error: subject:up:prototypical"[[1]]$"Sum Sq"[1] + summary(prototypicalByUpFix$aov)$"Error: subject:up:prototypical"[[1]]$"Sum Sq"[2])

  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE

#@ Reason disagreement: