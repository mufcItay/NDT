######################################################################
## Zerweck et al., 2020
## Analysis Experiment 1
##  
##   for the indirect task
##  - read data and exclude erroneous + too fast/slow RT trials
##  - average across trials for each participant
##  - perform classification based on median split
##  - calculate dprime based on median classifier
##  - perform paired t-test on RTdiff and dprime
##  
##   for the direct task
##  - read data and exclude too fast/slow RT trials
##  - average across trials for each participant
##  - calculate PCorr and dprime
##  - perform paired t-test on PCorr and dprime
##
##   for the sensitivity analysis (Test for ITA)
##  - calculate statistics for dprime direct vs indirect (mean,sd,etc)
##  - perform paired t-test on dprime direct vs. indirect
######################################################################

## Startup:
cat("Clear workspace\n")
rm(list=ls()) 
cat("Loading local-lib.R\n")
source("local-lib_Exp1.R")

## Settings:
myDebug <- F


##################################################################################
##################################################################################
#### Analysis indirect task (rttask) ####
##################################################################################
##################################################################################

## Read data:
# indirect task
echo1("----------------------------------------------------------------------")
cat("Read data of Experiment 1 / Indirect Task: Experiment1_indirectTask.dat\n")
rttask <- read.table("Experiment1_indirectTask.dat",header=T)


## Print structure of data for checking (only when debugging)
if(myDebug){
  cat("Structure of data:\n")
  str(rttask)
}

##Remove too fast/slow responses:
echo1("----------------------------------------------------------------------")
echo1("Excluding too fast/slow responses:")
echo2("   Total no. trials:",length(rttask$time_error))
echo2("   Too early trials:",sum(rttask$time_error=="Early"))
echo2("   Too late  trials:",sum(rttask$time_error=="Late"))
rttask <- rttask[rttask$time_error=="Normal",]

##Remove trials with wrong response:
echo1("----------------------------------------------------------------------")
echo1("Excluding trials with wrong response:")
echo2("   Total no. trials:",length(rttask$key_error))
echo2("   Correct trials:",sum(rttask$key_error=="correct"))
echo2("   Error   trials:",sum(rttask$key_error=="error"))
rttask <- rttask[rttask$key_error=="correct",]

## Perform analyses for each participant:
echo1("----------------------------------------------------------------------")
cat("Aggregate data for each participant\n")
indResults <- data.frame()
for(part in unique(rttask$sub)){
  echo2("Working on participant:",part)
  
  ##Select this participant's data:
  partData <- rttask[rttask$sub==part,]
  
  ##Perform standard analyses on this participant:
  partCongrRTs            <- partData[partData$congruency=="Congruent","RT"]
  partInconRTs            <- partData[partData$congruency=="Incongruent","RT"]
  partConN                <- length(partCongrRTs)
  partInconN              <- length(partInconRTs)
  partConRTmean           <- mean(partCongrRTs)
  partInconRTmean         <- mean(partInconRTs)
  partConRTsd             <- sd(partCongrRTs)
  partInconRTsd           <- sd(partInconRTs)
  
  ##Classifier:
  ## - First recode data to be compatible with our library-function
  ##   which is based on the coding:
  ##   0=congruent / 1=incongruent
  partData$congruencyTBCoded <- partData$congruency=="Incongruent"
  ## - then perform classification and calculate percent correct:
  partPCorrRT  <- medianClassifier(partData$RT, partData$congruencyTBCoded)
  ## - then perform classification and calculate dprime:
  partDprimeRT <- medianClassifierDprime(partData$RT, partData$congruencyTBCoded)

  ##Collect results:
  indResults <- rbind(indResults,
                      data.frame(part        = part,
                                 conN        = partConN,
                                 inconN      = partInconN,
                                 conRTmean   = partConRTmean,
                                 inconRTmean = partInconRTmean,
                                 conRTsd     = partConRTsd,
                                 inconRTsd   = partInconRTsd,
                                 RTdiff      = partInconRTmean-partConRTmean,
                                 dprime      = partDprimeRT,
                                 medianPCorr = partPCorrRT))
}

##Calculate t-tests:
tRTdiff      <- t.test(indResults$RTdiff)             #H0: RTdiff      = 0
tdprime      <- t.test(indResults$dprime)             #H0: meandprime  = 0
tMedianPCorr <- t.test(indResults$medianPCorr, mu=50) #H0: medianPCorr = 50%

##Some additional statistics:
Nparticipants      <- length(indResults$RTdiff)
meanRTdiff         <- mean(indResults$RTdiff)


##Print results for the indirect task:
echo2("...number of participants, indirect task: ",Nparticipants)
echo1("----------------------------------------------------------------------")
echo1("Aggregated results for each participant, indirect task:")
echo1("----------------------------------------------------------------------")
print(indResults)

echo1("----------------------------------------------------------------------")
echo1("RT-difference betw. congruent and incongruent conditions:")
echo1("----------------------------------------------------------------------")
echo3("Mean RTcongruent:                   ", mean(indResults$conRTmean)    ,"ms")
echo3("Mean RTincongruent:                 ", mean(indResults$inconRTmean)  ,"ms")
echo3("Mean of RTdiff:                     ", meanRTdiff                    ,"ms")
echo3("SD of RTdiff:                       ", sd(indResults$RTdiff)         ,"ms")
echo3("SEM of RTdiff:                      ", mySEM(indResults$RTdiff)      ,"ms")
echot("t-test of RTdiff against zero:      ", tRTdiff                            )

echo1("----------------------------------------------------------------------")
echo1("Sensitivity dprime (indirect task):")
echo1("----------------------------------------------------------------------")
echo2("Mean   dprime:                      ", mean(indResults$dprime) )
echo2("SD     dprime:                      ", sd(indResults$dprime) )
echo2("SEM    dprime:                      ", mySEM(indResults$dprime) )
echo2("Median dprime:                      ", median(indResults$dprime) )
echot("t-test  dprime against 0:           ", tdprime)
echo1("----------------------------------------------------------------------")

echo1("----------------------------------------------------------------------")
echo1("Median Classifier percent correct (indirect task):")
echo1("----------------------------------------------------------------------")
echo2("Mean   medianPCorr:                 ", mean(indResults$medianPCorr) )
echo2("SD     medianPCorr:                 ", sd(indResults$medianPCorr) )
echo2("SEM    medianPCorr:                 ", mySEM(indResults$medianPCorr) )
echo2("Median medianPCorr:                 ", median(indResults$medianPCorr) )
echot("t-test medianPCorr against 0:       ", tdprime)
echo1("----------------------------------------------------------------------")

##################################################################################
##################################################################################
#### Analysis direct task (perctask) ####
##################################################################################
##################################################################################

# direct task
echo1("----------------------------------------------------------------------")
cat("Read data of Experiment 1 / Direct Task: Experiment1_directTask.dat\n")
perc <- read.table("Experiment1_directTask.dat",header=T)

##Remove too fast/slow responses:
echo1("----------------------------------------------------------------------")
echo1("Excluding too fast/slow responses:")
echo2("   Total no. trials:",length(perc$time_error))
echo2("   Too early trials:",sum(perc$time_error=="Early"))
echo2("   Too late  trials:",sum(perc$time_error=="Late"))
perc <- perc[perc$time_error=="Normal",]

## Perform analyses for each participant:
echo1("----------------------------------------------------------------------")
cat("Aggregate data for each participant\n")
dirResults <- data.frame()
for(part in unique(perc$sub)){
  echo2("Working on participant:",part)
  
  ##Select this participant's data:
  partData <- perc[perc$sub==part,]
  
  ##Calculate percent correct for this participant:
  partNtrials <- length(partData$key_error)
  partPCorr   <- mean(partData$key_error=="correct")*100
  
  ## calculate dprime for this participant:
  HR <- mean((partData$key_error=="correct")
            [grepl("[6,9]", partData$prime)])
  FA <- 1 - mean((partData$key_error=="correct")
                [grepl("[1,4]", partData$prime)])
  
  partdprime <- qnorm(HR) - qnorm(FA)
  
  ##Collect results:
  dirResults <- rbind(dirResults,
                      data.frame(part        = part,
                                 Ntrials     = partNtrials,
                                 PCorr       = partPCorr,
                                 dprime      = partdprime))
}

##Calculate t-tests:
tPCorr  <- t.test(dirResults$PCorr,mu=50)  #H0: PCorr = 50
tdprime <- t.test(dirResults$dprime)       #H0: dprime = 0

##Some additional statistics:
Nparticipants      <- length(dirResults$PCorr)

##Print results:
echo2("...number of participants, direct task: ",Nparticipants)
echo1("----------------------------------------------------------------------")
echo1("Aggregated results for each participant, direct task:")
echo1("----------------------------------------------------------------------")
print(dirResults)

echo1("----------------------------------------------------------------------")
echo1("Percent-correct (direct task):")
echo1("----------------------------------------------------------------------")
echo2("Mean PCorr:                         ", mean(dirResults$PCorr))
echo2("SD of PCorr:                        ", sd(dirResults$PCorr))
echo2("SEM of PCorr:                       ", mySEM(dirResults$PCorr))
echot("t-test of PCorr against chance      ", tPCorr)

echo1("----------------------------------------------------------------------")
echo1("dprime (direct task):")
echo1("----------------------------------------------------------------------")
echo2("Mean dprime:                         ", mean(dirResults$dprime))
echo2("SD of dprime:                        ", sd(dirResults$dprime))
echo2("SEM of dprime:                       ", mySEM(dirResults$dprime))
echot("t-test of dprime against zero        ", tdprime)


##################################################################################
##################################################################################
#### Sensitivity analysis (direct vs indirect; ITA) ####
##################################################################################
##################################################################################

echo1("----------------------------------------------------------------------")
echo1("----------------------------------------------------------------------")
echo1("Perform sensitivity analysis comparing both tasks(ITA check) :")
echo1("----------------------------------------------------------------------")

# calculate difference between indirect and direct task dprime
dprimeDiff <- indResults$dprime-dirResults$dprime

# calculate t-test of dprimeDiff against zero (Test for an ITA)
tdprimeDiff <- t.test(dprimeDiff)

# some additional statistics..
meandprimeDiff <- mean(dprimeDiff)
sddprimeDiff   <- sd(dprimeDiff)
SEMdprimeDiff  <- mySEM(dprimeDiff)

# Print results for the sensitivity analysis (ITA Test)
echo1("difference in dprime (indirect-direct):")
echo1("----------------------------------------------------------------------")
echo2("Mean dprime difference:                     ", meandprimeDiff)
echo2("SD of dprime difference:                    ", sddprimeDiff)
echo2("SEM of dprime difference:                   ", SEMdprimeDiff)
echot("t-test of dprime difference against zero    ", tdprimeDiff)


