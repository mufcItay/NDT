######################################################################
## Zerweck et al., 2020
## Analysis Experiment 2
##  
##   for the indirect task
##  - read data and exclude erroneous + too fast/slow RT trials
##  - average across trials per participant and prime contrast
##  - perform classification based on median split
##  - calculate dprime based on median classifier
##  - perform paired t-test on RTdiff and dprime for specific contrast
##    (here we use 4.9 cd/m² as reported in our paper)
##  - perform ezANOVA on RTs with contrast x congruency
##
##   for the direct task
##  - read data and exclude too fast/slow RT trials
##  - average across trials for each participant and prime contrast
##  - calculate PCorr and dprime
##  - perform paired t-test on PCorr and dprime for specific contrast
##    (here we use 2.9 cd/m² as reported in our paper)
##
##   for the sensitivity analysis (Test for ITA)
##  - calculate statistics for dprime direct vs indirect (mean,sd,etc)
##  - perform paired t-test on dprime direct vs. indirect
######################################################################

## Startup:
cat("Clear workspace\n")
rm(list=ls()) 
cat("Loading local-lib.R\n")
source("local-lib_Exp2.R")

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
cat("Read data of Experiment 2 / Indirect Task: Experiment2_indirectTask.dat\n")
rttask <- read.table("Experiment2_indirectTask.dat",header=T)


## Print structure of data for checking (only when debugging)
if(myDebug){
  cat("Structure of data:\n")
  str(rttask)
}

##Remove trials with display timing errors:
echo1("----------------------------------------------------------------------")
echo1("Excluding trials with display timing errors:")
echo2("   Total no. trials:",length(rttask$TimingError))
echo2("   Timing error trials:",sum(rttask$TimingError== 1))
rttask <- rttask[rttask$TimingError== 0,]

##Exclude practice trials:
echo1("----------------------------------------------------------------------")
echo1("Excluding practice trials:")
echo2("   Total no. trials:",length(rttask$Task))
echo2("   Practice trials:",sum(rttask$Task== "RTTaskPrac"))
rttask <- rttask[rttask$Task== "RTTaskExp",]

##Remove too fast/slow responses:
echo1("----------------------------------------------------------------------")
echo1("Excluding too fast/slow responses:")
echo2("   Total no. trials:",length(rttask$ResponseTime))
echo2("   Too early trials:",sum(rttask$ResponseTime=="Early"))
echo2("   Too late  trials:",sum(rttask$ResponseTime=="Late"))
rttask <- rttask[rttask$ResponseTime=="Normal",]

##Remove trials with wrong response:
echo1("----------------------------------------------------------------------")
echo1("Excluding trials with wrong response:")
echo2("   Total no. trials:",length(rttask$ResponseError))
echo2("   Correct trials:",sum(rttask$ResponseError=="keypress_correct"))
echo2("   Error   trials:",sum(rttask$ResponseError=="keypress_error"))
rttask <- rttask[rttask$ResponseError=="keypress_correct",]
echo1("----------------------------------------------------------------------")

##Add column with prime contrast values in cd/m²:
rttask <- addColumnWithContrastAsCandela(rttask)

## perform analysis for each prime contrast and each participant
indResults = data.frame()
for (primeContrast in sort(unique(rttask$contrastInCandela)))
{
  echo2("Working on prime contrast:",primeContrast)
  
  # subselect contrast condition
  partrttask = rttask[rttask$contrastInCandela == primeContrast,]
  
  for(part in unique(partrttask$VP))
    {
    ##Select this participant's data:
    partData <- partrttask[partrttask$VP==part,]
  
    ##Perform standard analyses on this participant:
    partCongrRTs            <- partData[partData$Congruency==
                                          "Congruent","RT.Device"]
    partInconRTs            <- partData[partData$Congruency==
                                          "Incongruent","RT.Device"]
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
    partData$congruencyTBCoded <- partData$Congruency=="Incongruent"
    ## - then perform classification and calculate dprime:
    partDprimeRT = medianClassifierDprime(partData$RT.Device, 
                                          partData$congruencyTBCoded)
    
    ##Collect results:
    indResults <- rbind(indResults,
                        data.frame(part          = part,
                                   primecontrast = primeContrast,
                                   conN          = partConN,
                                   inconN        = partInconN,
                                   conRTmean     = partConRTmean,
                                   inconRTmean   = partInconRTmean,
                                   conRTsd       = partConRTsd,
                                   inconRTsd     = partInconRTsd,
                                   RTdiff        = partInconRTmean-partConRTmean,
                                   dprime        = partDprimeRT))
  }
}
 
   
##Calculate t-tests reported in Zerweck et al., 2020:
contrast = 4.9  # Note: you can change this parameter to see results 
                # from other prime contrast conditions. 
                # Default: 4.9 cd/m²
                # Be careful when changing because this changes the 
                # the printed summary of results below 

## some additional statistics for a specific prime contrast (default 4.9 cd/m²)
# RT data
Nparticipants <- length(indResults$RTdiff[indResults$primecontrast==contrast])
RTCon         <- indResults$conRTmean[indResults$primecontrast==contrast]
RTIncon       <- indResults$inconRTmean[indResults$primecontrast==contrast]
meanRTCon     <- mean(RTCon)
meanRTIncon   <- mean(RTIncon) 
RTDiff        <- RTIncon-RTCon
# dprime data for this specific prime contrast
dprime    <- indResults$dprime[indResults$primecontrast==contrast] 

## t-tests for RTdiff and dprime 
tRTdiff  <- t.test(indResults$RTdiff[indResults$primecontrast==contrast])
tdprime  <- t.test(indResults$dprime[indResults$primecontrast==contrast]) 

########################################################################
##calculate ANOVA with contrast and congruency as within-subject factors
# aggregate relevant data for anova
anovaDat = aggregate(rttask$RT.Device, 
                     list(sub = rttask$VP,
                          congruency = rttask$Congruency,
                          PrimeContrast = rttask$contrastInCandela),
                     mean)
names(anovaDat)[4] = "RT"

# name variables
RT <- anovaDat$RT
congruency <- anovaDat$congruency
PrimeContrast <- anovaDat$PrimeContrast

## use ezANOVA
library(ez)

echo1("----------------------------------------------------------------------")
echo1("Perform ezANOVA on RTs with congruency and prime contrast")
## ezANOVA analysis:
anovaDat$sub           <- as.factor(anovaDat$sub)
anovaDat$congruency    <- as.factor(anovaDat$congruency)
cat("Note: PrimeContrast is calibrated and therefore (roughly) 
equally spaced, such that conversion to factor is not necessary.")
res.ezANOVA <- ezANOVA(data=anovaDat,dv=RT,wid=sub,within=.(congruency,PrimeContrast),
                       detailed=T,return_aov=F)
echo1("----------------------------------------------------------------------")
#cat("Results ezANOVA (indirect task):\n")
#cat("----------------------------------------------------------------------\n")
#print(res.ezANOVA)
################################################################################


##Print results for the indirect task:
echo2("...number of participants, indirect task: ",Nparticipants)
echo1("----------------------------------------------------------------------")
echo1("Head of aggregated results per participant and prime contrast, indirect task:")
echo1("----------------------------------------------------------------------")
print(head(indResults))

echo1("----------------------------------------------------------------------")
echo3("RT-difference betw. congruent and incongruent conditions at"
      ,contrast,"cd/m²:")
echo1("----------------------------------------------------------------------")
echo3("Mean RTcongruent:                   ", meanRTCon,"ms")
echo3("Mean RTincongruent:                 ", meanRTIncon,"ms")
echo3("Mean of RTdiff:                     ", mean(RTDiff),"ms")
echo3("SD of RTdiff:                       ", sd(RTDiff)  ,"ms")
echo3("SEM of RTdiff:                      ", mySEM(RTDiff),"ms")
echot("t-test of RTdiff against zero:      ", tRTdiff                            )

echo1("----------------------------------------------------------------------")
echo1("Results ezANOVA with contrast and congruency (indirect task):")
echo1("----------------------------------------------------------------------")
echoezMain("main effect of congruency:           ", res.ezANOVA)
echoezInt("interaction effect congruency*contrast", res.ezANOVA)
echo1("----------------------------------------------------------------------")

echo3("Sensitivity dprime (indirect task) at",contrast,"cd/m²:")
echo1("----------------------------------------------------------------------")
echo2("Mean    dprime:                    ", mean(dprime) )
echo2("SD      dprime:                    ", sd(dprime) )
echo2("SEM     dprime:                     ", mySEM(dprime) )
echot("t-test  dprime against 0:           ", tdprime)
echo1("----------------------------------------------------------------------")

##################################################################################
##################################################################################
#### Analysis direct task (perctask) ####
##################################################################################
##################################################################################

# direct task
echo1("----------------------------------------------------------------------")
cat("Read data of Experiment 2 / Direct Task: Experiment2_directTask.dat\n")
perc <- read.table("Experiment2_directTask.dat",header=T)
echo1("----------------------------------------------------------------------")

##Remove trials with display timing errors:
echo1("----------------------------------------------------------------------")
echo1("Excluding trials with display timing errors:")
echo2("   Total no. trials:",length(perc$TimingError))
echo2("   Timing error trials:",sum(perc$TimingError== 1)) # no timing errors
perc <- perc[perc$TimingError== 0,]

##Exclude practice trials:
echo1("----------------------------------------------------------------------")
echo1("Excluding practice trials:")
echo2("   Total no. trials:",length(perc$Task))
echo2("   Practice trials:",sum(perc$Task== "PercPrac"))
perc <- perc[perc$Task== "PercExp",]

##Remove too fast/slow responses:
echo1("----------------------------------------------------------------------")
echo1("Excluding too fast/slow responses:")
echo2("   Total no. trials:",length(perc$ResponseTime))
echo2("   Too early trials:",sum(perc$ResponseTime=="Early"))
echo2("   Too late  trials:",sum(perc$ResponseTime=="Late"))
perc <- perc[perc$ResponseTime=="Normal",]
echo1("----------------------------------------------------------------------")

##Add column with prime contrast values in cd/m²:
perc <- addColumnWithContrastAsCandela(perc)

## perform analysis for each prime contrast and each participant
dirResults = data.frame()
for (primeContrast in sort(unique(perc$contrastInCandela)))
{
  echo2("Working on prime contrast:",primeContrast)
  
  ## Select contrast condition
  partperc = perc[perc$contrastInCandela == primeContrast,]
  
  for(part in unique(partperc$VP))
  {
    ##Select this participant's data:
    partData <- partperc[partperc$VP==part,]
    
    ##Calculate percent correct for this participant:
    partNtrials <- length(partData$ResponseError)
    partPCorr   <- mean(partData$ResponseError=="keypress_correct")*100
    
    ##calculate dprime for this participant:
    HR <- mean((partData$ResponseError=="keypress_correct")
               [grepl("[6,9]", partData$Prime)])
    FA <- 1 - mean((partData$ResponseError=="keypress_correct")
                   [grepl("[1,4]", partData$Prime)])
    
    ##If HR or FA are 0 or 1, use convention to compute finite d'
    nHR = sum(grepl("[6,9]", partData$Prime)) ## Number of trials for HR calculation
    if (HR == 0) HR = 1/nHR
    if (HR == 1) HR = (nHR-1)/nHR
    nFA = sum(grepl("[1,4]", partData$Prime)) ## Number of trials for FA calculation
    if (FA == 0) FA = 1/nFA
    if (FA == 1) FA = (nFA-1)/nFA
    
    #calculate dprime
    partdprime <- qnorm(HR) - qnorm(FA)
    
    ##Collect results:
    dirResults <- rbind(dirResults,
                        data.frame(part        = part,
                                   PrimeContrast = primeContrast,
                                   Ntrials     = partNtrials,
                                   PCorr       = partPCorr,
                                   dprime      = partdprime))
  }
}
 

##Calculate t-tests reported in Zerweck et al., 2020:
contrastDir = 2.9  # Note: you can change this parameter to see results 
# from other prime contrast conditions. 
# Default: 2.9 cd/m² for the direct task
# Be careful when changing because this changes the 
# the printed summary of results below 

## some additional statistics for a specific prime contrast (default 2.9 cd/m²)
# Percent correct
PCorr         <- dirResults$PCorr[dirResults$PrimeContrast==contrastDir]
Nparticipants <- length(PCorr)

# dprime data for this specific prime contrast
dprime        <- dirResults$dprime[dirResults$PrimeContrast==contrastDir] 
sd_dprime     <- sd(dprime)

## t-tests for Percent Correct and dprime 
tPCorr   <- t.test(PCorr, mu=50)
tdprime  <- t.test(dprime) 


##Print results:
echo2("...number of participants, direct task: ",Nparticipants)
echo1("----------------------------------------------------------------------")
echo1("Head of aggregated results per participant and prime contrast, direct task:")
echo1("----------------------------------------------------------------------")
print(head(dirResults))

echo1("----------------------------------------------------------------------")
echo3("Percent-correct (direct task) at",contrastDir,"cd/m²:")
echo1("----------------------------------------------------------------------")
echo2("Mean      PCorr:                   ", mean(PCorr))
echo2("SD of     PCorr:                   ", sd(PCorr))
echo2("SEM of    PCorr:                   ", mySEM(PCorr))
echot("t-test of PCorr against chance     ", tPCorr)

echo1("----------------------------------------------------------------------")
echo3("sensitivity dprime (direct task) at",contrastDir,"cd/m²:")
echo1("----------------------------------------------------------------------")
echo2("Mean      dprime:                    ", mean(dprime))
echo2("SD of     dprime:                    ", sd(dprime))
echo2("SEM of    dprime:                    ", mySEM(dprime))
echot("t-test of dprime against zero        ", tdprime)
echo1("----------------------------------------------------------------------")

##################################################################################
##################################################################################
#### Sensitivity analysis (direct vs indirect; ITA) ####
##################################################################################
##################################################################################

echo1("----------------------------------------------------------------------")
echo1("Perform sensitivity analysis comparing both tasks(ITA check) at 4.9 cd/m² :")
echo1("----------------------------------------------------------------------")

# select prime contrast you want to test
contrast_ITA <- 4.9  #cd/m². Default 4.9

# calculate difference between indirect and direct task dprime
dprimeInd  <- indResults$dprime[indResults$primecontrast==contrast_ITA]
dprimeDir  <- dirResults$dprime[dirResults$PrimeContrast==contrast_ITA]
dprimeDiff <- dprimeInd-dprimeDir

# calculate additional statistics for direct vs indirect sensitivity 
# at the selected contrast condition:
mean_dprimeInd <- mean(dprimeInd)
mean_dprimeDir <- mean(dprimeDir)
sd_dprimeInd   <- sd(dprimeInd)
sd_dprimeDir   <- sd(dprimeDir)

# calculate t-test of dprimeDiff against zero (Test for an ITA)
# at the selected contrast condition
tdprimeDiff <- t.test(dprimeDiff)

# Print results for the sensitivity analysis (ITA Test)
echo1("----------------------------------------------------------------------")
echo3("difference in dprime (indirect-direct) at",contrast_ITA,"cd/m²:")
echo1("----------------------------------------------------------------------")
echo2("Mean of   dprime      indirect task:           ", mean_dprimeInd)
echo2("Mean of   dprime      direct task:             ", mean_dprimeDir)
echo2("SD of     dprime      indirect task:           ", sd_dprimeInd)
echo2("SD of     dprime      direct task:             ", sd_dprimeDir)
echo2("mean of   dprimeDiff (indirect-direct):        ", mean(dprimeDiff))
echo2("SEM of    dprimeDiff  indirect-direct:         ", mySEM(dprimeDiff))
echot("t-test of dprimeDiff  against zero             ", tdprime)
echo1("----------------------------------------------------------------------")
