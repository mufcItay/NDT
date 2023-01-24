rm(list=ls(all=T)); # clear workspace

# Load libraries
library(ggplot2)
library(gridExtra)
library(dplyr)


exclude_fa        = 1 # exclude cases in which cue was V and subjects reported A? (cleaner to exclude)
treat_subaware    = "0" # how we treat cases in which cue was AV and subjects reported A|V: exclude (0), unaware (1) or aware (2)

# Enter experiment's info here ----------------------------------------
rootdir=paste(dirname(rstudioapi::getActiveDocumentContext()$path),'/Data',sep='')
setwd(rootdir)

## Choose mode
# ''       :  contrasts congruent vs. incongruent trials with no spatiotemporal modulation (many subjects few trials)
# '_long'       :  contrasts congruent vs. incongruent trials with no spatiotemporal modulation
# '_bigspace'   :  contrasts spatially congruent vs. incongruent trials (incongruent is 45cm away)
# '_smallspace' :  contrasts spatially congruent vs. incongruent trials (incongruent is 15cm away)
# '_bigtime'    :  contrasts temporally congruent vs. incongruent trials (incongruent is 300ms apart)
# '_smalltime'  :  contrasts temporally congruent vs. incongruent trials (incongruent is 100ms apart)

modes=c('_long', '_smalltime','_bigtime', '_smallspace', '_bigspace' )
biga=c() # bigdataframe will all 5 experiments

## loop over experiments
for (mnum in 1:length(modes)) {
  mode=modes[mnum]

  # Load files and gather the data in a single data.frame
  ssFilesList<-dir(rootdir, glob2rx(paste('data', mode,'.res',sep='')),full.names = T)
  data <- lapply(ssFilesList,function(x) read.table(x,header=FALSE,sep=","))
  a <- do.call("rbind", data)
  names(a)=c('rawcond','touchkey','rt','cuekey','suj')
  a$suj=as.factor(a$suj)

  # re-encode cues according to first element of rawcond: 1-A / 2-V / 3-AV / 4-no
  a$cue=ifelse(substr(a$rawcond,1,1)=='1','A',
               ifelse(substr(a$rawcond,1,1)=='2','V',
                      ifelse(substr(a$rawcond,1,1)=='3','AV',
                             ifelse(substr(a$rawcond,1,1)=='4','no',
                                    ifelse(substr(a$rawcond,1,1)=='6','AV','??')))))
  a$distcue=0;a$dist=0 # filler for other conditions

  if (mode=='_bigspace' | mode=='_smallspace') {
    # re-encode distance between cues according to second element of rawcond: 1-3: L / 4-6-R (targets always 2 or 5)
    a$distcue=ifelse(substr(a$rawcond,2,2)=='1','L1',
                     ifelse(substr(a$rawcond,2,2)=='2','00',
                            ifelse(substr(a$rawcond,2,2)=='3','R1',
                                   ifelse(substr(a$rawcond,2,2)=='4','L1',
                                          ifelse(substr(a$rawcond,2,2)=='5','00',
                                                 ifelse(substr(a$rawcond,2,2)=='6','R1','??'))))))

    a$distcue[substr(a$rawcond,1,1)=='6'] = 'B1'; # both visual and audio have a distance of 1 with touch
    # mirror distcue just to keep distance
    a$dist=substr(a$distcue,2,2)
  }

  # re-encode congruency according to last element of rawcond: 0-NA / 1-congruent / 2-incongruent
  a$cong=ifelse(substr(a$rawcond,nchar(a$rawcond),nchar(a$rawcond))=='0','no',
                ifelse(substr(a$rawcond,nchar(a$rawcond),nchar(a$rawcond))=='1','cong',
                       ifelse(substr(a$rawcond,nchar(a$rawcond),nchar(a$rawcond))=='2','incong','??')))

  # re-encode if cue is left or right
  a$sidecue = ifelse(as.numeric(substr(a$rawcond,2,2))<4,'L','R')

  # re-encode if target is left or right
  a$sidetarget = ifelse(a$sidecue=='L' & a$cong=='cong','L',
                        ifelse(a$sidecue=='L' & a$cong=='incong','R',
                               ifelse(a$sidecue=='R' & a$cong=='cong','R',
                                      ifelse(a$sidecue=='R' & a$cong=='incong','L','??'))))

  a$sidetarget[a$rawcond==450] = 'R'; a$sidetarget[a$rawcond==420] = 'L' # for no-cue trials

  # re-encode key press according to touchkey
  a$resp=ifelse(a$touchkey==96,'L',
                ifelse(a$touchkey==45,'R',
                       ifelse(a$touchkey==0,'NA','??')))

  # encode response accuracy
  a$cor=ifelse(a$resp==a$sidetarget,1,0)


  #  encode time variable in the multisensory condition (1, 4, 7; 4 is simultaneous)
  # for small time 3 4 5 / for big time: 1 4 7
  a$time=0; a$time2=0 # filler for other conditions
  if (mode=='_smalltime' | mode=='_bigtime') {
    a$time[a$cue=='AV']=ifelse(is.element(substr(a$rawcond[a$cue=='AV'],3,3),c('1','3')),'1',
                               ifelse(substr(a$rawcond[a$cue=='AV'],3,3)=='4','0',
                                      ifelse(is.element(substr(a$rawcond[a$cue=='AV'],3,3),c('5','7')),'1','??')))
    a$time[a$cue!='AV'] = '0'
    a=a[!is.element(a$time,'??'),] ## there are a few time trials in the small time expe that were actually run in the space condition by mistake, get rid of
  }


  # for small time 3 4 5 / for big time: 1 4 7
  if (mode=='_smalltime' | mode=='_bigtime') {
    a$time2[a$cue=='AV']=ifelse(is.element(substr(a$rawcond[a$cue=='AV'],3,3),c('1','3')),'A1',
                                ifelse(substr(a$rawcond[a$cue=='AV'],3,3)=='4','0',
                                       ifelse(is.element(substr(a$rawcond[a$cue=='AV'],3,3),c('5','7')),'V1','??')))
    a$time2[a$cue!='AV'] = '0'
    a=a[!is.element(a$time2,'??'),] ## there are a few time trials in the small time expe that were actually run in the space condition by mistake, get rid of
  }

  # re-encode perceived cue
  a$percept=ifelse(a$cuekey=='96','no',
                   ifelse(a$cuekey=='49','A',
                          ifelse(a$cuekey=='42','V',
                                 ifelse(a$cuekey=='45','AV','??'))))

  # just encode if something was perceived or not, whatever it is
  a$binpercept_fa=ifelse(a$cuekey=='96','unaware','aware') # watch out, this includes false alarms: like A was presented and subjects report V

  # encode if percept matches prime, and exclude false alarms
  for (c in c('A','V')) {
    a$binpercept_nofa[a$cue==c]=ifelse(a$percept[a$cue==c]=='no','unaware',
                                       ifelse(a$percept[a$cue==c]==c,'aware','fa'))
  }
  a$binpercept_nofa[a$cue=='AV']=ifelse(a$percept[a$cue=='AV']=='AV','aware',
                                        ifelse(a$percept[a$cue=='AV']=='no','unaware','subaware'))

  a$binpercept_nofa[a$cue=='no']=ifelse(a$percept[a$cue=='no']=='no','unaware','aware')

  if (exclude_fa==1) {
    print(paste(round(100*mean(is.element(a$binpercept_nofa,c('fa'))),2),'% of false alarms excluded',sep=''))
    a=a[!is.element(a$binpercept_nofa,c('fa')),]
  }


  if (exclude_fa==0) {
    a$binpercept_nofa[a$binpercept_nofa=='fa']='unaware'
  }

  # how we treat cases in which cue was AV and subjects reported A|V: exclude (0), unaware (1) or aware (2)
  switch(treat_subaware,
         "0" = {
           a=a[!is.element(a$binpercept_nofa,c('subaware')),]
         },
         "1" = {
           a$binpercept_nofa[a$binpercept_nofa=='subaware']='unaware'
         },
         "2" = {
           a$binpercept_nofa[a$binpercept_nofa=='subaware']='aware'
         }
  )

  ## choose on what the analysis is performed later
  a$binpercept=a$binpercept_nofa

  # remove outliers
  if (mode=='') {
    badguys=c('3674','3685','3442',"3311",'3276')
    a<-a[!is.element(a$suj,badguys),]
  }


  ## quick check for number of trials~conditions
  mintrials=ifelse(mode=='',0,50) # define cutoff for minimum number of trials across binpercept+cue+suj (0 for short manip many subjects, 50 otherwise)
  ntrials=aggregate(cor~binpercept+cue+suj,a[a$cue!='no',],length)
  toremove=which(ntrials$cor<mintrials)
  # subjects/conditions to remove: # remove subjects with low number of trials
  ntrials[toremove,]
  for (t in toremove) {
    Fitrials=is.element(a$suj,ntrials$suj[t]) & is.element(a$binpercept,ntrials$binpercept[t]) & is.element(a$cue,ntrials$cue[t])
    a<-a[!Fitrials,]
  }

  # correct for 610ms offset in arduino
  a$rt[a$cue=='no'] = a$rt[a$cue=='no']+0.61

  # specific correction for time:
  if (mode=='_bigtime') {
    a$rt[a$cue=='AV' & a$time!='0'] = a$rt[a$cue=='AV' & a$time!='0']-0.310
  }
  if (mode=='_smalltime') {
    a$rt[a$cue=='AV' & a$time!='0'] = a$rt[a$cue=='AV' & a$time!='0']-0.110
  }

  ## filter RT
  rawrt <- aggregate(rt~binpercept+suj,a,quantile,probs=c(0.025,0.975))
  for (i in unique(a$suj)) {
    for (m in unique(a$binpercept)) {
      a$rtlimsup[a$suj==i & a$binpercept==m ] <- rawrt$rt[,2][rawrt$suj==i&rawrt$binpercept==m ]
      a$rtliminf[a$suj==i & a$binpercept==m ] <- rawrt$rt[,1][rawrt$suj==i&rawrt$binpercept==m ]
    }
  }
  FiRt=a$rt<a$rtlimsup & a$rt>a$rtliminf;

  # percentage of trials we exclude based on RTs:
  print(mode)
  print(paste(100*round(1-mean(FiRt),3),'% of trials excluded based on RTs',sep=''))
  print(paste(100*(1-round(mean(a$cor),3)),'% of trials excluded based on accuracy',sep=''))
  a=a[FiRt,]

  ## transform RTs with 610ms offset
  a$rt=a$rt-0.610

  tmp <- ifelse(mode=='_bigtime' | mode=='_smalltime','a$time',
                ifelse(mode=='_bigspace' | mode=='_smallspace','a$dist',
                       ifelse(mode=='_long' | mode=='','0'))) # decide if we work across distances (space experiment) or times (time experiment)

  a$var = eval(parse(text=tmp))
  a$var=as.factor(a$var)
  art=a[a$cor==1 & is.element(a$cong,c('cong','incong')),] # define cued trials
  abl=a[a$cor==1 & a$cong=='no' & a$binpercept=='unaware',] # define baseline trials (we exclude trials in which subjects hallucinated a cue)

  biga=rbind(biga,art)

}

### post data filtering and aggregation

# subset the dataset to include only unaware conditions
uc <- biga[(biga$binpercept == 'unaware') & (biga$cor == TRUE),]
# excluding trials with spatiotemporal modulation (but I am not sure about this exclusion)
uc <- uc[uc$var ==0,]

# transform to the format of the non-directional analysis
uc_formatted <- uc %>%
  rename(idv = suj, iv = cong, exp = cue, dv = rt) %>% 
  mutate(dv = dv * 1000, exp = paste0('Noel et al._2020_',exp)) %>% 
  dplyr::select(idv, iv, dv, exp)

# compare against Figure 2 (AV: .969 ± .02; A: .988 ± .02; V: .974 ± .01)
uc_formatted %>% 
  group_by(exp, idv,iv) %>% 
  summarise(m = mean(dv)) %>% 
  spread(key = 'iv', value = m) %>% 
  group_by(exp, idv) %>% 
  summarise(rate = cong/incong) %>% 
  group_by(exp) %>% 
  summarise(m = median(rate))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv(uc_formatted, file = 'Noel et al_2020.csv')