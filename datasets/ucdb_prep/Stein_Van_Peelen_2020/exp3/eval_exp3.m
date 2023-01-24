clear all;

%---input
minTrials=5;  %min. trials per condition for analyses dependent on discrimination accuracy

%---output
%resAge, resGender (1 male, 2 female)
%every row is a subject, every column is a condition (orientation x
%presentation time, such that e.g. first column upright face, pres time 1, second column upright face, pres time 2, etc.)
%dprime_discrim
%dprime_localizae
%dprime_detect
%accuLoc_DiscrCor (localization accuracy dependent for trials with correct discrimination)
%accuLoc_DisscrInc (localization accuracy dependent for trials with incorrect discrimination)
%accuDet_DiscrCor (detection accuracy dependent for trials with correct discrimination)
%accuDet_DisscrInc (detection accuracy dependent for trials with incorrect discrimination)
results = table;
for cSub = 1:94
    
    load(['exp3_' num2str(cSub) '.mat']);
    subj = repelem(cSub, size(expDes,1))';
    cur_table = [table(subj , pas, discr_accu, loc_accu) array2table(expDes)];
    results  = [results; cur_table];
    resAge(cSub,1) = age;
    resGender(cSub,1) = gender;
    
    %variable expDes: every row is a trial, every column specifices an
    %experimental variable
    %columns:
    %1-target present(1), target absent(2)
    %2-target left(1), target right(2)
    %3-presentation time (1-5)
    %4-upright(1), inverted(2)
    %5-target exemplar
    
    %loc_accu is localization accuracy (0 or 1)
    %pas is response on perceptual awareness scale (1-4)
    %discr_accu is upright/inverted discrimination (0 or 1)
    
    for cPres=1:5
        
        %Performance
        
        %---Discrimination
        
        %Hits are upright stimuli with upright response
        hit_rate_discrim(cSub,cPres) = (numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==1 & discr_accu==1))) / ...
            (numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==1)));
        
        if hit_rate_discrim(cSub,cPres) == 1; hit_rate_discrim(cSub,cPres) = 1-(1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==1))))); end
        if hit_rate_discrim(cSub,cPres) == 0; hit_rate_discrim(cSub,cPres) = (1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==1))))); end
        
        %FAs are inverted stimuli with upright response
        fa_rate_discrim(cSub,cPres) =(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==2 & discr_accu==0))) / ...
            (numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==2)));
        
        if fa_rate_discrim(cSub,cPres) == 1; fa_rate_discrim(cSub,cPres) = 1-(1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==2))))); end
        if fa_rate_discrim(cSub,cPres) == 0; fa_rate_discrim(cSub,cPres) = (1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==2))))); end
        
        %Dprime discrimination
        dprime_discrim(cSub,cPres) = norminv(hit_rate_discrim(cSub,cPres))-norminv(fa_rate_discrim(cSub,cPres));
        
        
        for cOrient=1:2
            
            %---Localization
            
            %Hits are left stimuli with left response
            hit_rate_localize(cSub,5*cOrient+cPres-5) = (numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & expDes(:,2)==1 & loc_accu==1))) / ...
                (numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & expDes(:,2)==1)));
            
            if hit_rate_localize(cSub,5*cOrient+cPres-5) == 1; hit_rate_localize(cSub,5*cOrient+cPres-5) = 1-(1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==cOrient & expDes(:,2)==1))))); end
            if hit_rate_localize(cSub,5*cOrient+cPres-5) == 0; hit_rate_localize(cSub,5*cOrient+cPres-5) = (1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==cOrient & expDes(:,2)==1))))); end
            
            %FAs are right stimuli with left response
            fa_rate_localize(cSub,5*cOrient+cPres-5)  = (numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & expDes(:,2)==2 & loc_accu==0))) / ...;
                (numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & expDes(:,2)==2)));
            
            if fa_rate_localize(cSub,5*cOrient+cPres-5) == 1; fa_rate_localize(cSub,5*cOrient+cPres-5) = 1-(1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==cOrient & expDes(:,2)==2))))); end
            if fa_rate_localize(cSub,5*cOrient+cPres-5) == 0; fa_rate_localize(cSub,5*cOrient+cPres-5) = (1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==cOrient & expDes(:,2)==2))))); end
            
            %Dprime localization
            dprime_localize(cSub,5*cOrient+cPres-5) = norminv(hit_rate_localize(cSub,5*cOrient+cPres-5))-norminv(fa_rate_localize(cSub,5*cOrient+cPres-5));
            dprime_localize(cSub,5*cOrient+cPres-5) = dprime_localize(cSub,5*cOrient+cPres-5)./sqrt(2);
            
            %Localization accuracy dependent on Discrimination accuracy
            accuLoc_DiscrCor(cSub,5*cOrient+cPres-5) = nanmean(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & discr_accu==1));
            accuLoc_DiscrInc(cSub,5*cOrient+cPres-5) = nanmean(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & discr_accu==0));
            
            if cPres<5 %exlcude longest presentation time
                if numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & discr_accu==1))<minTrials;
                    accuLoc_DiscrCor(cSub,5*cOrient+cPres-5)=NaN; accuLoc_DiscrInc(cSub,5*cOrient+cPres-5)=NaN;
                elseif numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & discr_accu==0))<minTrials;
                    accuLoc_DiscrCor(cSub,5*cOrient+cPres-5)=NaN; accuLoc_DiscrInc(cSub,5*cOrient+cPres-5)=NaN;
                end
            end
            
            %---Detection
            
            %Hit Rate
            hit_rate_detect(cSub,5*cOrient+cPres-5) = (numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & pas>1 & expDes(:,4)==cOrient))) / ...
                (numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==cOrient)));
            
            if hit_rate_detect(cSub,5*cOrient+cPres-5) == 1; hit_rate_detect(cSub,5*cOrient+cPres-5) = 1-(1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==cOrient))))); end
            if hit_rate_detect(cSub,5*cOrient+cPres-5) == 0; hit_rate_detect(cSub,5*cOrient+cPres-5) = (1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cPres & expDes(:,4)==cOrient))))); end
            
            %False Alarm Rate
            fa_rate_detect(cSub,cPres)  = (numel(loc_accu(expDes(:,1)==2 & expDes(:,3)==cPres & pas>1))) / ...
                (numel(loc_accu(expDes(:,1)==2 & expDes(:,3)==cPres)));
            
            if fa_rate_detect(cSub,cPres) == 1; fa_rate_detect(cSub,cPres) = 1-(1/(2*(numel(loc_accu(expDes(:,1)==2 & expDes(:,3)==cPres))))); end
            if fa_rate_detect(cSub,cPres) == 0; fa_rate_detect(cSub,cPres) = (1/(2*(numel(loc_accu(expDes(:,1)==2 & expDes(:,3)==cPres))))); end
            
            %Dprime detection
            dprime_detect(cSub,5*cOrient+cPres-5) = norminv(hit_rate_detect(cSub,5*cOrient+cPres-5))-norminv(fa_rate_detect(cSub,cPres));
            
            %Detection accuracy dependent on Discrimination accuracy
            accuDet_DiscrCor(cSub,5*cOrient+cPres-5) = (numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & pas>1 & discr_accu==1)))/(numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & discr_accu==1)));
            accuDet_DiscrInc(cSub,5*cOrient+cPres-5) = (numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & pas>1 & discr_accu==0)))/(numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & discr_accu==0)));
            
            if cPres<5 %exlcude longest presentation time
                if numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & pas>1 & discr_accu==1))<minTrials;
                    accuDet_DiscrCor(cSub,5*cOrient+cPres-5)=NaN; accuDet_DiscrInc(cSub,5*cOrient+cPres-5)=NaN;
                elseif numel(loc_accu(expDes(:,1)==1 & expDes(:,4)==cOrient & expDes(:,3)==cPres & pas>1 & discr_accu==0))<minTrials;
                    accuDet_DiscrCor(cSub,5*cOrient+cPres-5)=NaN; accuDet_DiscrInc(cSub,5*cOrient+cPres-5)=NaN;
                end
            end
        end
    end
    
    %set subjects with insufficient trials for some pres. times to NaN;
    if any(isnan(accuLoc_DiscrCor(cSub,:))); accuLoc_DiscrCor(cSub,1:10)=NaN; accuLoc_DiscrInc(cSub,1:10)=NaN; end
    if any(isnan(accuLoc_DiscrInc(cSub,:))); accuLoc_DiscrCor(cSub,1:10)=NaN; accuLoc_DiscrInc(cSub,1:10)=NaN; end
    if any(isnan(accuDet_DiscrCor(cSub,:))); accuDet_DiscrCor(cSub,1:10)=NaN; accuDet_DiscrInc(cSub,1:10)=NaN; end
    if any(isnan(accuDet_DiscrInc(cSub,:))); accuDet_DiscrCor(cSub,1:10)=NaN; accuDet_DiscrInc(cSub,1:10)=NaN; end
end
writetable(results, "exp3all.csv")
%kick out subjects with insufficient trials for analyses dependent on
%discrimination accuracy
accuLoc_DiscrCor(any(isnan(accuLoc_DiscrCor),2),:) = [];
accuLoc_DiscrInc(any(isnan(accuLoc_DiscrInc),2),:) = [];
accuDet_DiscrCor(any(isnan(accuDet_DiscrCor),2),:) = [];
accuDet_DiscrInc(any(isnan(accuDet_DiscrInc),2),:) = [];

clear hit* fa* c* gender age pas discr_accu loc_accu expDes cSub cPres cOrient 