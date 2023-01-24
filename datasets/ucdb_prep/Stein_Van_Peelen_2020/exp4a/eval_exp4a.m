clear all;

%---input
minTrials=5;  %min. trials per condition for analyses dependent on discrimination accuracy

%---output
%resAge, resGender (1 male, 2 female)
%every row is a subject, every column is a condition (cue validity x
%presentation time, such that e.g. first column valid cue face, pres time 1, second column valid cue, pres time 2, etc.)
%dprime_discrim
%dprime_localize
%accuLoc_DiscrCor (localization accuracy dependent for trials with correct discrimination)
%accuLoc_DisscrInc (localization accuracy dependent for trials with incorrect discrimination)

for cSub = 1:45
    
    load(['exp4a_' num2str(cSub) '.mat']);
    
    resAge(cSub,1) = age;
    resGender(cSub,1) = gender;
    
    %variable expDes: every row is a trial, every column specifices an
    %experimental variable
    %columns:
    %1-target left(1), target right(2)
    %2-presentation time (1-4)
    %3-valid(1), invalid(2)
    %4-target category car, cat, chair, cup, guitar, hammer, person, shoe
    %5-target exemplar
    
    %loc_accu is localization accuracy (0 or 1)
    %discr_accu is valid/invalid discrimination (0 or 1)
    
    for cPres = 1:4
        
        %Performance
        
        %---Discrimination
        
        %Hits are valid trials with valid response
        hit_rate_discrim(cSub,cPres) = (numel(loc_accu(expDes(:,3)==1 & expDes(:,2)==cPres & discr_accu==1))) / ...
            (numel(loc_accu(expDes(:,3)==1 & expDes(:,2)==cPres)));
        
        if hit_rate_discrim(cSub,cPres) == 1; hit_rate_discrim(cSub,cPres) = 1-(1/(2*(numel(loc_accu(expDes(:,3)==1 & expDes(:,2)==cPres))))); end
        if hit_rate_discrim(cSub,cPres) == 0; hit_rate_discrim(cSub,cPres) = (1/(2*(numel(loc_accu(expDes(:,3)==1 & expDes(:,2)==cPres))))); end
        
        %FAs are invalid trials with valid response
        fa_rate_discrim(cSub,cPres) =(numel(loc_accu(expDes(:,3)==2 & expDes(:,2)==cPres & discr_accu==0))) / ...
            (numel(loc_accu(expDes(:,3)==2 & expDes(:,2)==cPres)));
        
        if fa_rate_discrim(cSub,cPres) == 1; fa_rate_discrim(cSub,cPres) = 1-(1/(2*(numel(loc_accu(expDes(:,3)==2 & expDes(:,2)==cPres))))); end
        if fa_rate_discrim(cSub,cPres) == 0; fa_rate_discrim(cSub,cPres) = (1/(2*(numel(loc_accu(expDes(:,3)==2 & expDes(:,2)==cPres ))))); end
        
        %Dprime discrimination
        dprime_discrim(cSub,cPres) = norminv(hit_rate_discrim(cSub,cPres))-norminv(fa_rate_discrim(cSub,cPres));
        
        
        %---Localization
        
        for cCue = 1:2
            
            % Hits are left stimuli with left response
            hit_rate_localize(cSub,4*cCue+cPres-4) = (numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cCue & expDes(:,2)==cPres & loc_accu==1))) / ...
                (numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cCue & expDes(:,2)==cPres)));
            
            if hit_rate_localize(cSub,4*cCue+cPres-4) == 1; hit_rate_localize(cSub,4*cCue+cPres-4) = 1-(1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cCue & expDes(:,2)==cPres))))); end
            if hit_rate_localize(cSub,4*cCue+cPres-4) == 0; hit_rate_localize(cSub,4*cCue+cPres-4) = (1/(2*(numel(loc_accu(expDes(:,1)==1 & expDes(:,3)==cCue & expDes(:,2)==cPres))))); end
            
            % FAs are right stimuli with left response
            fa_rate_localize(cSub,4*cCue+cPres-4)  = (numel(loc_accu(expDes(:,1)==2 & expDes(:,3)==cCue & expDes(:,2)==cPres & loc_accu==0))) / ...;
                (numel(loc_accu(expDes(:,1)==2 & expDes(:,3)==cCue & expDes(:,2)==cPres)));
            
            if fa_rate_localize(cSub,4*cCue+cPres-4) == 1; fa_rate_localize(cSub,4*cCue+cPres-4) = 1-(1/(2*(numel(loc_accu(expDes(:,1)==2 & expDes(:,3)==cCue & expDes(:,2)==cPres))))); end
            if fa_rate_localize(cSub,4*cCue+cPres-4) == 0; fa_rate_localize(cSub,4*cCue+cPres-4) = (1/(2*(numel(loc_accu(expDes(:,1)==2 & expDes(:,3)==cCue & expDes(:,2)==cPres))))); end
            
            % Dprime localization
            dprime_localize(cSub,4*cCue+cPres-4) = norminv(hit_rate_localize(cSub,4*cCue+cPres-4))-norminv(fa_rate_localize(cSub,4*cCue+cPres-4));
            dprime_localize(cSub,4*cCue+cPres-4) =  dprime_localize(cSub,4*cCue+cPres-4)./sqrt(2);
            
            accuLoc_DiscrCor(cSub,4*cCue+cPres-4) = nanmean(loc_accu(expDes(:,3)==cCue & expDes(:,2)==cPres & discr_accu==1));
            accuLoc_DiscrInc(cSub,4*cCue+cPres-4) = nanmean(loc_accu(expDes(:,3)==cCue & expDes(:,2)==cPres & discr_accu==0));
            
            if cPres<4 %exlcude longest presentation time
                if numel(loc_accu(expDes(:,3)==cCue & expDes(:,2)==cPres & discr_accu==1))<minTrials;
                    accuLoc_DiscrCor(cSub,4*cCue+cPres-4)=NaN; accuLoc_DiscrInc(cSub,4*cCue+cPres-4)=NaN;
                elseif numel(loc_accu(expDes(:,3)==cCue & expDes(:,2)==cPres & discr_accu==0))<minTrials;
                    accuLoc_DiscrCor(cSub,4*cCue+cPres-4)=NaN; accuLoc_DiscrInc(cSub,4*cCue+cPres-4)=NaN;
                end
            end
            
        end
        
        if any(isnan(accuLoc_DiscrCor(cSub,:))); accuLoc_DiscrCor(cSub,1:8)=NaN; accuLoc_DiscrInc(cSub,1:10)=NaN; end
        if any(isnan(accuLoc_DiscrInc(cSub,:))); accuLoc_DiscrCor(cSub,1:8)=NaN; accuLoc_DiscrInc(cSub,1:10)=NaN; end
        
    end
end

%kick out subjects with insufficient trials for analyses dependent on
%discrimination accuracy
accuLoc_DiscrCor(any(isnan(accuLoc_DiscrCor),2),:) = [];
accuLoc_DiscrInc(any(isnan(accuLoc_DiscrInc),2),:) = [];

clear hit* fa* c* gender age discr_accu loc_accu expDes cSub cPres cCue