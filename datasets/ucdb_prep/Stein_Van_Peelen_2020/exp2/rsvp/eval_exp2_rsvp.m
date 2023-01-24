clear all;

for cSub = 1:40
    
    load(['exp2_rsvp_' num2str(cSub) '.mat']);
    
    resAge(cSub,1) = age;
    resGender(cSub,1) = gender;
    
    %variable expDes: every row is a trial, every column specifices an
    %experimental variable
    %columns:
    %1-target left(1), target right(2)
    %2-presentation time (1-4)
    %3-t1 circle/diamond (used in RSVP only)
    %4-target exemplar
    %5-face upright(1), inverted(2)
    
    %loc_accu is localization accuracy (0 or 1)
    %t1_accu is T1 identification accuracy (0 or 1)
    
    loc_accu(t1_accu==0)=NaN; % kick out trials with incorrect t1 response
    
    % Output: res_loc_accu
    % Ordered such that every row a subject, first column upright face,
    % shortest pres time, second column upright face, medium pres time,
    % sixth column inverted face, longest pres time
    for cOrient = 1:2
        for cPres = 1:3
            res_loc_accu(cSub,3*cOrient+cPres-3) = nanmean(loc_accu(expDes(:,5)==cOrient & expDes(:,2)==cPres)); 
        end
    end

end
