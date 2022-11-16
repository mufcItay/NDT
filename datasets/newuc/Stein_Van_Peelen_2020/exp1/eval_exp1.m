clear all;

%requires Palamedes toolbox (http://www.palamedestoolbox.org) for staircase
%reversal averaging PAL_AMUD_analyzeUD

%output is staircase thresholds as res_sc_eyeavg, see below
%different paradigms in different cells:
exp = {'dichoptic'; 'monoptic'; 'noise'; 'cfs'};

for e = 1:length(exp)

    for cSub = 1:24
        
        load([exp{e} num2str(cSub) '.mat'])
        
        resAge(cSub,1) = age;
        resGender(cSub,1) = gender;
        
        %variable expDes: every row is a trial, every column specifices an
        %experimental variable
        %columns:
        %1-target left(1), target right(2)
        %2-target contrast high(1), low(2)
        %3-face upright(1), inverted(2)
        %4-target position (1 top position, 2 bottom position, 3 left positon, 4  right position)
        %7-8 staircases running (eye x contrast x orientation) 
        
        %Calculate mean of last 5 reversals for all 8 staircases
        for c = 1:8
            res_sc_raw{e}(cSub,c) = PAL_AMUD_analyzeUD(sc{c},'reversals',5);
        end
        
        %Average of the two eyes, such that
        %1st column: high contrast, upright
        %2nd column: high contrast, inverted
        %3rd column: low contrast, upright
        %4th column: low contrast, inverted
        cells = [1,5; 2,6; 3,7; 4,8];  
        for c = 1:length(cells)
            res_sc_eyeavg{e}(cSub,c) = mean([res_sc_raw{e}(cSub,cells(c,1)) res_sc_raw{e}(cSub,cells(c,2))]);
        end
        
    end
    
end
        
  