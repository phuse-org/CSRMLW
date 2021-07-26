data azt;
input Race $ AZT $ Symptoms $ Count @@;
datalines;
white azt yes 14 white azt no 93
white no-azt yes 32 white no-azt no 81
black azt yes 11 black azt no 52
black no-azt yes 12 black no-azt no 43 
;
run;

 
proc freq data=azt;
   tables Race*AZT*Symptoms /
           cmh noprint;
   weight Count;
output out = azt_results cmh;
run;

data azt_results;
 set azt_results;
 schema = 0;
run;

* process the output;
proc transpose data = azt_results out = all_long;
    by schema;
run;

* identify what we do want;
data all_long2;
    set all_long;
    where _label_ in ('CMH Nonzero Correlation' 'DF for CMH Nonzero Correlation' 'P-value for CMH Nonzero Correlation'
                      'CMH Row Mean Scores Differ' 'DF for CMH Row Mean Scores Differ' 'P-value for CMH Row Mean Scores Differ'
                      'CMH General Association' 'DF for CMH General Association' 'P-value for CMH General Association'
                      'Mantel-Haenszel Adjusted Odds Ratio' 'Lower CL, MH Adjusted Odds Ratio' 'Upper CL, MH Adjusted Odds Ratio'
                      'Breslow-Day Chi-Square' 'DF for Breslow-Day Chi-Square' 'P-value for Breslow-Day Chi-Square'
                     );
RUN;

* make identifying what we want a bit easier;
data all_long3;
    length type $100.;
    set all_long2;

    if index(_label_, "Nonzero") then hypothesis = "Non-zero Correlation";
    else if index(_label_, "Row Mean") then hypothesis = "Row Mean Scores Differ";
    else if index(_label_, "General Association") then hypothesis = "General Association";

    if index(_label_, "P-value") then  Type = "Prob";
    if index(_label_, "DF") then type = "DF";

    if _label_ = "Mantel-Haenszel Adjusted Odds Ratio" then type = 'MH OR';
    if _label_ = "Lower CL, MH Adjusted Odds Ratio" then type = "MH OR LL";
    if _label_ = "Upper CL, MH Adjusted Odds Ratio" then type = "MH OR UL";

    if _label_ =  'Breslow-Day Chi-Square' then type = "HChisq";
    if _label_ =  'DF for Breslow-Day Chi-Square' then type = "HDF";
    if _label_ = 'P-value for Breslow-Day Chi-Square' then type = "HP-value" ;
RUN;

*Parse out results related to the 3 HAs;
data part1;
    set all_long3 (where=(substr(type,1,1) not in ('H','M')));
    if missing(hypothesis)=0;
    if missing(type)=1 then type="Chisq";
    keep schema hypothesis type col1;
RUN;

proc sort data = part1;
    by schema hypothesis;
RUN;

proc transpose data = part1 out = result_part1 (drop = _name_);
    by schema hypothesis;
    id type;
    var col1;
run;

*Parse out results related to the MH OR, homogeneity test;
data part2;
    set all_long3;
    where substr(type,1,1) in ('M','H');
    keep schema type col1;
RUN;

proc transpose data = part2 out = result_part2 (drop = _name_);
     by schema;
     id type;
     var col1;
run;

libname out 'C:\cmh';
data out.result_part1;
    set result_part1;
run;

data out.result_part2;
    set result_part2;
run;
