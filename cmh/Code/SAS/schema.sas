libname cd '/var/swan/root/bhc/general/playground/cdisc_pilot/dev/data/adam/';

* create base data;
data base;
    set cd.adcibc (rename=(aval=temp_aval));
    where avisitn = 8 and efffl = 'Y' and ittfl = 'Y' and anl01fl = 'Y';
    aval = put(temp_aval, 1.);
RUN;

* schema data sets;
data s1;
    set base;
    where trtp in ('Placebo' 'Xanomeline Low Dose') and agegr1 in ('65-80' '>80');
    rename agegr1 = k trtp = x sex = y;
    schema = 1;
RUN;

data s2;
    set base;
    where RACE ne "AMERICAN INDIAN OR ALASKA NATIVE" and TRTP ne "Xanomeline High Dose";
    rename race = k sex = y trtp = x;
    schema = 2;
RUN;

data s3;
    set base;
    rename race = k sex = y trtp = x;
    schema = 3;
RUN;

data s4;
     set base;
     where TRTP ne "Xanomeline High Dose";
     rename trtp = x sex = y siteid = k;
     schema = 4;
RUN;

data s5;
     set base;
     rename trtp = x sex = y siteid = k;
     schema = 5;
RUN;

data s6;
     set base;
     where TRTP ne "Xanomeline High Dose";
     rename trtp = x sex = y aval = k;
     schema = 6;
RUN;

data s7;
     set base;
     rename trtp = x race = k aval = y;
      schema = 7;
RUN;

data s8;
     set base;
     rename trtp = x AGEGR1 = k aval = y;
      schema = 8;
RUN;

data s9;
     set base;
     rename trtp = x siteid = k aval = y;
      schema = 9;
RUN;

data s10;
     set base;
     rename aval = x TRTP = k AGEGR1 = y;
     schema = 10;
RUN;

* set all data together;
data all_schemas;
    length x $100. y $100. k $100.;
    set s:;
    keep schema x y k;
RUN;

proc sort data = all_schemas;
    by schema;
RUN;


* analysis;
proc freq data = all_schemas;
    by schema;
    tables k * x * y/ cmh;
    output out = all_results cmh;
RUN;

* process the output;
proc transpose data = all_results out = all_long;
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

libname out '/home/ggdzn';
data out.result_part1;
    set result_part1;
run;

data out.result_part2;
    set result_part2;
run;
