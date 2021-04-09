/*
Example 53.1 Randomized Complete Blocks with Means Comparisons and Contrasts
(View the complete code for this example.)


This example, reported by Stenstrom (1940), analyzes an experiment to investigate how snapdragons grow in various soils. 
To eliminate the effect of local fertility variations, the experiment is run in blocks, with each soil type sampled 
in each block. Since these data are balanced, the Type I and Type III SS are the same and are equal to the traditional ANOVA SS.

First, the standard analysis is shown, followed by an analysis that uses the SOLUTION option and includes 
MEANS and CONTRAST statements. The ORDER=DATA option in the second PROC GLM statement is used so that the ordering of 
coefficients in the CONTRAST statement can correspond to the ordering in the input data. 
The SOLUTION option requests a display of the parameter estimates, which are produced by default only if there are no 
CLASS variables. A MEANS statement is used to request a table of the means with two multiple-comparison procedures requested. 
In experiments with focused treatment questions, CONTRAST statements are preferable to general means comparison methods. 
The following statements produce Output 53.1.1 through Output 53.1.4.
*/

title 'Balanced Data from Randomized Complete Block';
data plants;
   input Type $ @;
   do Block = 1 to 3;
      input StemLength @;
      output;
   end;
   datalines;
Clarion  32.7 32.3 31.5
Clinton  32.1 29.7 29.1
Knox     35.7 35.9 33.1
O'Neill  36.0 34.2 31.2
Compost  31.8 28.0 29.2
Wabash   38.2 37.8 31.9
Webster  32.5 31.1 29.7
;
run;

proc means data=plants;
  var stemlength;
run;

proc glm;
   class Block Type;
   model StemLength = Block Type;
run;
quit;

proc glm order=data;
   class Block Type;
   model StemLength = Block Type / solution;

   /*----------------------------------clrn-cltn-knox-onel-cpst-wbsh-wstr */
   contrast 'Compost vs. others'  Type   -1   -1   -1   -1    6   -1   -1;
   contrast 'River soils vs. non' Type   -1   -1   -1   -1    0    5   -1,
                                  Type   -1    4   -1   -1    0    0   -1;
   contrast 'Glacial vs. drift'   Type   -1    0    1    1    0    0   -1;
   contrast 'Clarion vs. Webster' Type   -1    0    0    0    0    0    1;
   contrast "Knox vs. O'Neill"    Type    0    0    1   -1    0    0    0;
run;

   means Type / waller regwq;
run;
quit;
