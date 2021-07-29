/*-----------------------------------------------------------------
            S A S   S A M P L E   L I B R A R Y                   
                                                                  
      NAME: GLMEX2                                                
     TITLE: Example 2 for PROC GLM                                
   PRODUCT: STAT                                                  
    SYSTEM: ALL                                                   
      KEYS: Polynomial regression                                 
     PROCS: GLM                                                   
      DATA:                                                       
                                                                  
   SUPPORT: sasrdt                                                
       REF: PROC GLM, EXAMPLE 2.                                  
      MISC:                                                       
-----------------------------------------------------------------*/

libname ex50_2 "C:\Users\User\OneDrive - ManpowerGroup\_ExperisStuff\PHUSE\CSRMLW\CSRMLW\linear_models\Example  50.2 Regression with Mileage Data";

/* Regression with Mileage Data --------------------------------*/
title 'Gasoline Mileage Experiment';
data ex50_2.mileage;
   input mph mpg @@;
   * mph_sq = mph*mph;
   datalines;
20 15.4
30 20.2
40 25.7
50 26.2  50 26.6  50 27.4
55   .
60 24.8
;
run;

ods trace on;

ods output overallanova=ex50_2.overallanova;
ods output parameterestimates=ex50_2.parameterestimates;
ods output predictedvalues=ex50_2.predictedvalues;
ods output fitstatistics=ex50_2.fitstatistics;
proc glm data=ex50_2.mileage;
   model mpg=mph mph*mph / p clm;
run;
quit;

