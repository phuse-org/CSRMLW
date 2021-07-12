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

/* Regression with Mileage Data --------------------------------*/
title 'Gasoline Mileage Experiment';
data mileage;
   input mph mpg @@;
   datalines;
20 15.4
30 20.2
40 25.7
50 26.2  50 26.6  50 27.4
55   .
60 24.8
;
proc glm;
   model mpg=mph mph*mph / p clm;
run;
quit;
