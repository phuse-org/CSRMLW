* Reference: https://github.com/phuse-org/CSRMLW/tree/main/cmh;
* Schema: ;
libname cdscplt "C:\Users\yanga\OneDrive - Merck Sharp & Dohme, Corp\documents\Yanga\PhUSE\A-Clinical Statistical Reporting in a Multilingual World\CMHProject\CDISC_pilot_replication_SAS\dataanalysis";

* 1. Schema 1:;

proc freq data=cdscplt.adcibc;
  tables  agegr1*trtp*sex/cmh;
  where AVISITN = 8 & EFFFL = 'Y' & ITTFL = 'Y' & ANL01FL='Y'
  and trtp in ('Placebo'  'Xanomeline Low Dose') and agegr1 in ('65-80' '>80');
  output out= cmhtest1 cmh;
run;

data cmhcor1 cmhrms1 cmhga1;
  set cmhtest1;
  schema = 1;
  hypothesis = 'Nonzero Correlation';
  chisq = _cmhcor_;
  df = df_cmhco;
  prob = p_cmhcor;
  output cmhcor1;
  hypothesis = 'General Association';
  chisq = _CMHGA_;
  df = df_cmhga;
  prob = p_cmhga;
  output cmhga1;  
  hypothesis = 'Row Mean Score';
  chisq = _CMHRMS_;
  df = df_cmhrm;
  prob = p_cmhrms;
  output cmhrms1; 
run;

***  end of scheme 1 ***;
* Scheme 2 :
X) TRTP --> filtered for Placebo(n = 77) and Low Dose(n = 81) 
Y) SEX (92f : 66m) 
K) RACE--> filtered for White (144) and Black/African American (14) ;

proc freq data=cdscplt.adcibc;
  tables  race*trtp*sex/cmh;
  where AVISITN = 8 & EFFFL = 'Y' & ITTFL = 'Y' & ANL01FL='Y'
  and trtp in ('Placebo'  'Xanomeline Low Dose')
  and race in ('WHITE', 'BLACK OR AFRICAN AMERICAN');
  output out= cmhtest2 cmh;
run;

data cmhcor2 cmhrms2 cmhga2;
  set cmhtest2;
  schema = 2;
  hypothesis = 'Nonzero Correlation';
  chisq = _cmhcor_;
  df = df_cmhco;
  prob = p_cmhcor;
  output cmhcor2; 
  hypothesis = 'Row Mean Score';
  chisq = _CMHRMS_;
  df = df_cmhrm;
  prob = p_cmhrms;
  output cmhrms2; 
  hypothesis = 'General Association';
  chisq = _CMHGA_;
  df = df_cmhga;
  prob = p_cmhga;
  output cmhga2; 
run;

data cmhtest;
  set cmhcor1 cmhrms1 cmhga1 cmhcor2 cmhrms2 cmhga2;
  keep schema hypothesis chisq df prob;
run;

proc print data = cmhtest;
run;

proc export data= work.cmhtest 
            outfile= "C:\Users\yanga\OneDrive - Merck Sharp & Dohme, Cor
p\documents\Yanga\PhUSE\A-Clinical Statistical Reporting in a Multilingu
al World\CMHProject\output\cmhtestsas.xls" 
            DBMS=EXCEL REPLACE;
     SHEET="SAS_Results_1"; 
run;
