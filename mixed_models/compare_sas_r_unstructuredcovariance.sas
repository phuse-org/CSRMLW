proc import datafile='/lillyce/qa/vct/common/tfl/test_andy/phuse/output/LSMeans_ANCOVAwithoutRepeatedMeasures_KR_ADLBH_GithubData.csv'
  out=saslsmeans dbms=csv replace;                                                                                                                                        
run;

proc import datafile='/lillyce/qa/vct/common/tfl/test_andy/phuse/output/r/mmrm_emmeans.csv'
  out=rlsmeans dbms=csv replace;                                                                                                                                        
run;

data rlsmeans (rename=(TRTA=TRTP VISIT=AVISIT emmean=r_lsmeans se=r_lsmeans_stderr) drop=df lower_CL upper_CL);
  set rlsmeans;
run;

data saslsmeans (rename=(lsmeans=sas_lsmeans lsmeans_stderr=sas_lsmeans_stderr TRTA=TRTP));
  set saslsmeans (rename=(visit=avisit));
  if covariatestructure='Unstructured';
  avisit=upcase(avisit);
  if avisit='' then avisit='Overall';
run;

proc sort data=rlsmeans;
  by PARAMCD TRTP AVISIT;
run;

proc sort data=saslsmeans;
  by PARAMCD TRTP AVISIT;
run;

data lsmeans;
  merge rlsmeans saslsmeans;
  by PARAMCD TRTP AVISIT;
run;

proc import datafile='/lillyce/qa/vct/common/tfl/test_andy/phuse/output/LSMeansDifferences_ANCOVAwithoutRepeatedMeasures_KR_ADLBH_GithubData.csv'
  out=sasdiffs dbms=csv replace;
  GUESSINGROWS = 350;
run;

proc import datafile='/lillyce/qa/vct/common/tfl/test_andy/phuse/output/r/mmrm_pair_diff.csv'
  out=rdiffs dbms=csv replace;                                                                                                                                        
run;

data rdiffs (drop=contrast dashcount se df rename=(VISIT=AVISIT estimate=r_diffs lower_cl=r_lower upper_cl=r_upper));
  set rdiffs;
  length trtp reftrtp $ 40;
  dashcount=index(left(contrast),'-');
  if dashcount gt 1 then do;
    trtp=substr(left(contrast),1,dashcount-1);
    reftrtp=substr(left(contrast),dashcount+2);
  end;
run;

data rdiffs (drop=temptrt);
  set rdiffs;
  length temptrt $ 40;
  if not (reftrtp='Xanomeline High Dose' and trtp='Xanomeline Low Dose') then do;
    r_diffs=-1*r_diffs;
    r_lower=-1*r_lower;
    r_upper=-1*r_upper;
    temptrt=trtp;
    trtp=reftrtp;
    reftrtp=temptrt;
  end;
run;

data sasdiffs (drop=pval_interaction rename=(lsmeans_difference=sas_diffs lowerci95=sas_lower upperci95=sas_upper));
  set sasdiffs (rename=(visit=avisit trta=trtp reftrta=reftrtp));
  if covariatestructure='Unstructured';
  avisit=upcase(avisit);
*  if avisit='' then avisit='Overall';
run;

proc sort data=rdiffs;
  by PARAMCD TRTP reftrtp AVISIT;
run;

proc sort data=sasdiffs;
  by PARAMCD TRTP reftrtp AVISIT;
run;

data diffs;
  merge rdiffs sasdiffs;
  by PARAMCD TRTP reftrtp AVISIT;
run;

data diffs;
  set diffs;
  rawdiffs=r_diffs-sas_diffs;
  if sas_diffs not in (., 0) then pctdiffs=(r_diffs-sas_diffs)/sas_diffs;
  rawlower=r_lower-sas_lower;
  if sas_lower not in (., 0) then pctlower=(r_lower-sas_lower)/sas_lower;
  rawupper=r_upper-sas_upper;
  if sas_upper not in (., 0) then pctupper=(r_upper-sas_upper)/sas_upper;
run;

data lsmeans;
  set lsmeans;
  rawlsmeans=r_lsmeans-sas_lsmeans;
  if sas_lsmeans not in (., 0) then pctlsmeans=(r_lsmeans-sas_lsmeans)/sas_lsmeans;
  rawstderr=r_lsmeans_stderr-sas_lsmeans_stderr;
  if sas_lsmeans_stderr not in (., 0) then pctstderr=(r_lsmeans_stderr-sas_lsmeans_stderr)/sas_lsmeans_stderr;
run;

proc sort data=diffs;
  by paramcd param;
run;

proc sort data=lsmeans;
  by paramcd param;
run;

proc univariate data=diffs noprint;
  by paramcd param;
  var rawdiffs pctdiffs rawlower pctlower rawupper pctupper;
  output out=statsdiffs min=rawdiffsmin pctdiffsmin rawlowermin pctlowermin rawuppermin pctuppermin
                   max=rawdiffsmax pctdiffsmax rawlowermax pctlowermax rawuppermax pctuppermax
				   mean=rawdiffsmean pctdiffsmean rawlowermean pctlowermean rawuppermean pctuppermean;
run;

proc univariate data=lsmeans noprint;
  by paramcd param;
  var rawlsmeans pctlsmeans rawstderr pctstderr;
  output out=statslsmeans min=rawlsmeansmin pctlsmeansmin rawstderrmin pctstderrmin
                   max=rawlsmeansmax pctlsmeansmax rawstderrmax pctstderrmax
				   mean=rawlsmeansmean pctlsmeansmean rawstderrmean pctstderrmean
run;



proc export data=statsdiffs
   outfile="/lillyce/qa/vct/common/tfl/test_andy/phuse/output/Diffs_comparison.csv" 
   dbms=csv replace;
run;

proc export data=statslsmeans
   outfile="/lillyce/qa/vct/common/tfl/test_andy/phuse/output/LSMeans_comparison.csv" 
   dbms=csv replace;
run;


ods _all_ close;
ods listing;
proc print data=statsdiffs;
run;
proc print data=statslsmeans;
run;

proc print data=diffs;
  var paramcd param reftrtpn reftrtp trtpn trtp avisitn avisit r_diffs sas_diffs r_lower sas_lower r_upper sas_upper
    rawdiffs pctdiffs rawlower pctlower rawupper pctupper;
  where paramcd='BASO' and AVISIT='WEEK 2';
run;

proc print data=lsmeans;
  var paramcd param trtp avisitn avisit r_lsmeans sas_lsmeans r_lsmeans_stderr sas_lsmeans_stderr effect
    rawlsmeans pctlsmeans rawstderr pctstderr;
  where sas_lsmeans_stderr ne '';
run;
