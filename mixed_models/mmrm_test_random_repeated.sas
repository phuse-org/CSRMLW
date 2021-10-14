libname adam "/lillyce/qa/vct/common/tfl/test_andy/phuse/data/adam_github" access=readonly;
libname sdtm "/lillyce/qa/vct/common/tfl/test_andy/phuse/data/sdtm_github" access=readonly;

%macro ddfm(ddfmvalue=);

/* ANCOVA with Repeated Measures */
proc sort data=adam.adlbh out=adlbh;
  by paramcd param;
run;
ods _all_ close;
/*
ods _all_ close;
ods listing;
proc freq data=adlbh;
  where chg ne .;
  tables paramcd*param/list;
run;
*/

proc sort data=adlbh out=startdsn (keep=paramcd param) nodupkey;
  by paramcd param;
run;

data lsmeans;
run;
data diffs;
run;

%macro covarstructure(covar=, covar_label=, dsname=);

data lsmeans_&dsname;
  set startdsn;
  length comment $ 60;
  comment='Did not converge or Unable to make Hessian positive definite';
  estimate=.;
  stderr=.;
run;

data diffs_&dsname;
  set startdsn;
  length comment $ 60;
  comment='Did not converge or Unable to make Hessian positive definite';
  _visitnum=.;
  probt=.;
  estimate=.;
  lower=.;
  upper=.;
  _trtan=.;
run;

ods output lsmeans=lsmeans_&dsname (keep=paramcd param effect trtan visitnum estimate stderr); 
ods output diffs=diffs_&dsname (keep=paramcd param trtan _trtan probt visitnum _visitnum estimate lower upper 
                                  where=(visitnum=_visitnum and not missing(visitnum)));  

%if "&dsname"="un" %then %do;
  proc mixed data=adlbh;
     where base ne . and visitnum not in (., 99);
     class usubjid trtan(ref="0") visitnum;
     by paramcd param;
     model chg=base trtan visitnum  trtan*visitnum / solution cl alpha=0.05 ddfm=&ddfmvalue;
	 random visitnum/subject=usubjid type=&covar;
     repeated visitnum/subject=usubjid type=&covar;
     lsmeans trtan * visitnum / diff cl slice=visitnum;
     lsmeans trtan / diff cl;
  run;
%end;
%else %do;
  proc mixed data=adlbh;
     where base ne . and visitnum not in (., 99);
     class usubjid trtan(ref="0") visitnum;
     by paramcd param;
     model chg=base trtan visitnum  trtan*visitnum / solution cl alpha=0.05 ddfm=&ddfmvalue;
	 random visitnum/subject=usubjid type=&covar;
     repeated visitnum/subject=usubjid type=&covar;
     lsmeans trtan * visitnum / diff cl slice=visitnum;
     lsmeans trtan / diff cl;
  run;
%end;

data lsmeans_&dsname (rename=(estimate=lsmeans stderr=lsmeans_stderr));
  set lsmeans_&dsname;
  length covariatestructure $ 40;
  covariatestructure="&covar_label";
run;

data diffs_&dsname (drop=_visitnum rename=(probt=pval_interaction estimate=lsmeans_difference lower=lowerci95 upper=upperci95 _trtan=reftrtan));
  set diffs_&dsname;
  length covariatestructure $ 40;
  covariatestructure="&covar_label";
run;

data lsmeans;
  set lsmeans lsmeans_&dsname;
run;

data diffs;
  set diffs diffs_&dsname;
run;

%mend;

%covarstructure(covar=un, covar_label=Unstructured, dsname=un);
%covarstructure(covar=cs, covar_label=Compound Symmetry, dsname=cs);
%covarstructure(covar=ARH(1), covar_label=Heterogeneous First Order Autoregressive, dsname=arh1);
%covarstructure(covar=CSH, covar_label=Heterogeneous Compound Symmetry, dsname=csh);
%covarstructure(covar=TOEP, covar_label=Toeplitz, dsname=toep);
%covarstructure(covar=AR(1), covar_label=First Order Autoregressive, dsname=ar1);

proc sort data=adlbh out=trtdset (keep=trtan trta) nodupkey;
  by trtan trta;
run;

proc sort data=adlbh out=visitdset (keep=visitnum visit) nodupkey;
  by visitnum visit;
run;

proc sort data=diffs;
  by trtan;
run;

data diffs;
  merge trtdset diffs (in=a);
  by trtan;
  if a;
run;

proc sort data=diffs;
  by visitnum;
run;

data diffs;
  merge visitdset diffs (in=a);
  by visitnum;
  if a;
run;

proc sort data=lsmeans;
  by trtan;
run;

data lsmeans;
  merge trtdset lsmeans (in=a);
  by trtan;
  if a;
run;

proc sort data=lsmeans;
  by visitnum;
run;

data lsmeans;
  merge visitdset lsmeans (in=a);
  by visitnum;
  if a;
run;

proc sort data=diffs;
  by reftrtan;
run;

data diffs;
  merge trtdset (rename=(trtan=reftrtan trta=reftrta)) diffs (in=a);
  by reftrtan;
  if a;
run;

proc sort data=diffs;
  by paramcd covariatestructure visitnum trtan;
  where covariatestructure ne '';
run;

proc sort data=lsmeans;
  by paramcd covariatestructure visitnum trtan;
  where covariatestructure ne '';
run;

proc export data=diffs
   outfile="/lillyce/qa/vct/common/tfl/test_andy/phuse/output/LSMeansDifferences_ANCOVA_random_repeated_&ddfmvalue._ADLBH_GithubData.csv" 
   dbms=csv replace;
run;

proc export data=lsmeans
   outfile="/lillyce/qa/vct/common/tfl/test_andy/phuse/output/LSMeans_ANCOVA_randome_repeated_&ddfmvalue._ADLBH_GithubData.csv" 
   dbms=csv replace;
run;

%mend;

%ddfm(ddfmvalue=KR);
%ddfm(ddfmvalue=SATTERTHWAITE);
%ddfm(ddfmvalue=BETWITHIN);
%ddfm(ddfmvalue=CONTAIN);

ods listing;
proc print data=lsmeans;
  where paramcd='WBC';
run;
proc print data=diffs;
  where paramcd='WBC';
run;
