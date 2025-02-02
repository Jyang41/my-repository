* HW6-4: Generalized Estimation Equation;

dm "output;clear;log;clear;odsresults;clear";
options ls=75 ps=2000 formdlim='*' nodate nonumber nocenter; 


************************    I. data    **********************************************

* --- data set ---;
data seizure;
 infile 'C:\Users\jyang\OneDrive - Arizona State University\10 Classes_OneDrive\2024_STP598_Clinical Trials\Computing\Topic4\seizures.csv' dlm=',' firstobs=2;
 input id trt age baseline visit1-visit4;
run;

proc print;
title 'HW5-4';
run;

%macro univfmt;
data uniseizure (keep=id trt age baseline visit cnt cell); %* only keep the useful variables in the final data set;
 set seizure;           %* create the data set using the previously created seizure data set;   
 baseline = baseline/4; %* adjust for the number of weeks for baseline;
 %do i = 1 %to 4;       %* create two new variables, namely 'visit' and 'cnt';
  visit = &i.;          %* the ith visit;
  cnt = visit&i.;       %* the response at the ith visit;
  cell = cat(trt,visit);%* Use CAT function to create a variable to indicate the (treatment, visit) combination; 
  output;               %* create a record with the current variables in the data set;
 %end;
run;
%mend;

%univfmt;

* --- data preparation for log-linear model---;
data uniseizure;
 set uniseizure;
 log_age = log(age); /* Log-transform age */
 log_base = log(baseline); *Log-transform baseline, log(x);
 logcnt=log(cnt+1);       *obtain log(y) but avoid log(y=0);
run;

* --- sorting the data ---;
proc sort data=uniseizure;
 by trt visit;
run;


* --- visualization ---;
* Visualize the data to check for linearity between log count and log baseline;
ods graphics / attrpriority=none;
proc sgplot data=uniseizure;
 styleattrs datalinepatterns=(solid);
 loess y=logcnt x=log_base / group=cell; /* Replace cell with appropriate grouping variable if needed */
run;



**************************   II. Build model    ****************************************
* using PROC GENMOD with GEE for repeated measures

* 1) independence structure, dispersion parameter, phi;
proc genmod data=uniseizure;
 class id trt visit;
 model cnt = trt|visit log_age|trt|visit log_base|trt|visit / link=log type3 d=poisson scale=p; /*A|B|C = A B C AB BC CA ABC */
 repeated subject=id(trt) / type=indep within=visit modelse; /* Assuming independence */
 ods select GEEModPEst GEEFitCriteria;
 ods output GEEModPEst=out1 GEEFitCriteria=out2;
run;

* From OUT1, we create a macro variable &phi to store the estimated sqrt(\phi); 
proc sql;
 select estimate into :phi
 from out1
 where parm='Scale';
quit;

* Select an error structure with QIC: store QIC value for independent correlation structure;
proc sql;
 select value into :QICind
 from out2
 where criterion='QIC';
quit;

** 2) the AR(1) structure by keeping the same \phi-hat;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|trt|visit log_base|trt|visit /link=log type3 d=poisson scale=&phi. noscale;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods select GEEFitCriteria; *Only present this result;
  ods output GEEFitCriteria=out2;  *create output data sets to give the QIC;
  title "Cov structure: AR(1)";
run;

proc sql;
 select value into :QICar1
 from out2
 where criterion='QIC';
quit;

** 3) the exchangeable type covariance structure, i.e., compound symmetry;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|trt|visit log_base|trt|visit /link=log type3 d=poisson scale=&phi. noscale;
  repeated subject=id(trt) / type=cs within=visit;
  ods select GEEFitCriteria; 
  ods output GEEFitCriteria=out2; 
  title "Cov structure: Exchangeable";
run;

proc sql;
 select value into :QICcs
 from out2
 where criterion='QIC';
quit;

** 4) The unstrcuted covariance, not working well;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|trt|visit log_base|trt|visit /link=log type3 d=poisson scale=&phi. noscale;
  repeated subject=id(trt) / type=un within=visit;
  title "Cov structure: Unstructured";
run;
title;


**************   III. Compare the three covariance structures  *******************************

**** AR(1) yields a slighly smaller QIC than CS;
data _null_;
 file print;
 put "Indep.: &QICind"; *-1309.0017;
 put "AR(1) : &QICar1"; *-1309.4286	(smallest);
 put "CS    : &QICcs"; *-1309.2051;
run;




****************  IV. backward selection with AR(1)   *******************************

* 1) AR(1);
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|trt|visit log_base|trt|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; 
run;

proc sql;
 select value into :q1
 from out2
 where criterion='QICu';
quit;

* 2) no three-way interaction;
** Note. A|B|C @2 = A B C A*B A*C B*C, i.e. up to 2-way interactions;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|trt|visit log_base|trt|visit @2 /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; 
run;

proc sql;
 select value into :q2
 from out2
 where criterion='QICu';
quit;

* 3) elliminate log_base*visit;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|trt log_age|visit log_base|trt /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; 
run;

proc sql;
 select value into :q3
 from out2
 where criterion='QICu';
quit;

* 4) elliminate log_base*trt;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|trt log_age|visit log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; 
run;

proc sql;
 select value into :q4
 from out2
 where criterion='QICu';
quit;

* 5) elliminate log_age*visit ;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|trt log_base|trt log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; 
run;

proc sql;
 select value into :q5
 from out2
 where criterion='QICu';
quit;

* 6) elliminate log_age*trt ;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|visit log_base|trt log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; 
run;

proc sql;
 select value into :q6
 from out2
 where criterion='QICu';
quit;

* 7) elliminate ..*visit ;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|trt log_base|trt /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; 
run;

proc sql;
 select value into :q7
 from out2
 where criterion='QICu';
quit;

* 8) elliminate ..*trt ;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|visit log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; 
run;

proc sql;
 select value into :q8
 from out2
 where criterion='QICu';
quit;

* 9) elliminate age*trt, base*visit ;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|visit log_base|trt /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; 
run;

proc sql;
 select value into :q9
 from out2
 where criterion='QICu';
quit;

* 10) elliminate age*visit, base*trt ;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age|trt log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; 
run;

proc sql;
 select value into :q10
 from out2
 where criterion='QICu';
quit;

* 11) w/o age*.. base*..;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt|visit log_age log_base /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q11
 from out2
 where criterion='QICu';
quit; 

* 12) w/o trt*visit;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|trt log_age|visit log_base|trt log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q12
 from out2
 where criterion='QICu';
quit;

* 13)w/o trt*visit, w/o base*visit;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|trt log_age|visit log_base|trt /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q13
 from out2
 where criterion='QICu';
quit;

* 14)w/o trt*visit, w/o base*trt;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|trt log_age|visit log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q14
 from out2
 where criterion='QICu';
quit;

* 15)w/o trt*visit, w/o age*visit;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|trt log_base|trt log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q15
 from out2
 where criterion='QICu';
quit;

* 16)w/o trt*visit, w/o age*trt;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|visit log_base|trt log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q16
 from out2
 where criterion='QICu';
quit;

* 17)w/o trt*visit, w/o ..*visit;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|trt log_base|trt /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q17
 from out2
 where criterion='QICu';
quit;

* 18)w/o trt*visit, w/o age*visit w/o base*trt;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|trt log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q18
 from out2
 where criterion='QICu';
quit;

* 19)w/o trt*visit, w/o age*trt w/o base*visit;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|visit log_base|trt /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q19
 from out2
 where criterion='QICu';
quit;

* 20)w/o trt*visit, w/o ..*trt;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|visit log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q20
 from out2
 where criterion='QICu';
quit;

* 21)w/o interactions;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age log_base /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

proc sql;
 select value into :q21
 from out2
 where criterion='QICu';
quit;



****************  V. Compare the QICu for the above models;
data _null_;
 file print;
 put "mean1(full):                   	&q1";
 put "mean2(no 3-int):               	&q2";
 put "mean3(w/o log_base*visit):     	&q3";
 put "mean4(w/o log_base*trt):      	&q4";
 put "mean5(w/o log_age*visit):     	&q5";
 put "mean6(w/o log_age*trt):		 	&q6";
 put "mean7(w/o ..*visit): 				&q7";
 put "mean8(w/o ..*trt):   				&q8";
 put "mean9(w/o age*trt, base*visit):   &q9";
 put "mean10(w/o age*visit, base*trt):	&q10";
 put "mean11(no 2-int):        			&q11";
 put "mean12(w/o trt*visit):          	&q12";
 put "mean13(mean12 w/o base*visit):    &q13";
 put "mean14(mean12 w/o base*trt):      &q14";
 put "mean15(mean12 w/o age*visit): 	&q15";
 put "mean16(mean12 w/o age*trt):    	&q16";
 put "mean17(mean12 w/o ..*visit): 		&q17";
 put "mean18(mean15 w/o base*trt):   	&q18";
 put "mean19(mean16 w/o base*visit):  	&q19";
 put "mean20(mean12 w/o ..*trt):        &q20";
 put "mean21(w/o):     			  		&q21";
run;

****************  VI. analyze the data with Model 16  ***************;

* 16)w/o trt*visit, w/o age*trt;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|visit log_base|trt log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit corrw;
  ods output GEEFitCriteria=out2; *create output data sets;
run;

****************  VII. study trt difference  ***************;
proc sql;
 select avg(log_base) into :mlb
 from uniseizure;
quit;

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proc sql;
 select avg(log_age) into :mlb
 from uniseizure;
quit;
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;


ods trace on;
proc genmod data=uniseizure;
  class id trt visit;
  model cnt= trt visit log_age|visit log_base|trt log_base|visit /link=log type3 d=poisson scale=p;
  repeated subject=id(trt) / type=ar(1) within=visit corrw;
  contrast 'trt-score' trt 1 -1; *treatment effect at logbase=0 with score method;
  contrast 'trt-wald' trt 1 -1 /wald; *treatment effect at logbase=0 with Wald method;
  estimate 'trt-wald' trt 1 -1 ; *estimates of treatment effect at logbase=0 with Wald method;

  contrast 'trt-score at mean' trt 1 -1 log_base*trt &mlb. - &mlb.;  *score test at logbase=&mlb. (the mean of logbase);
  contrast 'trt-wald at mean' trt 1 -1 log_base*trt &mlb. - &mlb./wald;  *at logbase=&mlb. (the mean of logbase);
  estimate 'trt-wald at mean' trt 1 -1 log_base*trt &mlb. - &mlb.;
  ods select Estimates Contrasts;
run;
ods trace off;

* The Wald estimates suggest that the point estimate of mean ratio (mu_0/mu_1)=exp(L'beta) at mean logbase
is 1.4433;  

quit;
