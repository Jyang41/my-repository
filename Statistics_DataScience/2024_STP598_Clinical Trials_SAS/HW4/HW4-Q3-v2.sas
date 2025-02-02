* eye.sas *;
* For Example 5: Cox Proportional Hazards Model
A new treatment, Hyalurise, is being tested to facilitate the clearance of vitreous hemorrhage, 
a condition that results in severe vision impairment. Patients were randomly assigned to receive 
a single injection of either Hyalurise (n=83) or saline (n=60) in the affected eye and were
followewd for 1 year. Time to response in weeks is the primary endpoint, where a response 
corresponds to a sufficient clearance of the hemorrhage. The time for patients who discontinued
or completed the trial before achieving a response is condidered censored. 
;

dm "output;clear;log;clear;odsresults;clear"; 
options ls=100 ps=2000 formdlim='*' nodate nonumber nocenter; 


* Load data;
data eye;
 infile 'C:\Users\jyang\OneDrive - Arizona State University\10 Classes_OneDrive\2024_STP598_Clinical Trials\Computing\Topic3\eye.csv' dlm=',' firstobs=2;
 input pat age rsptim trt $ center $ dens inftim cens; 
	/* rsptim = time (wks) from randomization to response */
	/* trt = 1 for Hyalurise, TRT = 0 for Saline */
	/* center = study center (A, B, or C) */
	/* dens = 3 or 4, measure if basekube hemorrgage density for Grade 3 or 4 */
	/* inftim = time (wks) from randomization to onset of
            infection or other complications, whether patients developed certain infectious 
 			complication during this study(missing, if no infection) */
	/* cens: censored data (1=yes)*/

/* Convert trt$ and center$ to numeric variable
	*trt to numeric: 1 for Hyalurise, 0 for Saline;*/
 	* ncenter = 1 (for center A), 2 (for center B), and 0 (for center C);
data eye;
 set eye;
 ntrt = (trt='HYA');
 ncenter= 1*(center='A') + 2*(center='B');
run;

proc print noobs;
run;


* Model (I), Cox Proportional Hazards Model;
ods graphics on;
proc phreg data = eye;
  class ntrt(ref='0') ncenter(ref='0'); 
  model rsptim*cens(1) = ntrt ncenter age / selection=stepwise slentry=0.25 slstay=0.15;
  if (inftim > rsptim) or missing(inftim)
    then infctn = 0; 
  else infctn = 1; 
run;

* (a)
* Create a dataset for covariate values of interest for plotting survival functions;
data covariates;
    length id $50.; /* Create a new variable to hold the labels */
    input ntrt ncenter age;
    if ntrt = 1 then id="Treatment: HYA, Center: A, Age: 64"; /* Treatment group label */
    else id="Control: SAL, Center: A, Age: 64"; /* Control group label */
    datalines;
    1 1 64 /* Treatment group, Center A, Age 64 */
    0 1 64 /* Control group, Center A, Age 64 */
;
run;

ods graphics on;
proc phreg data=eye plots(overlay)=survival;
 	class ntrt(ref='0') ncenter(ref='0'); 
    model rsptim*cens(1) = ntrt ncenter age;
    baseline covariates=covariates out=Pred1 survival=_all_ / id=id;
run;
ods graphics off;

proc print data=Pred1(where=(ntrt=1 and ncenter=1 and age=64));
run;

* Print the estimated survival and 95% confidence intervals at t=40 weeks;
proc print data=Pred1;
    where rsptim=40;
run;


* (b);
ods graphics on;
proc lifetest plots=(s(cl) lls) outs=out data=eye;
 time rsptim*cens(1);
 strata ntrt;
run;
ods graphics off;

ods graphics on;
proc lifetest plots=(s(cl) lls) data=eye;
 time rsptim*cens(1);
 strata ncenter;
run;
ods graphics off;

ods graphics on;
proc lifetest plots=(s(cl) lls) data=eye;
 time rsptim*cens(1);
 strata age(30 to 90 by 20);
run;
ods graphics off;



* The ZPH test also shows no evidence of a strong violation 
  of the proportional hazards assumption.
* This is done by using the ZPH(TRANSFORM=IDENTITY) option;
proc phreg data = eye ZPH(TRANSFORM=IDENTITY);
 class ntrt(ref='0') ncenter(ref='0'); 
 model rsptim*cens(1) = ntrt ncenter age; 
run;


* Model (II): Stratified Cox Proportional Hazards Model;
proc phreg data = eye;
  class ntrt(ref='0')ncenter(ref='0'); 
  model rsptim*cens(1) = ntrt age;
  strata ncenter; /* Stratify the model by 'ncenter' */
run;


*(c)
******* Model (III) Extended Cox Regression;
* We define a new time-dependent variable ntrt2 for
* the interaction between ntrt and a function of time, g(t). 
* Specifically, ntrt2=0 if t <=20, and
* ntrt2=ntrt when t > 20;
* Note also that the RL option of the MODEL statement
*  gives a 95% approximated CI for hazard ratio;

* Define the time-dependent variable ntrt2 in a new data step;
* Extended Cox Regression Model (III);

proc phreg data=eye;
 class ntrt(ref='0');
 if (rsptim <= 20) then ntrt2=0;
 else ntrt2 = ntrt;
 model rsptim*cens(1) = ntrt age ntrt2/ RL; 
 strata ncenter;
run;
ods graphics off;

quit;

