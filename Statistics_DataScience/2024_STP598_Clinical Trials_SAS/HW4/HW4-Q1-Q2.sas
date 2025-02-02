* Q1. The remission times of 42 patients with acute leukemia were reported
from a clinical trial undertaken to assess the ability of a drug called 
6-mercaptopurine (6-MP) to maintain remission. 
Each patient was randomized to receive either 6-MP or placebo, and the 
study was terminated after 1 year. Time (in weeks) in remission for each
patient in the study is listed below, where ‘+’ means a right censored 
data point.;

dm "output;clear;log;clear;odsresults;clear;";
options ls=75 ps=2000 formdlim='*' nodate nonumber nocenter;

* load the data;
data leukemia;
 infile 'C:\Users\jyang\OneDrive - Arizona State University\10 Classes_OneDrive\2024_STP598_Clinical Trials\HW\HW4\leukemia.csv' dlm=',' firstobs=2;
 input pat trt $ wks cens; 
run;


proc print data=leukemia noobs;
 var pat trt wks cens; 
run;

* nonparametric Kaplan-Meier estimation;
proc lifetest plots=(s(cl atrisk) lls ls h) outs=out data=leukemia;
 time wks*cens(1);
 strata trt;
run;

* Q2. The file hsvfull.csv contains the full data set for Example 4 
introduced in class. For this study, the investigator also collected the 
number of episodes of the disease during the 12-month period before 
treatment. Let us denote this covariate by X, and treat it as a continuous 
variable. Consider including the treatment group and X in a Cox 
proportional hazards regression to study the time-to-event response for 
this study.;

dm "output;clear;log;clear;odsresults;clear;";
options ls=75 ps=2000 formdlim='*' nodate nonumber nocenter;

* load the data;
data hsvfull;
 infile 'C:\Users\jyang\OneDrive - Arizona State University\10 Classes_OneDrive\2024_STP598_Clinical Trials\HW\hsvfull.csv' dlm=',' firstobs=2;
 input vac $ pat wks x cens; 
run;

proc print data=hsvfull noobs;
 var pat vac wks x cens; 
run;


* The Cox Model with stepwise selection;
proc phreg data = hsvfull;
  class vac(ref='PBO'); /* 'vac' is the treatment group variable */
  model wks*cens(1) = vac x / ties=exact ; /* Cox regression model*/
run;

