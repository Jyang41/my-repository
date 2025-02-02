/* HW 5_1 */
dm "output;clear;log;clear;odsresults;clear"; ** to clear output and log windows*;
options ls=75 ps=2000 formdlim='*' nodate nonumber nocenter; ** useful option for report;

*Q1*;
proc format;
   value responsefmt 1 = 'YES'
                     0 = 'NO';
run;

data study1;
input factor $ treatment $ response frq @@;
format response responsefmt.;
datalines;
+ A 1 40 + B 1 32
+ A 0 10 + B 0 18
- A 1 60 - B 1 48
- A 0 90 - B 0 102
;
run;
proc print data=study1 label noobs;
 label factor='Factor X (+/-)'
	   response='Response';
run;

proc freq data=study1 order=data;
 table factor*treatment*response /cmh(MANTELFLEISS) nocol nopercent;
 weight frq;
run;


***********;
* Q2 *;
proc format;
   value responsefmt 1 = 'YES'
                     0 = 'NO';
run;

data study2;
input factor $ treatment $ response frq @@;
format response responsefmt.;
datalines;
+ A 1 47 + B 1 26
+ A 0 12 + B 0 15
- A 1 56 - B 1 51
- A 0 85 - B 0 108
;
run;
proc print data=study2 label noobs;
 label factor='Factor X (+/-)'
	   response='Response';
run;

proc freq data=study2 order=data;
   tables factor*treatment*response / cmh(MANTELFLEISS) nocol nopercent;
   weight frq;
run;


**
proc freq data=study2 order=data;
   table factor*treatment*response/cmh chisq trend measures cl nocol nopercent;       
   weight frq;
run;


*Q3*;
proc format;
   value responsefmt 1 = 'YES'
                     0 = 'NO';
run;

data study3;
input factor $ treatment $ response frq @@;
format response responsefmt.;
datalines;
+ A 1 33 + B 1 38
+ A 0 8 + B 0 21
- A 1 64 - B 1 45
- A 0 95 - B 0 96
;
run;

proc print data=study3 label noobs;
 label factor='Factor X (+/-)'
	   response='Response';
run;

proc freq data=study3 order=data;
   tables factor*treatment*response / cmh(MANTELFLEISS) nocol nopercent;
   weight frq;
run;

****
proc freq data=study3 order=data;
   table factor*treatment*response/cmh chisq nocol nopercent;       
   weight frq;
run;
