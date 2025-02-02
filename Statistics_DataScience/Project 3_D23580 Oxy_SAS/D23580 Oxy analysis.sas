
dm "output;clear;log;clear;odsresults;clear";
options ls=75 ps=2000 formdlim='*' nodate nonumber nocenter; 

* Import from Excel;
proc import datafile="D:\Dropbox (ASU)\1 Manuscript_Press\#1 Manuscript writing\202201_D23580\zz Oxy data_SAS\Oxy for modeling.xlsx" out=data dbms=xlsx replace;
run;

* Data to Long Format;
proc transpose data=data out=data_long;
  by id trt_FS base trial;  
  var X00 X10 X30 X45 X60; 
run;

data data_long;
  set data_long;
  /* Extract numeric time from the _NAME_ variable */
  time = input(substr(_NAME_, 2), best.);
  rename COL1=count;  /* Rename the variable holding the transposed data to 'count' */
  drop _NAME_ label;  /* Drop unnecessary variables */
run;


*____Normalize;
data normalized_data;
  set data_long;
  if base > 0 then norm_count = count / base;  /* Normalizing count by base */
  else norm_count = .;  /* Handle cases where base is zero or missing */
run;

* Mixed-Effects Model, AR(1) stdurcture, using normalized data;
proc mixed data=normalized_data method=reml;
  class id trt_FS time ;
  model norm_count = trt_FS time trt_FS*time / ddfm=kenwardroger;
  random intercept / subject=id;
  repeated time / subject=id type=AR(1);
  lsmeans trt_FS time / adjust=tukey pdiff=all;
  title "Normalized, AR(1), Mixed-Effects Modeling2 consinder individual differences";
run;


* ANCOVA- FS effect by controlling base, using normalized data;
proc glm data=normalized_data;
  class trt_FS time;
  model norm_count = trt_FS time trt_FS*time;
  means trt_FS time / tukey;  /* Tukey's post-hoc test if needed */
  title "Normalized, ANCOVA analysis-FS effect by controlling base" ;
run;


* graph for exch subject;
proc sgplot data=data_long;
  series x=time y=count / group=id;
  xaxis label='Time (minutes)';
  yaxis label='Count';
  title 'Profile Plot of Counts Over Time for Each Subject';
run;

*Generalized Estimating Equations (GEE)modeling;
proc genmod data=normalized_data;
  class id trt_FS (ref='5') time;
  model norm_count = trt_FS time trt_FS*time / dist=normal ; 
  repeated subject=id / type=ar(1) within=time corrw modelse; /* AR(1) correlation structure */
  title 'GEE modeling';
  run;


*###########################################;
* Data to Long Format;
proc transpose data=data out=data_long2;
  by id trt_FS base trial;  
  var X10 X30 X45 X60; 
run;

data data_long2;
  set data_long2;
  /* Extract numeric time from the _NAME_ variable */
  time = input(substr(_NAME_, 2), best.);
  rename COL1=count;  /* Rename the variable holding the transposed data to 'count' */
  drop _NAME_ label;  /* Drop unnecessary variables */
run;


*____Normalize;
data normalized_data2;
  set data_long2;
  if base > 0 then norm_count = count / base;  /* Normalizing count by base */
  else norm_count = .;  /* Handle cases where base is zero or missing */
run;

* Mixed-Effects Model, AR(1) stdurcture, using normalized data;
proc mixed data=normalized_data2 method=reml;
  class id trt_FS time ;
  model norm_count = trt_FS time trt_FS*time / ddfm=kenwardroger;
  random intercept / subject=id;
  repeated time / subject=id type=AR(1);
  lsmeans trt_FS time / adjust=tukey pdiff=all;
  title "Normalized, AR(1), Mixed-Effects Modeling2 consinder individual differences";
run;


* ANCOVA- FS effect by controlling base, using normalized data;
proc glm data=normalized_data2;
  class trt_FS (ref='1') time;
  model norm_count = trt_FS time trt_FS*time;
  means trt_FS time / tukey;  /* Tukey's post-hoc test if needed */
  title "Normalized, ANCOVA analysis-FS effect by controlling base" ;
run;

