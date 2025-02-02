/* HW2 */

**********************;
* Randomization list *;
**********************;
*-------------------------;
* 1. Simple randomization.
   Use RANDUNI() function in a data-step to generate a list of 
   60 random integers for a study with 2 treatment groups.
   Here, RANDUNI() generates a random number from Unif(0,1).
   I also use date() function to generate a random seed.
   This is done by creating the macro variable, &seed., in
   the following %LET statement.; 


%LET seed=%sysfunc(date()); * Create a macro variable to use date() as seed;

data randomlist; * Create a data set called 'randomlist';
 do i = 1 to 20; * loop through 60 units;
  trt = 1 + floor(2*ranuni(&seed.)); * unif(0,1) pseudorandom number with random seed;
  output; * save the current record to the data set;
          * All the variables, including seed, i, and trt, will be 
            created in the data set; 
 end; *end the loop;
run; * close the data-step;


* Display the data set in the output window (with label);
proc print data=randomlist label;
 label i='unit' trt ='Group'; *assign labels to variables;
run;

* Use PROC FREQ to count the size of each treatment group;
proc freq data=randomlist;
 table trt;
run;

*--------------------------------
* 2. permuted block randomization
   We now generate a randomization list with permuted blocks.
   Each block has s=2 replicates of each of the t=2 treatments.
   The block size is b=2*2. For a list of n=20 patients,
   we thus need n/b = 20/(2*2)=5 blocks. 
   These parameters are used in PROC PLAN below.
   We again use the macro variable &seed. as random seed. 
   In the FACTORS statement, we create two factors, called blocks and blksize.
   As noted above, we need 5 ordered blocks (thus 'blocks=5 ordered').
   With 'blksize=6 random', we include a random permutation of {1, 2, ..., 6} in each block.
   The list is then stored in the data set perblk.
   see also, https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.3/statug/statug_plan_syntax01.htm ;
     
proc plan seed=&seed.; *use the previously generated macro variable &seed.;
 factors blocks=5 ordered 
         blksize=6 random; 
 output out=perblk;
run;

* We see that each block includes a random permutation of integers 1 to 6. 
  In the data step below, we modify this perblk dataset to assign {1,3,5} to group 1,
  and {2,4,6} to group 2. This is done by using the modulus operator, but 
  note that mod(2,2)=mod(4,2)=mod(6,2)=0, and mod(1,2)=mod(3,2)=mod(5,2)=1.
  Hence, even numbers are assigned to group 2 and odd numbers to group 1.;
           
data perblk;
 set perblk; * set back the perblk data set;
 group=mod(blksize,2); 
 if group=0 then group=2; 
run;

* We now have the randomization list we need;
proc print data=perblk;
run;

* copmpare the 2 groups;
proc freq data=perblk;
 table group;
run;

 
/* HW2 */

*************************************************
* Treatment Comparison with continuous response *
*************************************************

* whether treatment A (new drug) is effective in lowering DBP as compared to B (placebo)?
* DBP1: baseline
* DBP2 - DBP5: monthly measured
* d = (DBP5 - DBP1)
1) Study the treatment effects with parametric and nonparametric approaches
2) Compare two approaches and discuss
3) Hypothesis test
4) point and interval estimates of the treatment difference;


* Set the environment and clear previous outputs;
dm "output;clear;log;clear;odsresults;clear;";
options ls=100 ps=100 formdlim='*' nodate nonumber nocenter;


* Define a fileref for the CSV file location;
filename dbp 'C:\Users\jyang\OneDrive - Arizona State University\10 Classes_OneDrive\2024_STP598_Clinical Trials\HW\HW2\DBP.csv';



* Import the DBP.csv data;
data dbp_data;
   infile dbp delimiter= ',' firstobs=2 truncover;
   input Subject TRT $ DBP1-DBP5 Age Sex $;
run;
proc print data=dbp_data(obs=6);
run;

* Calculate the primary endpoint d;
data dbp_data;
   set dbp_data;
   d = DBP5 - DBP1; /* Calculate the difference between baseline and month 4 */
run;


*--- summary statistics & one-sample (and paired) t test;
proc means mean std n t probt data=dbp_data; 
 by TRT;
 var dbp1 dbp5 d;
run;

* paired t-test ;
ods trace on;
proc ttest h0=0 data=fev;
 by TRT;
 paired dbp5*dbp1;
 ods output Statistics=stats  ConfLimits=CIs TTests=ttest;
run;
ods trace off;


* Parametric method: one-sample t-test for each treatment group.
	About 'within each group'.
	whether the mean change in DBP within each treatment group is 
	significantly different from zero.
	This test is done separately for each treatment group.;

proc ttest h0=0 data=dbp_data;
 by TRT;
 var d;
run;


* Parametric method: two-sample t-test
	About 'between the groups'
	whether there is a significant difference in the mean changes in DBP 
	between the two treatment groups;

proc ttest h0=0 data=dbp_data;
   class TRT;
   var d;
   ods select TTests;
run;



* Nonparametric method - Wilcoxon rank-sum test;

proc npar1way data=dbp_data wilcoxon HL(REFCLASS='B') FP(REFCLASS='B') correct=no; 
   class TRT;
   var d;
   exact wilcoxon; *we may request for exact p-value for the WMW test;
run;


%include nptsd;
%NPTSD(
 data=dbp_data,
 var=d,
 group=TRT,
 alpha=0.05,
 exact=yes);

**************************
**************************


   
