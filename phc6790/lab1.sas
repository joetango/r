/* 
Title: Lab 1
Description: First lab/group assignment

Name: Joe Dickerson
Due Date: 09/21/2025
*/

*1. ;
proc power;
twosamplefreq
oddsratio = 2 3 4
refproportion = 0.3
power = 0.9
groupweights = (1 1)
ntotal= .;
run;


ods output Output=table;
proc power;
twosamplefreq
oddsratio = 2 3 4
refproportion = 0.3
power = 0.9
groupweights = (2 1)
ntotal = .;
run;

proc print data=table; 
run;


*2a. ;
proc power;
twosamplefreq
oddsratio = 2.5
power = 0.85
refproportion = 0.25
groupweights = (1 1)
alpha = 0.05
ntotal = .;
run;

*2b. ;
proc power;
twosamplefreq
oddsratio = 2.5
power = 0.85
refproportion = 0.25
groupweights = (3 1)
alpha = 0.05
ntotal = .;
run;

*2c. ;
proc power;
twosamplefreq
oddsratio = 2 to 6 by 1
refproportion = 0.25
groupweights = (1 1)
power = .
ntotal = 194;
plot x = effect;
run;

*3a. / 3b. ;
data dataset;
input mh hd count;
datalines;
1 1 34
1 0 20
0 1 65
0 0 82 
;
run;

proc freq data = dataset;
table mh*hd /chisq;
weight count;
run;

*3c. ;
proc freq data = dataset;
table mh*hd / chisq relrisk;
weight count;
run;

*3d / 3e do not require code;

*4a does not require code;

*4b. ;
proc power;
twosamplefreq
relativerisk = 1.73
refproportion = 0.004
groupweights = (1 1)
power = 0.90
ntotal = .;
run;

*4c. requires no code;
