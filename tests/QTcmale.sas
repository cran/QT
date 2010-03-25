ods trace output;
ods pdf file="QTc.pdf";

libname original xport "qtcdata.xpt";

options nofmterr;
proc copy in=original out=work;
run;
data data; 
	set work.sas;
run;
proc sort data=data;
by ID RR;
run;
data qtci; 
	set data;
	where qbase=1;
run;
ods listing;
TITLE 'QTcI ANALYSIS (Estimate QTcI exponent using baseline data only)'; 
PROC MIXED DATA =qtci METHOD=ML;
CLASS ID GENDER;
MODEL LNQT=GENDER LNRR LNRR*GENDER / SOLUTION CL COVB ALPHA=0.05 ALPHAP=0.05 DDFM=BW
outpm=predI1 outp=predblupI1 (rename=(pred=eblup));
ods output SolutionF=parameterI1 COVB=covI1 COVPARMS=iivI1 FitStatistics=fitI1 SolutionR=randomI1;
RANDOM INT LNRR / TYPE=UN SUBJECT=ID s;
RUN;

data predI1; set predI1; baseline= 'Yes'; correction="QTcI"; if id="-9999" or id="-999"; tused=(upper-pred)/StdErrPred;keep tused trt gender lnrr pred StdErrPred DF alpha lower upper baseline correction; run;
data parameterI1; set parameterI1; baseline= 'Yes'; correction="QTcI"; run;
data fitI1; set fitI1; baseline = 'Yes'; correction="QTcI"; run;
data randomI1; set randomI1; baseline = 'Yes'; correction="QTcI"; run;
data predblupI1; set predblupI1; baseline = 'Yes'; correction="QTcI"; run;
data iivI1; set iivI1; baseline = 'Yes'; correction="QTcI"; run;

/*Merge qtci parameter estimates with work.data to calculate qtci*/
data random;
   set randomI1;
   where effect="LNRR";
   eta=estimate;
   keep id eta;
run;
proc sort data=random ;
by id;
run;
proc sort data=data out=data;
	by id;
run;
data data;
	merge data random;
	fixed=1;
	by id;
run;

/*proc sort data=derived.sas out=gender nodupkey;
by pid;
run;*/
data popMale;
	set parameterI1;
	where effect="LNRR";
	fixed=1;
	pop=estimate;
	keep fixed pop;
run;
data data;
   merge data popmale;
   by fixed;
run;
data data;
	set data;
	exponent=eta+pop;
run;
data data;
	set data;
	if qt="." then qt=0;
	qtci = qt/(rr/1000)**exponent;
	newrr = (rr-1000)/1000;
	if id="-999" then qtci=".";
	if id="-9999" then qtci=".";
	if id="-999" then qbase=0;
	if id="-9999" then qbase=0;run;
data nonbase;
	set data;
	where qbase=0;
run;

TITLE 'QTcI ANALYSIS (Check QTcI correlation with RR)'; 
PROC MIXED DATA =nonbase METHOD=ML;
CLASS ID GENDER;
MODEL QTcI=GENDER NEWRR / SOLUTION CL COVB ALPHA=0.05 ALPHAP=0.05 DDFM=BW
outpm=predI2 outp=predblupI2 (rename=(pred=eblup));
ods output SolutionF=parameterI2 COVB=covI2 COVPARMS=iivI2 FitStatistics=fitI2 SolutionR=randomI2;
RANDOM INT NEWRR / TYPE=UN SUBJECT=ID s;
RUN;

data predI2; set predI2; lnrr=NEWRR; baseline= 'No'; correction="QTcI"; if id="-9999" or id="-999"; tused=(upper-pred)/StdErrPred;keep tused trt gender lnrr pred StdErrPred DF alpha lower upper baseline correction; run;
data parameterI2; set parameterI2; baseline= 'No'; correction="QTcI"; run;
data fitI2; set fitI2; baseline = 'No'; correction="QTcI"; run;
data randomI2; set randomI2; baseline = 'No'; correction="QTcI"; run;
data predblupI2; set predblupI2; baseline = 'No'; correction="QTcI"; run;
data iivI2; set iivI2; baseline = 'No'; correction="QTcI"; run;

TITLE 'QTcB ANALYSIS (Check QTcB correlation with RR)'; 
PROC MIXED DATA = nonbase METHOD=ML;
CLASS ID GENDER;
MODEL QTcB=GENDER NEWRR / SOLUTION CL COVB ALPHA=0.05 ALPHAP=0.05 DDFM=BW
outpm=predB outp=predblupB (rename=(pred=eblup));
ods output SolutionF=parameterB COVB=covB COVPARMS=iivB FitStatistics=fitB SolutionR=randomB;
RANDOM INT NEWRR / TYPE=UN SUBJECT=ID s;
RUN;

data predB; set predB; lnrr=NEWRR; baseline= 'No'; correction="QTcB"; if id="-9999" or id="-999"; tused=(upper-pred)/StdErrPred;keep tused trt gender lnrr pred StdErrPred DF alpha lower upper baseline correction; run;
data parameterB; set parameterB; baseline= 'No'; correction="QTcB"; run;
data fitB; set fitB; baseline = 'No'; correction="QTcB"; run;
data randomB; set randomB; baseline = 'No'; correction="QTcB"; run;
data predblupB; set predblupB; baseline = 'No'; correction="QTcB"; run;
data iivB; set iivB; baseline = 'No'; correction="QTcB"; run;

TITLE 'QTcF ANALYSIS (Check QTcF correlation with RR)'; 
PROC MIXED DATA =nonbase METHOD=ML;
CLASS ID GENDER;
MODEL QTcF=GENDER NEWRR / SOLUTION CL COVB ALPHA=0.05 ALPHAP=0.05 DDFM=BW
outpm=predF outp=predblupF (rename=(pred=eblup));
ods output SolutionF=parameterF COVB=covF COVPARMS=iivF FitStatistics=fitF SolutionR=randomF;
RANDOM INT NEWRR / TYPE=UN SUBJECT=ID s;
RUN;
data predF; set predF; lnrr=NEWRR; baseline= 'No'; correction="QTcF"; if id="-9999" or id="-999"; tused=(upper-pred)/StdErrPred;keep tused trt gender lnrr pred StdErrPred DF alpha lower upper baseline correction; run;
data parameterF; set parameterF; baseline= 'No'; correction="QTcF"; run;
data fitF; set fitF; baseline = 'No'; correction="QTcF"; run;
data randomF; set randomF; baseline = 'No'; correction="QTcF"; run;
data predblupF; set predblupF; baseline = 'No'; correction="QTcF"; run;
data iivF; set iivF; baseline = 'No'; correction="QTcF"; run;

ods pdf close;
ods listing close;
ods trace off;
quit;
data pred; set predI1 predI2 predB predF; run;
data predblup; set predblupI1 predblupI2 predblupB predblupF; run;
data parameter; set parameterI1 parameterI2 parameterB parameterF; run;
data eta; set randomI1 randomI2 randomB randomF; run;
data fit; set fitI1 fitI2 fitB fitF; run;
data iiv; set iivI1 iivI2 iivB iivF; run;

PROC EXPORT DATA=parameter
            OUTFILE= "qtciparaest.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=eta
            OUTFILE= "qtcirandom.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=iiv
            OUTFILE= "qtciiiv.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=fit
            OUTFILE= "qtcifit.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=pred
            OUTFILE= "qtcipred.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=predblup
            OUTFILE= "qtciipred.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=data
            OUTFILE= "qtci.csv" 
            DBMS=CSV REPLACE;
RUN;
