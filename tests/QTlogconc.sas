ods trace output;
ods pdf file="QTlogconcAnalysis.pdf";

libname original xport "data.xpt";


options nofmterr;
proc copy in=original out=work;
run;
data old1; 
	set work.sas;
	DQTCOBS=qtc_cfb;
	DDQTCOBS=ddqtc;
run;

data old2; set old1;
where QTCONC = 1;
run;
data old3; set old2;
where CONC>0 and CONC NE .;
run;
data old3; set old3;
CONC=log(CONC);
run;
TITLE 'ANALYSIS INCLUDING INTERCEPT'; 
PROC MIXED DATA =OLD3 METHOD=ML;
CLASS ID;
MODEL DDQTCOBS=CONC/ SOLUTION CL ALPHA=0.1 ALPHAP=0.1 DDFM= KENWARDROGER
outpm=pred1
outp=predblup1 (rename=(pred=eblup));
RANDOM INT CONC / TYPE=UN SUBJECT=ID s;
ods output SolutionF=parameter1 CovParms=iiv1 FitStatistics=fit1 SolutionR=random1;
RUN;

data qtp1; set pred1; intercept= 'Yes'; if id="-999" or id="-9999"; tused=(upper-pred)/StdErrPred;keep tused trt conc pred StdErrPred DF alpha lower upper intercept; run;
data para1; set parameter1; intercept= 'Yes'; run;
data fit1; set fit1; intercept = 'Yes'; run;
data random1; set random1; intercept = 'Yes'; run;
data predblup1; set predblup1; intercept = 'Yes'; run;

TITLE 'ANALYSIS EXCLUDING INTERCEPT';
PROC MIXED DATA =OLD3 METHOD=ML;
CLASS ID;
MODEL DDQTCOBS=CONC/ SOLUTION CL ALPHA=0.1 ALPHAP=0.1 NOINT DDFM= KENWARDROGER
outp=predblup2 (rename=(pred=eblup))
outpm=pred2;
RANDOM  CONC / TYPE=UN SUBJECT=ID s;
ods output   SolutionF=parameter2 CovParms=iiv2 FitStatistics=fit2 SolutionR=random2;
RUN;
data qtp2; set pred2; intercept= 'No'; if id="-999" or id="-9999"; tused=(upper-pred)/StdErrPred; keep tused trt conc pred StdErrPred DF alpha lower upper intercept ; run;
data para2; set parameter2; intercept= 'No'; run;
data fit2; set fit2; intercept = 'No'; run;
data random2; set random2; intercept = 'No'; run; 
data predblup2; set predblup2; intercept = 'No'; run;

TITLE 'ANALYSIS FIXED INTERCEPT TO ZERO';
PROC MIXED DATA =OLD3 METHOD=ML;
CLASS ID;
MODEL DDQTCOBS=CONC/ SOLUTION CL ALPHA=0.1 ALPHAP=0.1 NOINT DDFM= KENWARDROGER
outp=predblup3 (rename=(pred=eblup))
outpm=pred3;
RANDOM INT CONC / TYPE=UN SUBJECT=ID s;
ods output   SolutionF=parameter3 CovParms=iiv3 FitStatistics=fit3  SolutionR=random3;
RUN;
data qtp3; set pred3; intercept= 'Fixed'; if id="-999" or id="-9999";tused=(upper-pred)/StdErrPred; keep tused trt conc pred StdErrPred DF alpha lower upper intercept ; run;
data para3; set parameter3; intercept= 'Fixed'; run;
data fit3; set fit3; intercept='Fixed'; run;
data random3; set random3; intercept='Fixed'; run; 
data predblup3; set predblup3; intercept = 'Fixed'; run;

ods pdf close;
ods trace off;
quit;

data qtp; set qtp1 qtp2 qtp3; proc print; run;
data para; set para1 para2 para3; method = 'approx'; proc print; run;
data fit; set fit1 fit2 fit3; proc print;run;
data random; set random1 random2 random3; proc print;run;
data predblup; set predblup1 predblup2 predblup3; proc print; run;
data iiv; set iiv1 iiv2 iiv3; proc print; run;
*output all and allpara to csv files on your local disk;
PROC EXPORT DATA=qtp
            OUTFILE= "qtpred.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=para
            OUTFILE= "paraest.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=fit
            OUTFILE= "fit.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=random
            OUTFILE= "random.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=predblup
            OUTFILE= "predblup.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=iiv
            OUTFILE= "iiv.csv" 
            DBMS=CSV REPLACE;
RUN;
