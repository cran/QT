ods trace output;
ods pdf file="QTcSponsor.pdf";

libname original xport "qtcsponsordata.xpt";

options nofmterr;
proc copy in=original out=work;
run;
data data; 
	set work.sas;
	newrr = (rr-1000)/1000;
	if id="-999" then qbase=0;
	if id="-9999" then qbase=0;
run;

data nonbase;
	set data;
	where qbase=0;
run;

proc sort data=nonbase;
by ID RR;
run;

TITLE 'QTc ANALYSIS'; 
PROC MIXED DATA =nonbase METHOD=ML;
CLASS ID GENDER TRT;
MODEL QTC =GENDER NEWRR / SOLUTION CL COVB ALPHA=0.05 ALPHAP=0.05 DDFM=BW
outpm=pred outp=predblup (rename=(pred=eblup));
ods output SolutionF=parameter COVB=cov COVPARMS=iiv FitStatistics=fit SolutionR=random;
RANDOM INT NEWRR / TYPE=UN SUBJECT=ID s;
RUN;
data pred; set pred; LNRR=NEWRR; baseline= 'No'; correction="QTc"; if id="-9999" or id="-999"; tused=(upper-pred)/StdErrPred;keep tused trt gender LNRR pred StdErrPred DF alpha lower upper baseline correction; run;
data parameter; set parameter; baseline= 'No'; correction="QTc"; run;
data fit; set fit; baseline = 'No'; correction="QTc"; run;
data random; set random; baseline = 'No'; correction="QTc"; run;
data predblup; set predblup; baseline = 'No'; correction="QTc"; run;
data iiv; set iiv; baseline = 'No'; correction="QTc"; run;

ods pdf close;
ods listing close;
ods trace off;
quit;

data pred; set pred; run;
data predblup; set predblup; run;
data parameter; set parameter; run;
data eta; set random; run;
data fit; set fit; run;
data iiv; set iiv; run;

PROC EXPORT DATA=parameter
            OUTFILE= "qtcsponsorparaest.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=eta
            OUTFILE= "qtcsponsorrandom.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=iiv
            OUTFILE= "qtcsponsoriiv.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=fit
            OUTFILE= "qtcsponsorfit.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=pred
            OUTFILE= "qtcsponsorpred.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=predblup
            OUTFILE= "qtcsponsoripred.csv" 
            DBMS=CSV REPLACE;
RUN;
PROC EXPORT DATA=data
            OUTFILE= "qtcsponsor.csv" 
            DBMS=CSV REPLACE;
RUN;
