ods trace output;
ods pdf file="QTtime.pdf";*pathname for the output;

libname original xport "data.xpt";

options nofmterr;

proc copy in=original out=work;
run;

data old1; 
	set work.sas;
	DQTCOBS=qtc_cfb;
	DDQTCOBS=ddqtc;
	if ddqtcobs^=.;
run;
