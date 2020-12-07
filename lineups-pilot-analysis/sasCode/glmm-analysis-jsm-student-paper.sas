*CLEARS SAS LOG AND RESULTS FOR CLEANER WORKING ENVIRONMENT;
dm "log; clear; odsresults; clear;";

* Dell: C:\Users\erobinson6\ ;
* PC: C:\Users\ERobi\ ;

ODS PDF FILE = "C:\Users\ERobi\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\results\jsm-student-paper-sas-output.pdf";

PROC IMPORT
	DATAFILE = 'C:\Users\ERobi\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\data\jsm-student-paper-11302020.csv'
	OUT = lineup_data
	REPLACE;
	GUESSINGROWS = 50;
RUN;

TITLE "Lineup Results Data";
PROC PRINT DATA = lineup_data (OBS = 10) NOOBS;
RUN;

PROC SORT DATA = lineup_data;
	BY DECENDING test_param;
RUN;

TITLE "Lineup Model";
PROC GLIMMIX DATA = lineup_data ORDER = data;
	WHERE	participant_count > 5;
	CLASS 	nick_name run data_name pic_id test_param param_value curvature target_curvature null_curvature;

	MODEL 	correct = curvature|test_param / D = Binomial LINK = logit SOLUTION;

	RANDOM	intercept / SUBJECT = run; * (row) participant block;
	RANDOM	intercept / SUBJECT = data_name; * (column) dataset block;
/*	RANDOM	curvature / SUBJECT = run*data_name; *(whole plot);*/
	*NOT INCLUDING SPLIT FOR OVERDISPERSION;

	LSMEANS	test_param*curvature / SLICEDIFF = curvature PLOT = MEANPLOT(SLICEBY = test_param CL ILINK JOIN) ILINK CL LINES ADJUST = TUKEY ODDS ODDSRATIO;
	SLICE	test_param*curvature / SLICEBY = curvature  LINES ADJUST = TUKEY;
	SLICE	test_param*curvature / SLICEBY = test_param LINES ADJUST = TUKEY;

	ODS OUTPUT LSMeans = lsmeans SliceDiffs = slicediffs Tests3 = tests3;
	NLOPTIONS MAXITER = 100;
RUN;

/*PROC PRINT DATA = lsmeans;*/
/*RUN;*/

PROC EXPORT DATA = lsmeans
	OUTFILE = 'C:\Users\ERobi\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\results\jsm-student-paper-lsmeans.csv'
	DBMS    = csv
	REPLACE;
RUN;

/*PROC PRINT DATA = slicediffs;*/
/*RUN;*/

PROC EXPORT DATA = slicediffs
	OUTFILE = 'C:\Users\ERobi\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\results\jsm-student-paper-slicediffs.csv'
	DBMS    = csv
	REPLACE;
RUN;

/*PROC PRINT DATA = tests3;*/
/*RUN;*/

PROC EXPORT DATA = tests3
	OUTFILE = 'C:\Users\ERobi\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\results\jsm-student-paper-tests3.csv'
	DBMS    = csv
	REPLACE;
RUN;

ODS PDF CLOSE;
