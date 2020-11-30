*CLEARS SAS LOG AND RESULTS FOR CLEANER WORKING ENVIRONMENT;
dm "log; clear; odsresults; clear;";

* Dell: C:\Users\erobinson6\ ;
* PC: C:\Users\ERobi\ ;

ODS PDF FILE = "C:\Users\erobinson6\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\results\jsm-student-paper-sas-output.pdf";

PROC IMPORT
	DATAFILE = 'C:\Users\erobinson6\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\data\jsm-student-paper-11302020.csv'
	OUT = sim_lineup_data
	REPLACE;
	GUESSINGROWS = 50;
RUN;

TITLE "Lineup Results Data";
PROC PRINT DATA = sim_lineup_data NOOBS;
RUN;

TITLE "Lineup Model";
PROC GLIMMIX DATA = sim_lineup_data;
	WHERE	participant_count > 5;
	CLASS 	nick_name run data_name pic_id test_param param_value curvature target_curvature null_curvature;

	MODEL 	correct = curvature|test_param / D = Binomial LINK = logit;

	RANDOM	intercept / SUBJECT = run;
	RANDOM	intercept / SUBJECT = data_name;
	RANDOM	curvature / SUBJECT = run*data_name;

	LSMEANS	test_param*curvature / PLOT = MEANPLOT(SLICEBY = test_param CL ILINK JOIN) ILINK CL LINES;
	SLICE	test_param*curvature / SLICEBY = curvature LINES;
	SLICE	test_param*curvature / SLICEBY = test_param LINES;

	ODS OUTPUT LSMeans = lsmeans;
	NLOPTIONS MAXITER = 100;
RUN;

PROC EXPORT DATA = lsmeans
	OUTFILE = 'C:\Users\erobinson6\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\results\jsm-student-paper-lsmeans-11302020.csv'
	DBMS    = csv
	REPLACE;
RUN;

ODS PDF CLOSE;
