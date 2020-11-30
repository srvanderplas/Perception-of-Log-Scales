*CLEARS SAS LOG AND RESULTS FOR CLEANER WORKING ENVIRONMENT;
dm "log; clear; odsresults; clear;";

PROC IMPORT
	DATAFILE = 'C:\Users\ERobi\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\data\jsm-student-paper-11232020.csv'
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

	LSMEANS	test_param*curvature / PLOT = MEANPLOT(SLICEBY = test_param CL ILINK JOIN) ILINK CL;
	SLICE	test_param*curvature / SLICEBY = curvature LINES;

	NLOPTIONS MAXITER = 100;
RUN;
