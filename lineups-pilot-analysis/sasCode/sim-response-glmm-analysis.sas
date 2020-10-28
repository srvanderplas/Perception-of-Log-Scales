*CLEARS SAS LOG AND RESULTS FOR CLEANER WORKING ENVIRONMENT;
dm "log; clear; odsresults; clear;";

PROC IMPORT
	DATAFILE = 'C:\Users\ERobi\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\data\sim_response_data.csv'
	OUT = sim_lineup_data
	REPLACE;
	GUESSINGROWS = 50;
RUN;

TITLE "Simulated Lineup Results Data";
PROC PRINT DATA = sim_lineup_data (OBS = 10) NOOBS;
RUN;

TITLE "Simulated Lineup Model";
PROC GLIMMIX DATA = sim_lineup_data PCONV = 1e-2;
	CLASS 	nick_name data_name pic_id test_param param_value target_curvature null_curvature;
	MODEL 	correct = test_param|target_curvature|null_curvature / D = Binomial LINK = logit;
	RANDOM	intercept / SUBJECT = nick_name;
	RANDOM	intercept / SUBJECT = data_name;
	RANDOM	target_curvature*null_curvature / SUBJECT = nick_name*data_name;
	LSMEANS	test_param*target_curvature*null_curvature / PLOT = MEANPLOT(SLICEBY = test_param CL ILINK JOIN) ILINK CL LINES;
	NLOPTIONS MAXITER = 100;
RUN;
