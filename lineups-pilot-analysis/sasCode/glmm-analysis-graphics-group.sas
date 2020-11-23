*CLEARS SAS LOG AND RESULTS FOR CLEANER WORKING ENVIRONMENT;
dm "log; clear; odsresults; clear;";

* ----------------------------------------------------------------------------------------------------------------------------------------;
* Import Full Results Data ---------------------------------------------------------------------------------------------------------------;
* ----------------------------------------------------------------------------------------------------------------------------------------;

PROC IMPORT
	DATAFILE = 'C:\Users\ERobi\Documents\GitHub\Perception-of-Log-Scales\lineups-pilot-analysis\data\jsm-student-paper-11232020.csv'
	OUT = lineup_results_data
	REPLACE;
	GUESSINGROWS = 50;
RUN;

TITLE "Lineup Results Data";
PROC PRINT DATA = lineup_results_data (OBS = 10) NOOBS;
RUN;

* ----------------------------------------------------------------------------------------------------------------------------------------;
* Filter for Model Data ------------------------------------------------------------------------------------------------------------------;
* ----------------------------------------------------------------------------------------------------------------------------------------;

/*description ip_address nick_name age gender academic_study start_time end_time run_time data_name pic_name pic_id */
/*test_param param_value rorschach target_curvature null_curvature target_variability null_variability */
/*sample_size obs_plot_location response_no correct conf_level choice_reason participant_count plot_count */

DATA lineup_model_data;
	SET 	lineup_results_data;
	WHERE 	participant_count > 6 & rorschach = '0' & target_variability = null_variability;
/*	WHERE 	participant_count > 6 & rorschach = '0' & target_variability ne null_variability;*/
RUN;

TITLE "Lineup Model Data";
PROC PRINT DATA = lineup_model_data (OBS = 10) NOOBS;
RUN;

* ----------------------------------------------------------------------------------------------------------------------------------------;
* ----------------------------------------------------------------------------------------------------------------------------------------;
* ----------------------------------------------------------------------------------------------------------------------------------------;

TITLE "Binomial Overall Linear vs Log (Within Variability)";
PROC GLIMMIX DATA = lineup_model_data;
	CLASS 	nick_name pic_id test_param param_value rorschach target_curvature null_curvature target_variability null_variability;
	MODEL 	correct = test_param / D = Binomial LINK = logit;
	RANDOM	intercept / SUBJECT = nick_name;
	LSMEANS	test_param / PLOT = MEANPLOT(CL ILINK) ILINK CL LINES;
	NLOPTIONS MAXITER = 100;
RUN;

TITLE "Binomial Split Plot (Within Variability)";
PROC GLIMMIX DATA = lineup_model_data METHOD = Laplace;
	CLASS 	nick_name pic_id test_param param_value rorschach target_curvature null_curvature target_variability null_variability;
	MODEL 	correct = test_param|target_curvature|null_curvature|target_variability / D = Binomial LINK = logit;
	RANDOM	intercept target_curvature*null_curvature*target_variability / SUBJECT = nick_name;
	LSMEANS	test_param*target_curvature*null_curvature*target_variability / SLICE = target_curvature*null_curvature*target_variability 
																		   PLOT = MEANPLOT(SLICEBY = test_param PLOTBY = target_variability JOIN CL ILINK) ILINK CL;
	NLOPTIONS MAXITER = 100;
RUN;
