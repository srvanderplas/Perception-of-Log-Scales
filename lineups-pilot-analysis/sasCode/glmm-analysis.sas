*CLEARS SAS LOG AND RESULTS FOR CLEANER WORKING ENVIRONMENT;
dm "log; clear; odsresults; clear;";

PROC IMPORT
	DATAFILE = 'C:\Users\ERobi\Documents\GitHub\Perception-of-Log-Scales\pilot_analysis\data\graphics-group-09.17.2020.csv'
	OUT = results_data
	REPLACE;
	GUESSINGROWS = 50;
RUN;

TITLE "Results Data";
PROC PRINT DATA = results_data (OBS = 10) NOOBS;
RUN;

/*description ip_address nick_name age gender academic_study start_time end_time run_time pic_id test_param param_value */
/*rorschach sample_size obs_plot_location response_no correct conf_level choice_reason data_name pic_name participant_count plot_count */

PROC GLIMMIX DATA = results_data METHOD = Laplace;
	WHERE	plot_count > 3 & rorschach = "0" & param_value in  ("target-E-Hv_null-E-Lv_r0",
																"target-H-Hv_null-H-Lv_r0",

		                                                      	"target-H-Lv_null-E-Lv_r0",
                                                     
		                                                     	"target-H-Hv_null-M-Hv_r0",
		                                                      	"target-H-Lv_null-M-Lv_r0",
		                                                      	"target-M-Lv_null-H-Lv_r0",
		                                                      
		                                                        "target-E-Hv_null-M-Hv_r0",
		                                                   		"target-H-Hv_null-H-Hv_r1");
	CLASS 	nick_name pic_id test_param param_value rorschach;
	MODEL 	correct = test_param|param_value / D = Binomial LINK = cloglog;
	RANDOM	Intercept test_param / SUBJECT = nick_name;
	LSMEANS	test_param*param_value / PLOT = MEANPLOT(SLICEBY = test_param JOIN CL);
	NLOPTIONS MAXITER = 100;
RUN;
