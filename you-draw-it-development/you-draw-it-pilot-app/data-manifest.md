# Contents

+ See `you_draw_it_data.db`.

+ Completed studies:
  + ISU Graphics Group (04-08-2021 at 4:00pm) [Exponential Prediction](https://srvanderplas.github.io/Perception-of-Log-Scales/you-draw-it-development/you-draw-it-pilot-app/analyses/you-draw-it-exponential-prediction-graphics-group-04.08.2021.html), [Eye Fitting Straight Lines]()

+ Tables
    + experiment_details
        + **experiment:** provides the graphical experiment name, `emily-you-draw-it-pilot-app`.
        + **question:** what question are the participants being asked? `Use your mouse to fill in the trend in the yellow box region`.
        + **ydipp:** you draw it's per plot, `12 (8 log scale; 4 eyefitting)`.
        + **trials_req:** number of trieals required for participant to move on to the experiment, `0`.
    + exp_parameter_details
        + **parm_id:** unique parameter combination ID for exponential data, `exp_1, exp_2, exp_3, exp_4`.
        + **beta:** beta parameter used to simulate exponential point data, `0.1, 0.23`.
        + **sd:** standard deviation used to generate exponential point data errors, `0.09, 0.25`.
        + **points_end_scale:** parameter indicating how far along the x-domain to simulate points, `0.5, 0.75`.
        + **points_choice:** parameter indicating how far to show points in the you draw it plot, `partial`.
        + **N:** number of points to generate along the exponential trend, `N = 30`.
        + **aspect_ratio:** aspect ratio of the you draw it plot, `1`.
        + **free_draw:** allow free draw on the you draw it plot, `FALSE`.
        + **x_min:** minimum x value to simulate data from, `0`.
        + **x_max:** maximum x value to simulate data to, `20`.
        + **x_by:** step of x values to simulate data from, also determines the increment of user data outputted, `0.25`.
        + **ymin_scale:** lower end buffer on y-axis, `0.5 (implies min(y)*0.5)`
        + **ymax_scale:** upper end buffer on y-axis, `2 (implies max(y)*2)`.
        + **draw_start_scale:** where along the x-axis should the user begin drawing `0.5 (implies 20*0.5 = 10)`
    + eyefitting_parameter_details `see README.md Treatment Design`
        + **parm_id:** unique parameter combination ID for eye fitting straight lines data `S, F, V, N`
        + **y_xbar:** parameter indicating the y-value at the mean of the x values.
        + **slope:** parameter indicating the slope of the line.
        + **sigma:** parameter indicating the standard deviation of the point data errors.
        + **x_min:** minimum x value to simulate data from, `0`.
        + **x_max:** maximum x value to simulate data to, `20`.
    + simulated_data
        + **dataset:** indicates whether the given x/y values correspond to the point data or line data
        + **parm_id:** provides the unique parameter combination ID `exp_1, exp_2, exp_3, exp_4, S, F, V, N`.
        + **x:** x values for the entire simulated data set.
        + **y:** corresponding y values for the entire simulated data set (points or lines)
        + **ip_address:** (hashed for privacy): unique identifier for the computer being used to complete the experiment.
        + **nick_name:** (hashed for privacy): unique identifier combining the user's ip_address, screen resolution, etc.
        + **study_starttime:** indicates the time participant begin the study, used for unique identification of run.
    + feedback
        + **parm_id:** provides the unique parameter combination ID `exp_1, exp_2, exp_3, exp_4, S, F, V, N`.
        + **x:** x values of the points drawn by the user.
        + **y:** y values of the actual fitted line (by lm or nls) corresponding to the y values.
        + **ydrawn:** y values as drawn by the user line (participant) corresponding to the x values.
        + **linear:** indicates whether the data was drawn on the linear or log scale `true/false`.
        + **ip_address:** (hashed for privacy): unique identifier for the computer being used to complete the experiment.
        + **nick_name:** (hashed for privacy): unique identifier combining the user's ip_address, screen resolution, etc.
        + **study_starttime:** indicates the time participant begin the study, used for unique identification of run.
        + **start_time:** time in which the participant began predicting that particular you draw it plot
        + **end_time:** time in which the participant completed predicting that particular you draw it plot.
    + users
        + **nick_name:** (hashed for privacy): unique identifier combining the user's ip_address, screen resolution, etc.
        + **study_starttime:** indicates the time participant begin the study, used for unique identification of run.
        + **age:** indicates the participants age range.
        + **gender:** indicates the gender the participant identifies by.
        + **academic_study:** indicates the level of education the participant has completed.
        + **recruitment:** indicates how the participant was recruited for the study.
        + **ip_address:** (hashed for privacy): unique identifier for the computer being used to complete the experiment.
    
  