# Pilot: You Draw It
## Exponential Prediction (Log/Linear)
## Eye Fitting Straight Lines in the Modern Era

### Shiny front-end/testing framework for you draw it with r2d3 package

Emily Robinson, Susan VanderPlas, Reka Howard

# Experiment Name

+ Conducted: April 2021 - Present
+ Platform: [Shiny App](https://shiny.srvanderplas.com/eye-fitting/ )
+ Recruitment Method: ISU Graphics Group, etc.
+ Trend Types: Simulated Exponential (linear/log scale); Simulated Linear

# Data Simulation
+ Data was simulated for each individual upon start of experiment.
+ See `code/data-generation.R` for data simulation functions.
+ See [Data Validation](https://srvanderplas.github.io/Perception-of-Log-Scales/you-draw-it-development/you-draw-it-pilot-app/validation/data-generation-validation.html) for 100 simulation reps.

## Exponential Data

*Algorithm: Exponential Data Generation* `expDataGen()`

**In parameters:** `beta, sd, points_choice = "partial", points_end_scale, N = 30, xmin = 20, xmax = 20, xby = 0.25`

**Out:** data list of point data and line data

1. Randomly select and jitter N = 30 x-values along the domain.
2. Generate "good" errors based on N(0,sd). 
    + Set constraint of the mean of the first N/3 = 10 errors less than |2*sd|
3. Simulate point data based on
    + `y = exp(x*beta + errorVals)`
4. Obtain starting value for beta
    + `lm(log(y) ~ x, data = point_data)`
5. Use NLS to fit a better line to the point data
    + `nls(y ~ exp(x*beta), data = point_data, ...)`
6. Simulate nonlinear least squares line data
    + `y = exp(x*betahat)`
7. Output data list of point data and line data

+ Specific r2d3 options:
    + aspect ratio = 1
    + **linear =**
    + free_draw = FALSE
    + points = "partial",
    + x_by = 0.25
    + draw_start_scale = 0.5 (start drawing at x = 10)
    + show_finished = T - graphics group / F
    + x_range = c(0,20)
    + y_range = range(y-points)*c(0.5, 2)

## Linear Data

+ Parameter combinations were selected to simulate data that replicates the data sets (S, F, V, N) in [Eye Fitting Straight Lines (1981)](https://www.tandfonline.com/doi/abs/10.1080/00031305.1981.10479335).

*Algorithm: Linear Data Generation* `linearDataGen()`

**In parameters:** `y_xbar, slope, sigma, N = 30, xmin, xmax, xby = 0.25`

**Out:** data list of point data and line data

1. Randomly select and jitter N = 30 x-values along the domain.
2. Determine y-intercept at x = 0 from the provided slope and y-intercept at the mean of x (y_xbar).
    + Slope-intercept form: `y - y_xbar = m(x-xbar)`
3. Generate "good" errors based on N(0,sigma). 
    + Set constraint of the mean of the first N/3 = 10 errors less than |2*sigma|
4. Simulate point data based on
    + `y = yintercept + slope*x + error`
5. Obtain least squares regression coefficients
    + `lm(y ~ x, data = point_data)`
6. Simulate least squares regression line data
    + `y = yintercepthat + slopehat*x`
7. Output data list of point data and line data

+ Specific r2d3 options:
    + aspect ratio = 1
    + linear = "true"
    + free_draw = TRUE
    + points = "full",
    + x_by = 0.25
    + draw_start = NA
    + show_finished = T - graphics group / F
    + x_range = c(0,20)
    + y_range = range(all eye fitting data)*c(1.1, 1.1)

# Plot Generation
+ See `www/js/shinydrawr-d3v5.js` for D3.js code.

# Experimental Design

## Treatment Design
+ Linear ([Eye fitting straight lines](https://srvanderplas.github.io/Perception-of-Log-Scales/you-draw-it-development/eye-fitting-straight-lines-in-the-modern-era/eye-fitting-straight-lines-in-the-modern-era.html)): 1-way ANOVA with 4 treatments
    + 4 Treatments
      + **S**: positive slope, low variance (y_xbar = 3.88, slope = 0.66, sigma = 1.3, xrange = (0,20))
      + **F**: positive slope, high variance (y_xbar = 3.9, slope = 0.66, sigma = 1.98, xrange = (0,20))
      + **V**: steep positive slope (y_xbar = 3.89, slope = 1.98, sigma = 1.5, xrange = (4,18))
      + **N**: negative slope, high variance (y_xbar = 4.11, slope = -0.70, sigma = 2.5, xrange = (0,20))

+ Exponential (Linear/Log): 2 x 2 x 2 Factorial
    + Beta: 0.1 (sd. 0.09); 0.23 (0.25)
    + Points End: 0.5; 0.75
    + Scale: Linear; Log
    
+ Visit [You Draw It Development - parameter selection](https://emily-robinson.shinyapps.io/you-draw-it-parameter-selection/) for examples.

## Experimental Design
+ See `code/randomization.R`.
+ 8 data sets were generated for each individual upon start of experiment (4 Linear + 4 Exponential)
+ 12 you draw it task plots were shown to each individual (4 Linear + (4 Exponential x 2 Scales))
+ The order of each of the 12 plots was randomly assigned for each individual in a CRD.

# Data Files
+ Data file can be found in `you_draw_it_data.db`.
+ Field descriptions can be found in [data-manifest.md](https://github.com/srvanderplas/Perception-of-Log-Scales/blob/master/you-draw-it-development/you-draw-it-pilot-app/data-manifest.md).

# Results
+ [Github Repo](https://github.com/srvanderplas/Perception-of-Log-Scales/tree/master/you-draw-it-development/you-draw-it-pilot-app)
+ Presentations: 
    + [You Draw It with r2d3 - ISU Graphics Group 04-08-2021](https://srvanderplas.github.io/Perception-of-Log-Scales/presentations/you-draw-it-with-r2d3-graphicsgroup04082021/index.html#1)
+ Manuscripts: 
+ Preliminary Analyses:
    + ISU Graphics Group (04/08/2021): [Exponential Prediction](https://srvanderplas.github.io/Perception-of-Log-Scales/you-draw-it-development/you-draw-it-pilot-app/analyses/you-draw-it-exponential-prediction-graphics-group-04.08.2021.html), [Eye Fitting Straight Lines](https://srvanderplas.github.io/Perception-of-Log-Scales/you-draw-it-development/you-draw-it-pilot-app/analyses/you-draw-it-eyefitting-straight-lines-graphics-group-04.08.2021.html)
    + Twitter/Reddit/Direct Email Pilot Study (05/03/2021): [Exponential Prediction](https://srvanderplas.github.io/Perception-of-Log-Scales/you-draw-it-development/you-draw-it-pilot-app/analyses/you-draw-it-exponential-prediction-pilot.html), [Eye Fitting Straight Lines](https://srvanderplas.github.io/Perception-of-Log-Scales/you-draw-it-development/you-draw-it-pilot-app/analyses/you-draw-it-eyefitting-straight-lines-pilot.html)

