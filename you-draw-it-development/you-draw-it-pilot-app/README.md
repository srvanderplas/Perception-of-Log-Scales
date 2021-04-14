# Pilot: You Draw It
## Increasing Exponential Data Displayed on a Log Scale
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

## Exponential Data
+ parm_id (4): exp_1, exp_2, exp_3, exp_4

+ Visit [Eye Fitting Straight Lines in the Modern Era](https://emily-robinson.shinyapps.io/you-draw-it-parameter-selection/) for examples.

## Linear Data

+ Parameter combinations were selected to simulate data that replicates the data sets (S, F, V, N) in [Eye Fitting Straight Lines (1981)](https://www.tandfonline.com/doi/abs/10.1080/00031305.1981.10479335).
+ Visit [Eye Fitting Straight Lines in the Modern Era](https://emily-robinson.shinyapps.io/you-draw-it-parameter-selection/) for examples.

1. Randomly select and jitter N = 30 x-values along the domain.
2. Determine y-intercept at x = 0 from the provided slope and y-intercept at the mean of x (y_xbar).
    + Slope-intercept form: y - y_xbar = m(x-xbar)
3. Generate "good" errors based on N(0,sigma). 
    + Set constraint of the mean of the first N/3 = 10 errors less than |2 x sigma|

# Experimental Design

## Treatment Design
+ Linear (Eye fitting straight lines): 1-way ANOVA with 4 treatments
    + 4 Treatments
      + **S**: positive slope - low variance 
      + **F**: positive slope - high variance
      + **V**: steep positive slope
      + **N**: negative slope - high variance
      
```{r eyefitting-paramaeter-details}
eyefitting_parameter_details <- tibble(
                                  parm_id = c("S", "F", "V", "N"),
                                  y_xbar = c(3.88, 3.9, 3.89, 4.11),
                                  slope  = c(0.66, 0.66, 1.98, -0.70),
                                  sigma  = c(1.3, 2.8, 1.5, 2.5),
                                  x_min   = c(0, 0, 4, 0),
                                  x_max   = c(20, 20, 18, 20)
                                  )
eyefitting_parameter_details %>%
  knitr::kable()
```
    
+ Exponential (Linear/Log): 2 x 2 x 2 Factorial
    + Beta: 0.1 (sd. 0.09); 0.23 (0.25)
    + Points End: 0.5; 0.75
    + Scale: Linear; Log

## Experimental Design
+ See `code/randomization.R`.
+ 8 data sets were generated for each individual upon start of experiment (4 Linear + 4 Exponential)
+ Each individual participated in 12 you draw it plot tasks (4 Linear + (4 Exponential x 2 Scales))
+ The order of each of the 12 plots was randomly assigned for each individual in a CRD.

# Data Files
+ See `you_draw_it_data.db`.
+ Data file and field descriptions can be found in `data/README.md`.

# Results
+ [Github Repo](https://github.com/srvanderplas/Perception-of-Log-Scales/tree/master/you-draw-it-development/you-draw-it-pilot-app)
+ Presentations: [You Draw It with r2d3 - ISU Graphics Group 04-08-2021](https://srvanderplas.github.io/Perception-of-Log-Scales/presentations/you-draw-it-with-r2d3-graphicsgroup04082021/index.html#1)
+ Manuscripts: 

