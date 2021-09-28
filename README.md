# Log perception evaluated through graphical testing

This repository is used for the development, analysis, and collaboration for research evaluating log perception through graphical tests. In particular, the goal of this research is to use graphical tests to determine if there are benefits to displaying exponentially increasing data on a log scale rather than a linear scale. This research is inspired by the increased use of graphics to communicate the status of the COVID-19 pandemic to the public. We have developed and conducted a series of three graphical tests implemented through RShiny designed to evaluate the impact our choice of scale (log/linear) has on human perception of exponentially increasing trends. The three studies focus on: perception of exponential growth, prediction of exponential growth, and translation from graphical to numerical estimation. The results of these graphical tests will allow me to provide guidelines for other statisticians to help them actively choose which of many possible graphics to draw, according to some set of design choices, to ensure that their charts are effective.

## Graphical Tests

### 1. Perception through Lineups

+ Participate [here](https://shiny.srvanderplas.com/log-study/)

To lay a foundation for future exploration of the use of log scales, the first study utilizes statistical lineups, a plot consisting of smaller panels introduced in Buja et. al (2009), to examine the most fundamental ability to identify differences in charts; this does not require that participants understand exponential growth, identify log scales, or have any mathematical training. Instead, we are simply testing the change in perceptual sensitivity resulting from visualization choices. Preliminary results indicate that presenting data on the log scale makes us more sensitive to slight changes in curvature; an exception occurred when identifying an exponential curve from curves closely resembling a linear trend, indicating it is easy to identify a curve in a group of lines but much harder to identify a line in a group of curves.

### 2. Prediction with 'You Draw It'

+ Participate [here](https://emily-robinson.shinyapps.io/you-draw-it-pilot-app/)

To determine whether there are cognitive disadvantages to log scales, the second study utilizes interactive graphics to test an individuals' ability to make predictions for exponentially increasing data. As part of this study, we established a graphical testing method adapted from the New York Times 'You Draw It' feature. 'You Draw It' task plots are created using Data Driven Documents (D3), a JavaScript-based graphing framework that facilitates user interaction. Integrating this into RShiny using the r2d3 package, participants are asked to draw a line using their computer mouse through the scatter-plot trend shown on their screen. The 'You Draw It' method is validated by replicating "Eye Fitting Straight Lines" by Mosteller et. al (1981). This tool is then used to test an individuals' ability to make predictions for exponentially increasing data on both the log and linear scales. Results from the pilot study provide support that when exponential growth rate is large, underestimation of exponential growth occurs when making predictions on a linear scale and there is an improvement in accuracy of predictions made on the log scale.

**Eye fitting straight lines in the modern era**
+ Reddit/Twitter/Email (Spring 2021) [analysis](https://srvanderplas.github.io/Perception-of-Log-Scales/analysis/youdrawit-eyefitting-model.html)

**Underestimation of exponential growth**
+ Reddit/Twitter/Email (Spring 2021) [analysis](https://srvanderplas.github.io/Perception-of-Log-Scales/analysis/youdrawit-exponential-model.html)

### 3. Numerical Translation and Estimation

*This study is currently under development*

The final study is designed to test an individuals' ability to translate a graph of exponentially increasing data into real value quantities. This study requires higher cognitive thinking as participants must be able to interpret and understand the context of the data being presented. While this study is currently under development, we are interested in the results as this study requires the most cognitive attention and ability and as a general public, we are untrained in understanding logarithmic functions.

## Results

**Papers**

+ [2021 JSM Student Paper](https://srvanderplas.github.io/Perception-of-Log-Scales/manuscripts/jsm-2021-student-paper-submission/jsm-2021-student-paper-submission.pdf)
+ Download Emily's dissertation [here](https://github.com/earobinson95/EmilyARobinson-UNL-dissertation/raw/2bf2a4613c95c5f93ae065fbe97ffef43bd68560/_book/thesis.pdf); [Github repository](https://github.com/earobinson95/EmilyARobinson-UNL-dissertation)
+ Eye Fitting Straight Lines in the Modern Era [(In Preparation)](https://earobinson95.github.io/Eye-Fitting-Straight-Lines-in-the-Modern-Era/Eye-Fitting-Straight-Lines-in-the-Modern-Era.pdf)

**Presentations**

+ Do logs work during a pandemic? Perception of exponentially increasing data, *Iowa State Graphics Group* (2/11/2021) [slides](https://srvanderplas.github.io/Perception-of-Log-Scales/presentations/graphics-group-02112020/index.html#1) 
+ 'You Draw It' with r2d3, *Iowa State Graphics Group* (4/8/2021) [slides](https://srvanderplas.github.io/Perception-of-Log-Scales/presentations/you-draw-it-with-r2d3-graphicsgroup04082021/index.html#1)
+ Human perception of exponentially increasing data displayed on a log scale evaluated through experimental graphics tasks, *UNL Ph.D. Preliminary Exam* (7/22/2021) [slides](https://earobinson95.github.io/presentations/Dissertation/2021-07-22-preliminary-exam/index.html#1)
+ Pandemic Dilemmas: Human perception of exponentially increasing data displayed on a log scale evaluated through experimental graphics tasks, *Joint Statistical Meetings* (8/9/2021) [slides](https://earobinson95.github.io/presentations/Conferences/2021-JSM/2021-JSM-recording/index.html#1) / [video](https://app.vidgrid.com/view/PosTs8VPU6p8)
+ Eye Fitting Straight Lines in the Modern Era, *Midwest Women in Science Conference* (9/18/2021) [slides](https://earobinson95.github.io/presentations/Conferences/2021-MidwestWISC/index.html#1)
