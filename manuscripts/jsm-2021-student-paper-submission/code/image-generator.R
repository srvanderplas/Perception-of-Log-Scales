library(tidyverse)
library(nullabor)
library(gridExtra)
library(scales)

seed_num = 68505

xMid_vals  <- c(14.5, 13, 11.5)
sigma_vals <- c(0.25, 0.12, 0.05)

yRange_vals = c(10,100)
xRange_vals = c(0,20)
sample_size = 50

# ----------------------------------------------------------------------------------
# Generate Example Lineups ---------------------------------------------------------
# ----------------------------------------------------------------------------------

# Obtain alphahat, betahat, and thetahat for different midpoints.
coefEst <- function(xMid, xRange = xRange_vals, yRange = yRange_vals){
  
  # This creates the line y = -x (scaled to fit the x and y ranges)
  # |*            0
  # |  *
  # |    *
  # |      *
  # |        1
  # |          2
  # |0___________3
  #
  # where 1, 2, 3 represent different points used to determine the line curvature
  
  lineData   <- tibble(xLine = seq(xRange[1],xRange[2],0.1),
                       yLine = -(abs(diff(yRange))/abs(diff(xRange)))*(xLine-xRange[1])+yRange[2])
  pointsData <- tibble(xPoint = c(xRange[1], (xMid-0.1), (xMid+0.1), xRange[2]),
                       yPoint = c(yRange[1], lineData$yLine[lineData$xLine == xMid], lineData$yLine[lineData$xLine == xMid], yRange[2]))
  
  # Connecting the 0 points in the illustration above with the 3rd point that
  # determines curvature gives us a set of 3 points to use to fit an exponential
  # line to the data.
  
  # We fit a linear regression to the log-transformed data to get starting values
  lm.fit <- lm(log(yPoint) ~ xPoint, data = pointsData)
  
  alpha.0  <- exp(coef(lm.fit)[1]) %>% as.numeric()
  beta.0   <- coef(lm.fit)[2] %>% as.numeric()
  theta.0 <- min(pointsData$yPoint) * 0.5  # Why 0.5?
  
  # and then use NLS to fit a better line to the data
  start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
  nonlinear.fit   <- nls(yPoint ~ alpha * exp(beta * xPoint) + theta ,
                         data = pointsData, start = start)
  
  coefficients <- tibble(alphahat = (coef(nonlinear.fit)[1] %>% as.numeric()),
                         betahat  = coef(nonlinear.fit)[2] %>% as.numeric(),
                         thetahat = coef(nonlinear.fit)[3] %>% as.numeric())
  
  return(coefficients)
}

expSim <- function(alphahat, betahat, thetahat, sigma, nReps = 1, N = sample_size, xRange = xRange_vals, yRange = yRange_vals){
  
  alpha = alphahat/(exp(sigma^2/2))
  beta  = betahat
  theta = thetahat
  
  vals <- seq(xRange[1], xRange[2], length.out = N*3/4)
  xvals <- sample(vals, N, replace = T)
  xvals <- jitter(xvals)
  
  expData <- tibble(x = rep(xvals, nReps),
                    y = alpha*exp(beta*x + rnorm(N*nReps,0,sigma)) + theta)
  return(expData)
}

coefData <- tibble(xMid = c(xMid_vals[1], xMid_vals[2], xMid_vals[3])) %>%
  mutate(coefficients = pmap(list(xMid),coefEst)) %>%
  unnest(coefficients)

# Create dataset for parameter table ------------------------------------------------------
parameter_data <- cbind(coefData, sigma_vals) %>%
  mutate(alphatilde = alphahat/(exp(sigma_vals^2/2)),
         difficulty = c("Obvious Curvature", "Noticable Curvature", "Almost Linear")) %>%
  select(difficulty, xMid, sigma_vals, alphahat, alphatilde, betahat, thetahat) 
# write.csv(parameter_data, file = "manuscripts/jsm-2021-student-paper-submission/data/parameter_data.csv", row.names = F)

panelData <- tibble("panel" = c("target", rep("null",19)),
                    ".n" = seq(0,19,1))

set.seed(seed_num)
simData <- tibble(panel       = c("target", "null"),
                   curvature   = c("E", "H"),
                   variability = c("Lv","Lv"),
                   xMid        = c(xMid_vals[1], xMid_vals[2]),
                   sigma       = c(sigma_vals[1], sigma_vals[3])) %>%
            left_join(coefData, by = "xMid") %>% 
            full_join(panelData, by = "panel") %>%
            mutate(data = pmap(list(alphahat, betahat, thetahat,sigma),expSim)) %>%
            unnest(data)

set.seed(68505)
pos_linear        <- sample(1:20, 1)
lineupData_linear <- lineup(true = simData %>%
                       filter(panel == "target") %>%
                       select("x", "y"),
                     samples = simData %>%
                       filter(panel == "null") %>%
                       select("x", "y", ".n"),
                     pos = pos_linear)
# write.csv(lineupData_linear, file = "manuscripts/jsm-2021-student-paper-submission/data/lineupData_linear.csv", row.names = F)
linearPlot <- ggplot(lineupData_linear, aes(x=x, y=y)) +
  facet_wrap(~.sample, ncol=5) +
  geom_point(size = .75) +
  theme(aspect.ratio = 1) +
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
  )

linearPlot
# ggsave(plot = linearPlot, filename = "linear-lineup-example.png", path = here::here("manuscripts", "jsm-2021-student-paper-submission", "images"), device = "png", width = 10, height = 8.3, dpi = 600)

set.seed(1)
pos_log        <- sample(1:20, 1)
lineupData_log <- lineup(true = simData %>%
                           filter(panel == "target") %>%
                           select("x", "y"),
                         samples = simData %>%
                           filter(panel == "null") %>%
                           select("x", "y", ".n"),
                         pos = pos_log)
# write.csv(lineupData_log, file = "manuscripts/jsm-2021-student-paper-submission/data/lineupData_log.csv", row.names = F)
logPlot <- ggplot(lineupData_log, aes(x=x, y=y)) +
  facet_wrap(~.sample, ncol=5) +
  geom_point(size = .75) +
  theme(aspect.ratio = 1) +
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
  ) +
  scale_y_continuous(trans = "log10")
logPlot
# ggsave(plot = logPlot, filename = "log-lineup-example.png", path = here::here("manuscripts", "jsm-2021-student-paper-submission", "images"), device = "png", width = 10, height = 8.3, dpi = 600)

# ----------------------------------------------------------------------------------
# Data Generation Models -----------------------------------------------------------
# ----------------------------------------------------------------------------------
# WITH THETA

expSim <- function(xMid, xRange = xRange_vals, yRange = yRange_vals){
  
  lineData   <- tibble(xLine = seq(xRange[1],xRange[2],0.1),
                       yLine = ((yRange[1]-yRange[2])/(xRange[2]-xRange[1]))*(xLine-xRange[1])+yRange[2])
  pointsData <- tibble(xPoint = c(xRange[1], (xMid-0.1), (xMid+0.1), xRange[2]),
                       yPoint = c(yRange[1], lineData$yLine[lineData$xLine == xMid], lineData$yLine[lineData$xLine == xMid], yRange[2]))
  
  
  lm.fit <- lm(log(yPoint) ~ xPoint, data = pointsData)
  alpha.0  <- exp(coef(lm.fit)[1]) %>% as.numeric()
  beta.0   <- coef(lm.fit)[2] %>% as.numeric()
  theta.0 <- min(pointsData$yPoint) * 0.5  
  start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
  nonlinear.fit   <- nls(yPoint ~ alpha * exp(beta * xPoint) + theta , data = pointsData, start = start)
  alpha <- coef(nonlinear.fit)[1] %>% as.numeric()
  beta <- coef(nonlinear.fit)[2] %>% as.numeric()
  theta <- coef(nonlinear.fit)[3] %>% as.numeric()
  
  expData <- tibble(x = seq(xRange[1],xRange[2],0.1),
                    y = alpha*exp(beta*x) + theta)
  return(list(expData = expData, lineData = lineData, pointsData = pointsData))
}

set.seed(seed_num)
expData <- tibble(Curvature = c("Easy", "Medium", "Hard"),
                  xMid = xMid_vals) %>%
  mutate(Curvature = factor(Curvature, levels = c("Easy", "Medium", "Hard")),
         data = map(xMid, expSim)) %>%
  unnest(data) %>%
  unnest(data) %>%
  mutate(xMid = factor(xMid))

linearPlot <- expData %>%
  # filter(!is.na(y)) %>%
  ggplot(aes(x = x, y = y, group = Curvature, color = Curvature)) + 
  geom_line() +
  scale_color_brewer(palette = "Paired") +
  ggtitle("Linear Scale") +
  scale_x_continuous("", limits = c(0,20), breaks = seq(0,20,5)) + 
  ylab("") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        aspect.ratio = 1)

logPlot <- expData %>%
  # filter(!is.na(y)) %>%
  ggplot(aes(x = x, y = y, group = Curvature, color = Curvature)) + 
  geom_line() +
  scale_color_brewer(palette = "Paired") +
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Log Scale") +
  scale_x_continuous("", limits = c(0,20), breaks = seq(0,20,5)) + 
  ylab("") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        aspect.ratio = 1)

threeCoefPlot <- grid.arrange(linearPlot, logPlot, ncol = 2)

# Heuristic Simulation -----------------------------------------------------------------------
linearPlot_dots_threeParm <- linearPlot + 
  geom_line(aes(x = xLine, y = yLine), color = "gray") +
  geom_point(aes(x = xPoint, y = yPoint), color = "black")

ggsave(plot = linearPlot_dots_threeParm, filename = "heuristic-simulation.png", path = here::here("manuscripts", "jsm-2021-student-paper-submission", "images"), device = "png", width = 10, height = 5, dpi = 600)

# ----------------------------------------------------------------------------------
# Difficulty -----------------------------------------------------------------------
# ----------------------------------------------------------------------------------

coefEst <- function(xMid, xRange = c(0,20), yRange = yRange_vals){
  
  # This creates the line y = -x (scaled to fit the x and y ranges)
  # |*            0
  # |  *
  # |    *
  # |      *
  # |        1
  # |          2
  # |0___________3
  #
  # where 1, 2, 3 represent different points used to determine the line curvature
  
  lineData   <- tibble(xLine = seq(xRange[1],xRange[2],0.1),
                       yLine = -(abs(diff(yRange))/abs(diff(xRange)))*(xLine-xRange[1])+yRange[2])
  pointsData <- tibble(xPoint = c(xRange[1], (xMid-0.1), (xMid+0.1), xRange[2]),
                       yPoint = c(yRange[1], lineData$yLine[lineData$xLine == xMid], lineData$yLine[lineData$xLine == xMid], yRange[2]))
  
  # Connecting the 0 points in the illustration above with the 3rd point that
  # determines curvature gives us a set of 3 points to use to fit an exponential
  # line to the data.
  
  # We fit a linear regression to the log-transformed data to get starting values
  lm.fit <- lm(log(yPoint) ~ xPoint, data = pointsData)
  
  alpha.0  <- exp(coef(lm.fit)[1]) %>% as.numeric()
  beta.0   <- coef(lm.fit)[2] %>% as.numeric()
  theta.0 <- min(pointsData$yPoint) * 0.5  # Why 0.5?
  
  # and then use NLS to fit a better line to the data
  start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
  nonlinear.fit   <- nls(yPoint ~ alpha * exp(beta * xPoint) + theta ,
                         data = pointsData, start = start)
  
  coefficients <- tibble(alphahat = (coef(nonlinear.fit)[1] %>% as.numeric()),
                         betahat  = coef(nonlinear.fit)[2] %>% as.numeric(),
                         thetahat = coef(nonlinear.fit)[3] %>% as.numeric())
  
  return(coefficients)
}

expSim <- function(alphahat, betahat, thetahat, sigma, nReps = 1, N = 50, xRange = c(0,20), yRange = yRange_vals){
  
  alpha = alphahat/(exp(sigma^2/2))
  beta  = betahat
  theta = thetahat
  
  vals <- seq(xRange[1], xRange[2], length.out = N*3/4)
  xvals <- sample(vals, N, replace = T)
  xvals <- jitter(xvals)
  # xvals <- seq(xRange[1], xRange[2], length.out = N)
  
  expData <- tibble(x = rep(xvals, nReps),
                    y = alpha*exp(beta*x + rnorm(N*nReps,0,sigma)) + theta)
  return(expData)
}

# Evaluate Fit 
calcLOF <- 
  function(sim.data){
    lof.mod <- lm(y ~ as.factor(x), data = sim.data)
    lof <- anova(lof.mod) %>% 
      broom::tidy() %>%
      filter(term == "as.factor(x)") %>%
      select(statistic)
    return(lof)
  }

coefData <- tibble(xMid = xMid_vals) %>%
  mutate(coefficients = pmap(list(xMid),coefEst)) %>%
  unnest(coefficients)

#Identify parameters
parmData <- tibble(Curvature   = c("Obvious Curvature", "Noticeable Curvature", "Almost Linear"),
                   Variability = c("Low", "Low", "Low"),
                   xMid        = c(xMid_vals[1], xMid_vals[2], xMid_vals[3]),
                   sigma       = sigma_vals) %>%
  left_join(coefData, by = "xMid")

set.seed(seed_num)
lofData <- parmData %>%
  expand_grid(replicate = seq(1,1000,1)) %>%
  mutate(data = pmap(list(alphahat,betahat,thetahat, sigma,nReps = 10),expSim)) %>%
  mutate(lof = map(data, calcLOF)) %>%
  unnest(lof) %>%
  select(-c(data))
# write.csv(lofData, file = "manuscripts/jsm-2021-student-paper-submission/data/lofData.csv", row.names = F)

# Compare Curvature within Varability
lofPlot_curvature <- lofData %>%
  mutate(Curvature = factor(Curvature, levels = c("Obvious Curvature", "Noticeable Curvature", "Almost Linear"))) %>%
  mutate(Variability = factor(Variability, levels = c("Low"))) %>%
  ggplot(aes(x = statistic, fill = Curvature, color = Curvature)) +
  geom_density(alpha = 0.7) +
  scale_fill_brewer(palette = "Paired") + 
  scale_color_brewer(palette = "Paired") + 
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        aspect.ratio = 1)
lofPlot_curvature
# ggsave(plot = lofPlot_curvature, filename = "lof-curvature.png", path = here::here("manuscripts", "jsm-2021-student-paper-submission", "images"), device = "png", width = 10, height = 5, dpi = 600)
