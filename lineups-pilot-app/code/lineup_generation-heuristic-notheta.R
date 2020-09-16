library(tidyverse)
library(nullabor)
library(readr)
library(digest)
library(purrr)

# Obtain alphahat, betahat, and thetahat for different midpoints.
coefEst <- function(xMid, xRange = c(0,20), yRange = c(1,100)){
  
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
  
  # and then use NLS to fit a better line to the data
  start <- list(alpha = alpha.0, beta = beta.0)
  nonlinear.fit   <- nls(yPoint ~ alpha * exp(beta * xPoint),
                         data = pointsData, start = start)
  
  coefficients <- tibble(alphahat = (coef(nonlinear.fit)[1] %>% as.numeric()),
                         betahat  = coef(nonlinear.fit)[2] %>% as.numeric())

  return(coefficients)
}

expSim <- function(alphahat, betahat, sigma, nReps = 1, N = 20, xRange = c(0,20), yRange = c(1,100)){
  
  alpha = alphahat/(exp(sigma^2/2))
  beta  = betahat
  
  vals <- seq(xRange[1], xRange[2], length.out = N*3/4)
  xvals <- sample(vals, (N-2), replace = T)
  xvals <- jitter(c(xRange[1],xvals,xRange[2]))
  
  expData <- tibble(x = rep(xvals, nReps),
                    y = alpha*exp(beta*x + rnorm(N*nReps,0,sigma)))
  return(expData)
}

# Calculate coefficient estimates
coefData <- tibble(xMid = c(9, 13, 15.8)) %>%
  mutate(coefficients = pmap(list(xMid),coefEst)) %>%
  unnest(coefficients)

# Identify parameters
parmData <- tibble(diff.num    = seq(1,6,1),
                   curvature   = c("E", "E", "M", "M", "H", "H"),
                   variability = c("Lv", "Hv", "Lv", "Hv", "Lv", "Hv"),
                   xMid        = c(15.8, 15.8, 13, 13, 9,  9),
                   sigma       = c(0.25, 0.37, 0.12, 0.2, 0.06, 0.1)
                   ) %>%
            left_join(coefData, by = "xMid")
parmData

trtData <- expand_grid(target = seq(1,6,1),
                       null   = seq(1,6,1)) %>%
  as_tibble() %>%
  mutate(difficulty = seq(1,36,1)) %>%
  left_join(parmData, by = c("target" = "diff.num")) %>%
  left_join(parmData, by = c("null" = "diff.num"), suffix = c(".target", ".null")) %>%
  mutate(rorschach = ifelse(target == null, 1, 0),
         param_value = paste("target-", curvature.target, "-", variability.target, "_null-", curvature.null, "-", variability.null, "_r", rorschach, sep = "")) %>%
  filter(curvature.target == curvature.null | (curvature.target != curvature.null & variability.target == variability.null), rorschach == 0) %>%
  expand_grid(set = seq(1,3,1)) %>%
  mutate(data_name = paste("set", set, "-", param_value, sep = ""))

panelData <- tibble("panel" = c("target", rep("null",19)),
                    ".n" = seq(0,19,1))

set.seed(56156)
simulatedData <- expand_grid(target = seq(1,6,1),
                             null = seq(1,6,1)) %>%
  as_tibble() %>%
  mutate(difficulty = seq(1,36,1)) %>%
  pivot_longer(cols = c("target", "null"),
               names_to = "panel",
               values_to = "diff.num") %>%
  left_join(parmData, by = "diff.num") %>%
  full_join(panelData, by = "panel") %>%
  right_join(trtData, by = "difficulty") %>%
  select(set, panel, .n, alphahat, betahat, sigma, param_value, data_name) %>%
  arrange(param_value, set, .n) %>%
  mutate(data = pmap(list(alphahat,betahat,sigma), expSim)) %>%
  unnest(data)

source(here::here("lineups-pilot-app", "code", "save_lineups.R"))
picture_details <- matrix(NA, nrow = nrow(trtData), ncol = 9) %>% as.data.frame()

colnames(picture_details) <- c("sample_size", "param_value", "p_value",
                               "obs_plot_location", "linear", "log",
                               "experiment", "difficulty", "data_name")

for(i in 1:nrow(trtData)){
  setID        <- trtData[i, "set"] %>% as.numeric()
  paramID      <- trtData[i, "param_value"] %>% as.character()
  dataID       <- digest(paste(trtData[i, "data_name"] %>% as.character(), Sys.time()), "md5", serialize = F)
  linearID     <- digest(paste(trtData[i, "data_name"] %>% as.character(), "-linear_", Sys.time(), sep = ""), "md5", serialize = F)
  logID        <- digest(paste(trtData[i, "data_name"] %>% as.character(), "-log_", Sys.time(), sep = ""), "md5", serialize = F)
  difficultyID <- trtData[i, "difficulty"] %>% as.numeric()
  pos          <- sample(1:20, 1)
  
  lineupData <- lineup(true = simulatedData %>%
                         filter(panel == "target", set == setID, param_value == paramID) %>%
                         select("x", "y"),
                       samples = simulatedData %>%
                         filter(panel == "null", set == setID, param_value == paramID) %>%
                         select("x", "y", ".n"),
                       pos = pos)
  
  write.csv(lineupData, file = here::here("lineups-pilot-app", "plots", "data", paste(dataID, ".csv", sep = "")), row.names = F)
  
  linearPlot <- ggplot(lineupData, aes(x=x, y=y)) +
    facet_wrap(~.sample, ncol=5) +
    geom_point() +
    theme(aspect.ratio = 1) +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y  = element_blank(),
          axis.text.x  = element_blank(),
    )
  
  save_lineup(linearPlot, file = linearID, path = here::here("lineups-pilot-app", "plots"), width = 15, height = 12.5, dpi = 300)
  
  logPlot <- ggplot(lineupData, aes(x=x, y=y)) +
    facet_wrap(~.sample, ncol=5) +
    geom_point() +
    theme(aspect.ratio = 1) +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y  = element_blank(),
          axis.text.x  = element_blank(),
    ) +
    scale_y_continuous(trans = "log10")
  logPlot
  save_lineup(logPlot, file = logID, path = here::here("lineups-pilot-app", "plots"), width = 15, height = 12.5, dpi = 300 )
  
  picture_details[i, "sample_size"]       <- 20
  picture_details[i, "param_value"]       <- paramID
  picture_details[i, "p_value"]           <- 1
  picture_details[i, "obs_plot_location"] <- pos
  picture_details[i, "linear"]            <- paste("plots/svg/", linearID, ".svg", sep = "")
  picture_details[i, "log"]               <- paste("plots/svg/", logID, ".svg", sep = "")
  picture_details[i, "experiment"]        <- "emily-log-1"
  picture_details[i, "difficulty"]        <- difficultyID
  picture_details[i, "data_name"]         <- dataID
}

picture_details <- picture_details %>%
  pivot_longer(cols = c("linear", "log"),
               names_to = "test_param",
               values_to = "pic_name") %>%
  mutate(pic_id = seq(1, nrow(trtData)*2),
         difficulty = ifelse(test_param == "linear", (100 + difficulty), (200 + difficulty))) %>%
  select("pic_id", "sample_size", "test_param",	"param_value", "p_value",	
         "obs_plot_location", "pic_name", "experiment", "difficulty", "data_name")

write.csv(picture_details, file = here::here("lineups-pilot-app", "plots", "picture-details.csv"), row.names = F)


simulation_details <- tibble(simulation_method = "heuristic",
                             model = "alpha*exp^[beta*x+epsilon]")
write.table(simulation_details, file = here::here("lineups-pilot-app", "plots", "simulation-details.txt"), row.names = F)
