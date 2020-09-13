library(purrr)
library(tidyverse)
library(nullabor)
library(readr)
library(digest)

# Only works for x starting at 0
# expSim <- function(beta, sigma, N = 30, xRange = c(0,20), yRange = c(1,100)){
#   theta <- (yRange[2]-yRange[1]*(exp(beta*(xRange[2]-xRange[1]))))/(1-exp(beta*xRange[2]+sigma^2/2))
#   alpha <- (yRange[1]-theta)/(exp(beta*xRange[1]+sigma^2/2))
#   expData <- tibble(x = seq(xRange[1],xRange[2], length.out = N),
#                     y = alpha*exp(beta*x + rnorm(N,0,sigma)) + theta)
#   return(expData)
# }


expSim <- function(xMid, sigma, N = 20, xRange = c(0,20), yRange = c(1,100)){

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
  #
  lineData   <- tibble(xLine = seq(xRange[1],xRange[2],0.1),
                       yLine = (abs(diff(yRange))/abs(diff(xRange)))*(xLine-xRange[1])+yRange[2])
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

  alpha <- (coef(nonlinear.fit)[1] %>% as.numeric())/(exp((sigma^2)/2))
  beta <- coef(nonlinear.fit)[2] %>% as.numeric()
  theta <- coef(nonlinear.fit)[3] %>% as.numeric()

  # Then the coefficients are used to generate y values which are reasonable
  expData <- tibble(x = seq(xRange[1],xRange[2], length.out = N),
                    y = alpha*exp(beta*x + rnorm(N,0,sigma)) + theta)
  return(expData)
}

parmData <- tibble(diff.num    = seq(1,6,1),
                   curvature   = c("E", "E", "M", "M", "H", "H"),
                   variability = c("Lv", "Hv", "Lv", "Hv", "Lv", "Hv"),
                   xMid        = c(14.5, 14.5,  13,   13,   11.8,   11.8),
                   sigma       = c(0.25, 0.33, 0.14, 0.18, 0.07, 0.09),)

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
                  select(set, panel, .n, xMid, sigma, param_value, data_name) %>%
                  arrange(param_value, set, .n) %>%
                  mutate(data = map2(xMid,sigma,expSim)) %>%
                  unnest(data)

source("save_lineups.R")
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

  write.csv(lineupData, file = paste("csv_files/", dataID, ".csv", sep = ""), row.names = F)

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

  save_lineup(linearPlot, file = linearID, path = "plots")

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

  save_lineup(logPlot, file = logID, path = "plots")

picture_details[i, "sample_size"]       <- 20
picture_details[i, "param_value"]       <- paramID
picture_details[i, "p_value"]           <- 1
picture_details[i, "obs_plot_location"] <- pos
picture_details[i, "linear"]            <- paste("plots/svgs/", linearID, ".svg", sep = "")
picture_details[i, "log"]               <- paste("plots/svgs/", logID, ".svg", sep = "")
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
                      select("pic_id", "sample_size", "test_param",	"param_value", "p_value",	"obs_plot_location", "pic_name", "experiment", "difficulty", "data_name")

write.csv(picture_details, file = here::here("lineups-pilot-app", "plots", "picture-details.csv"), row.names = F)

