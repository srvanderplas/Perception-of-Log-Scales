library(tidyverse)
library(nullabor)
library(readr)
library(digest)
library(purrr)

# ALGEBRAIC SIMULATION (+ theta)
# Only works for x starting at 0
expSim <- function(beta, sigma, nReps = 1, N = 20, xRange = c(0,20), yRange = c(1,100)){
  theta <- (yRange[2]-yRange[1]*(exp(beta*(xRange[2]-xRange[1]))))/(1-exp(beta*xRange[2]+sigma^2/2))
  alpha <- (yRange[1]-theta)/(exp(beta*xRange[1]+sigma^2/2))
  
  vals <- seq(xRange[1], xRange[2], length.out = N*3/4)
  xvals <- sample(vals, N, replace = T)
  xvals <- jitter(xvals)
  # xvals <- seq(xRange[1], xRange[2], length.out = N)
  
  expData <- tibble(x = rep(xvals, nReps),
                    y = alpha*exp(beta*x + rnorm(N*nReps,0,sigma)) + theta)
  return(expData)
}

# ALGEBRAIC SIMULATION (- theta)
# expSim <- function(beta, sigma, nReps = 1, N = 20, xRange = c(0,20), yRange = c(1,100)){
#   alpha   <- (yRange[2]/exp(beta*xRange[2]))
#   
#   vals <- seq(xRange[1], xRange[2], length.out = N*3/4)
#   xvals <- sample(vals, N, replace = T)
#   xvals <- jitter(xvals)
#   # xvals <- seq(xRange[1], xRange[2], length.out = N)
#   
#   expData <- tibble(x = rep(xvals, nReps),
#                     y = alpha*exp(beta*x + rnorm(N*nReps,0,sigma)))
#   return(expData)
# }

parmData <- tibble(diff.num    = seq(1,6,1),
                   curvature   = c("E", "E", "M", "M", "H", "H"),
                   variability = c("Lv", "Hv", "Lv", "Hv", "Lv", "Hv"),
                   beta        = c(0.3, 0.3, 0.15, 0.15, 0.07,  0.07),
                   sigma       = c(0.22, 0.3, 0.14, 0.18, 0.07, 0.09)
                   )

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
                  select(set, panel, .n, beta, sigma, param_value, data_name) %>%
                  arrange(param_value, set, .n) %>%
                  mutate(data = pmap(list(beta,sigma),expSim)) %>%
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
  linearPlot
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

