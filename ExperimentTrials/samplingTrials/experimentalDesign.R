library(purrr)
library(tidyverse)
library(nullabor)
library(readr)

setwd("~/GitHub/Perception-of-Log-Scales/ExperimentTrials/samplingTrials")

# Only works for x starting at 0
# expSim <- function(beta, sigma, N = 30, xRange = c(0,20), yRange = c(1,100)){
#   theta <- (yRange[2]-yRange[1]*(exp(beta*(xRange[2]-xRange[1]))))/(1-exp(beta*xRange[2]+sigma^2/2))
#   alpha <- (yRange[1]-theta)/(exp(beta*xRange[1]+sigma^2/2))
#   expData <- tibble(x = seq(xRange[1],xRange[2], length.out = N),
#                     y = alpha*exp(beta*x + rnorm(N,0,sigma)) + theta)
#   return(expData)
# }

expSim <- function(xMid, sigma, N = 20, xRange = c(0,20), yRange = c(1,100)){
  
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
  alpha <- (coef(nonlinear.fit)[1] %>% as.numeric())/(exp((sigma^2)/2))
  beta <- coef(nonlinear.fit)[2] %>% as.numeric()
  theta <- coef(nonlinear.fit)[3] %>% as.numeric()
  
  expData <- tibble(x = seq(xRange[1],xRange[2], length.out = N),
                    y = alpha*exp(beta*x + rnorm(N,0,sigma)) + theta)
  return(expData)
}

parmData <- tibble(diff.num    = seq(1,6,1),
                   curvature   = c("Easy", "Easy", "Medium", "Medium", "Hard", "Hard"),
                   variability = c("Low",  "High", "Low",    "High",   "Low",  "High"),
                   beta        = c(0.3, 0.3, 0.15, 0.15, 0.07, 0.07),
                   xMid        = c(14,  14,  13,   13,   12,   12),
                   sigma       = c(0.3, 0.4, 0.12, 0.17, 0.05, 0.07),)

trtData <- expand_grid(target = seq(1,6,1),
                       null   = seq(1,6,1)) %>%
           as_tibble() %>%
           mutate(trt.num = seq(1,36,1)) %>%
           expand_grid(scale = c("linear", "log")) %>%
           left_join(parmData, by = c("target" = "diff.num")) %>%
           left_join(parmData, by = c("null" = "diff.num"), suffix = c(".target", ".null")) %>%
           mutate(rorschach = ifelse(target == null, 1, 0)) %>%
           filter(curvature.target == curvature.null | (curvature.target != curvature.null & variability.target == variability.null))

trt.rorschach <- trtData %>%
                  filter(rorschach == 1) %>%
                  select(trt.num) %>%
                  unique()

trt <- trtData %>%
          filter(rorschach == 0) %>%
          select(trt.num) %>%
          unique() %>%
          as.vector()

ibdData <- read.csv("ibdData.csv") %>% 
            as_tibble() %>%
            expand_grid(scale = c("linear", "log"))
  
panelData <- tibble("panel" = c("target", rep("null",19)),
                    ".n" = seq(0,19,1))

factorialData <- expand_grid(target = seq(1,6,1),
                             null = seq(1,6,1)) %>%
                  as_tibble() %>%
                  mutate(trt.num = seq(1,36,1)) %>%
                  pivot_longer(cols = c("target", "null"),
                               names_to = "panel",
                               values_to = "diff.num") %>%
                  left_join(parmData, by = "diff.num") %>%
                  full_join(panelData, by = "panel") %>%
                  # expand_grid(scale = c("linear", "log")) %>%
                  right_join(ibdData, by = "trt.num") %>%
                  select(block, trt.num, scale, panel, .n, diff.num, curvature, variability, beta, sigma, xMid) %>%
                  arrange(block, trt.num, scale) %>%
                  # mutate(data = map2(beta,sigma,expSim)) %>%
                  mutate(data = map2(xMid,sigma,expSim)) %>%
                  unnest(data)

pictureDetails <- matrix(NA, nrow = nrow(ibdData)*2, ncol = 7) %>% as.data.frame()
colnames(pictureDetails) <- c("pic.id", "block", "trt.num", "scale", "data.name", "pic.name", "target.pos")
for(i in 1:nrow(ibdData)){
  blockID <- ibdData[i, "block"] %>% as.numeric()
  trtID   <- ibdData[i, "trt.num"] %>% as.numeric()
  scaleID <- ibdData[i, "scale"] %>% as.character()
  pos     <- sample(1:20, 1)

  lineupData <- lineup(true = factorialData %>% 
                                  filter(panel == "target", block == blockID, trt.num == trtID, scale == scaleID) %>%
                                  select("x", "y"), 
                     samples = factorialData %>% 
                                 filter(panel == "null", block == blockID, trt.num == trtID, scale == scaleID) %>%
                                 select("x", "y", ".n"), 
                     pos = pos)
  
  write.csv(lineupData, file = paste("data/linupData_", i, ".csv", sep = ""))

  linupPlot <- ggplot(lineupData, aes(x=x, y=y)) + 
                  facet_wrap(~.sample, ncol=5) +
                  geom_point() + 
                  theme(aspect.ratio = 1) +
                  theme_bw() +
                  theme(axis.title.y = element_blank(),
                        axis.title.x = element_blank(),
                        axis.text.y  = element_blank(),
                        axis.text.x  = element_blank(),
                       )
  if(scaleID == "log"){
    linupPlot <- lineupPlot + scale_y_continuous(trans = "log10")
  }

  save(linearLineup, file = paste("plots/linearLineup_", i, ".png", sep = ""))
  
pictureDetails[i, "pic.id"]     <- i
pictureDetails[i, "block"]      <- blockID
pictureDetails[i, "trt.num"]    <- trtID
pictureDetails[i, "scale"]      <- scaleID
pictureDetails[i, "data.name"]  <- paste("data/linupData_", i, ".csv", sep = "")
pictureDetails[i, "pic.name"]   <- paste("../plots/linearLineup_", i, ".png", sep = "")
pictureDetails[i, "target.pos"] <- pos
}

pictureDetails <- pictureDetails %>%
                    left_join(trtData, by = "trt.num")
pictureDetails

write.csv(pictureDetails, file = "details/picture-details.csv", row.names = F)

