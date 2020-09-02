library(purrr)
library(tidyverse)
library(nullabor)
library(readr)

setwd("~/GitHub/Perception-of-Log-Scales/ExperimentTrials/lineupsTrials")

# Only works for x starting at 0
expSim <- function(beta, sigma, N = 30, xRange = c(0,20), yRange = c(1,100)){
  theta <- (yRange[2]-yRange[1]*(exp(beta*(xRange[2]-xRange[1]))))/(1-exp(beta*xRange[2]+sigma^2/2))
  alpha <- (yRange[1]-theta)/(exp(beta*xRange[1]+sigma^2/2))
  expData <- tibble(x = seq(xRange[1],xRange[2], length.out = N),
                    y = alpha*exp(beta*x + rnorm(N,0,sigma)) + theta)
  return(expData)
}

parmData <- tibble("diff.num" = seq(1,6,1),
                   "diff.name" = c("Easy-Low", "Easy-High",
                                   "Medium-Low", "Medium-High",
                                   "Hard-Low", "Hard-High"),
                   "beta" = c(0.3, 0.3, 0.15, 0.15, 0.07, 0.07),
                   "sigma" = c(0.3, 0.4, 0.12, 0.17, 0.05, 0.07))

panelData <- tibble("panel" = c("target", rep("null",19)),
                    ".n" = seq(0,19,1))

factorialData <- expand.grid(target = seq(1,6,1),
                             null = seq(1,6,1)) %>%
                  as_tibble() %>%
                  mutate(trt.num = seq(1,36,1)) %>%
                  pivot_longer(cols = c("target", "null"),
                               names_to = "panel",
                               values_to = "diff.num") %>%
                  left_join(parmData, by = "diff.num") %>%
                  full_join(panelData, by = "panel") %>%
                  mutate(data = map2(beta,sigma,expSim)) %>%
                  unnest(data)

trtData <- factorialData %>% 
                  select(trt.num, panel, diff.name) %>%
                  unique() %>%
                  pivot_wider(id_cols = c("trt.num"),
                              names_from = "panel",
                              values_from = "diff.name") %>%
                  left_join(parmData, by = c("target" = "diff.name")) %>%
                  left_join(parmData, by = c("null" = "diff.name"), suffix = c(".target", ".null")) %>%
                  select(trt.num, target, beta.target, sigma.target, null, beta.null, sigma.null)

# write.csv(trtData, file = "trtData.csv", row.names = F)

pictureDetails <- matrix(NA, nrow = 36, ncol = 6) %>% as.data.frame()
colnames(pictureDetails) <- c("pic.id", "trt.num", "data.name", "linearPic.name", "logPic.name", "target.pos")
for(i in 1:36){

trt <- trtData[i, "trt.num"] %>% as.numeric()
pos <- sample(1:20, 1)

lineupData <- lineup(true = factorialData %>% 
                                  filter(panel == "target", trt.num == trt) %>%
                                  select("x", "y"), 
                     samples = factorialData %>% 
                                 filter(panel == "null", trt.num == trt) %>%
                                 select("x", "y", ".n"), 
                     pos = pos)
write.csv(lineupData, file = paste("data/linupData_", i, ".csv", sep = ""))

linearLineup <- ggplot(lineupData, aes(x=x, y=y)) + 
  facet_wrap(~.sample, ncol=5) +
  geom_point() + 
  theme(aspect.ratio = 1) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
       )
save(linearLineup, file = paste("plots/linearLineup_", i, ".png", sep = ""))

logLineup <- ggplot(lineupData, aes(x=x, y=y)) + 
  facet_wrap(~.sample, ncol=5) +
  geom_point() + 
  theme(aspect.ratio = 1) +
  theme_bw() +
  scale_y_continuous(trans = "log10") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
  )
save(logLineup, file = paste("plots/logLineup_", i, ".png", sep = ""))

pictureDetails[i, "pic.id"]  <- i
pictureDetails[i, "trt.num"] <- trt 
pictureDetails[i, "data.name"] <- paste("data/linupData_", i, ".Rdata", sep = "")
pictureDetails[i, "linearPic.name"] <- paste("../plots/linearLineup_", i, ".png", sep = "")
pictureDetails[i, "logPic.name"] <- paste("plots/logLineup_", i, ".png", sep = "")
pictureDetails[i, "target.pos"] <- pos
}

pictureDetails <- pictureDetails %>%
                    left_join(trtData, by = "trt.num")
pictureDetails

write.csv(pictureDetails, file = "details/picture-details.csv", row.names = F)

