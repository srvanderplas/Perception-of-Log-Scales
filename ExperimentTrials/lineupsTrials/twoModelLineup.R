library(purrr)
library(tidyverse)
library(nullabor)
library(readr)

n = 15

beta.target = 0.07
beta.null = 0.15

xMid.target = 12
xMid.null = 13.5

sigma.target = 0.08
sigma.null = 0.12

# -----------------------------------------------------------------------------------------------------
# ORIGINAL --------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------

# With theta
expSim <- function(beta, sigma, N = n, xRange = c(0,20), yRange = c(1,100)){
  theta <- (yRange[2]-yRange[1]*(exp(beta*(xRange[2]-xRange[1]))))/(1-exp(beta*xRange[2]+sigma^2/2))
  alpha <- (yRange[1]-theta)/(exp(beta*xRange[1]+sigma^2/2))
  expData <- tibble(x = seq(xRange[1],xRange[2], length.out = N),
                    y = alpha*exp(beta*x + rnorm(N,0,sigma)) + theta)
  return(expData)
}

# Without theta
expSim <- function(beta, sigma, N = n, xRange = c(1,20), yRange = c(1,100)){
  alpha <- (yRange[2]/exp(beta*xRange[2]))/(exp((sigma^2)/2))
  expData <- tibble(x = seq(xRange[1],xRange[2], length.out = N),
                    y = alpha*exp(beta*x + rnorm(N,0,sigma)))
  return(expData)
}

# Create dataset
true <- cbind(group = rep(1,n), expSim(beta = beta.target, sigma = sigma.target)) %>%
        rbind(cbind(group = rep(2,n), expSim(beta = beta.null, sigma = sigma.null))) %>%
        mutate(group = factor(group))

all.sim <- data.frame(x=NULL, y=NULL, .n=NULL)
for (i in 1:19) {
  simDat <- cbind(group = rep(1,n), expSim(beta = beta.null, sigma = sigma.null)) %>%
            rbind(cbind(group = rep(2,n), expSim(beta = beta.null, sigma = sigma.null))) %>%
            mutate(group = factor(group))
  sim <- cbind(simDat, .n=i)
  all.sim <- rbind(all.sim, sim)
}

# -----------------------------------------------------------------------------------------------------
# HEURISTICALLY ---------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------

# With theta
expSim <- function(xMid, sigma, N = n, xRange = c(0,20), yRange = c(1,100)){
  
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

# Without theta
expSim <- function(xMid, sigma, N = n, xRange = c(1,20), yRange = c(1,100)){
  
  lineData   <- tibble(xLine = seq(xRange[1],xRange[2],0.1),
                       yLine = ((yRange[1]-yRange[2])/(xRange[2]-xRange[1]))*(xLine-xRange[1])+yRange[2])
  pointsData <- tibble(xPoint = c(xRange[1], xMid, xRange[2]),
                       yPoint = c(yRange[1], lineData$yLine[lineData$xLine == xMid], yRange[2]))
  
  
  lm.fit <- lm(log(yPoint) ~ xPoint, data = pointsData)
  alpha.0  <- exp(coef(lm.fit)[1]) %>% as.numeric()
  beta.0   <- coef(lm.fit)[2] %>% as.numeric()
  start <- list(alpha = alpha.0, beta = beta.0)
  nonlinear.fit   <- nls(yPoint ~ alpha * exp(beta * xPoint), data = pointsData, start = start)
  alpha <- (coef(nonlinear.fit)[1] %>% as.numeric())/(exp((sigma^2)/2))
  beta <- coef(nonlinear.fit)[2] %>% as.numeric()
  
  expData <- tibble(x = seq(xRange[1],xRange[2], length.out = N),
                    y = alpha*exp(beta*x + rnorm(N,0,sigma)))
  
  return(expData)
}

# Create dataset
true <- cbind(group = rep(1,n), expSim(xMid = xMid.target, sigma = sigma.target)) %>%
        rbind(cbind(group = rep(2,n), expSim(xMid = xMid.null, sigma = sigma.null))) %>%
        mutate(group = factor(group))

all.sim <- data.frame(x=NULL, y=NULL, .n=NULL)
for (i in 1:19) {
  simDat <- cbind(group = rep(1,n), expSim(xMid = xMid.null, sigma = sigma.null)) %>%
    rbind(cbind(group = rep(2,n), expSim(xMid = xMid.null, sigma = sigma.null))) %>%
    mutate(group = factor(group))
  sim <- cbind(simDat, .n=i)
  all.sim <- rbind(all.sim, sim)
}

# CREATE LINEUP ---------------------------------------------------------------------------------------

pos <- sample(1:20, 1)
d <- lineup(true=true, samples=all.sim, pos=pos)

ggplot(d, aes(x=x, y=y, color = group)) + 
  facet_wrap(~.sample, ncol=5) +
  geom_point(alpha = 0.7) + 
  theme(aspect.ratio = 1) +
  theme_bw() +
theme(axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y  = element_blank(),
      axis.text.x  = element_blank(),
     )

ggplot(d, aes(x=x, y=y, color = group)) + 
  facet_wrap(~.sample, ncol=5) +
  geom_point(alpha = 0.7) + 
  theme(aspect.ratio=1) +
  theme_bw() +
  scale_y_continuous(trans = "log10") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
  )

attr(d, "pos")
