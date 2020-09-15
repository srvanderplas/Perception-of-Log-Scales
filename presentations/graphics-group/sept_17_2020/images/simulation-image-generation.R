
# -----------------------------------------------------------------------------
# Algebraic Simulation (no theta) ---------------------------------------------
# -----------------------------------------------------------------------------

expSim <- function(beta, xRange = c(1,20), yRange = c(1,100)){
  alpha   <- (yRange[2]/exp(beta*xRange[2]))
  expData <- tibble(x = seq(xRange[1],xRange[2],1),
                    y = alpha*exp(beta*x))
  return(expData)
}

expData <- tibble(beta = c(0.07, 0.15, 0.3)) %>%
  mutate(data = map(beta, expSim)) %>%
  unnest(data) %>%
  mutate(beta = factor(beta))

linearPlot2 <- expData %>%
  ggplot(aes(x = x, y = y, group = beta, color = beta)) + 
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  ggtitle("Linear Scale")

logPlot2 <- expData %>%
  ggplot(aes(x = x, y = y, group = beta, color = beta)) + 
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))+
  ggtitle("Log Scale")
grid.arrange(linearPlot2, logPlot2, ncol = 2)

# -----------------------------------------------------------------------------
# Algebraic Simulation + Theta ------------------------------------------------
# -----------------------------------------------------------------------------

expSim <- function(beta, xRange = c(0,20), yRange = c(1,100)){
  theta <- (yRange[2]-yRange[1]*(exp(beta*(xRange[2]-xRange[1]))))/(1-exp(beta*xRange[2]))
  alpha <- (yRange[1]-theta)/(exp(beta*xRange[1]))
  expData <- tibble(x = seq(xRange[1],xRange[2],1),
                    y = alpha*exp(beta*x)+theta)
  return(expData)
}

expData <- tibble(beta = c(0.07, 0.15, 0.3)) %>%
  mutate(data = map(beta, expSim)) %>%
  unnest(data) %>%
  mutate(beta = factor(beta))

linearPlot4 <- expData %>%
  ggplot(aes(x = x, y = y, group = beta, color = beta)) + 
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  ggtitle("Linear Scale")

logPlot4 <- expData %>%
  ggplot(aes(x = x, y = y, group = beta, color = beta)) + 
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Log Scale")
grid.arrange(linearPlot4, logPlot4, ncol = 2)



# -----------------------------------------------------------------------------
# Heuristic Simulation + Theta ------------------------------------------------
# -----------------------------------------------------------------------------

expSim <- function(xMid, xRange = c(0,20), yRange = c(1,100)){
  
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

expData <- tibble(xMid = c(11.8, 13, 14.5)) %>%
  mutate(data = map(xMid, expSim)) %>%
  unnest(data) %>%
  unnest(data) %>%
  mutate(xMid = factor(xMid))

linearPlot3 <- expData %>%
  ggplot(aes(x = x, y = y, group = xMid, color = xMid)) + 
  geom_line() +
  geom_point(aes(x = xPoint, y = yPoint), color = "black") +
  geom_line(aes(x = xLine, y = yLine), color = "gray") +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  ggtitle("Linear Scale")

logPlot3 <- expData %>%
  ggplot(aes(x = x, y = y, group = xMid, color = xMid)) + 
  geom_line() +
  geom_point(aes(x = xPoint, y = yPoint), color = "black") +
  geom_line(aes(x = xLine, y = yLine), color = "gray") +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Log Scale")
grid.arrange(linearPlot3, logPlot3, ncol = 2)
