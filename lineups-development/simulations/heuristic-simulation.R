library(purrr)

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
  theta.0 <- min(pointsData$yPoint) * 0.5  # Why 0.5?
  
  # and then use NLS to fit a better line to the data
  start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
  nonlinear.fit   <- nls(yPoint ~ alpha * exp(beta * xPoint) + theta ,
                         data = pointsData, start = start)
  
  coefficients <- tibble(alpha = (coef(nonlinear.fit)[1] %>% as.numeric()),
                         beta  = coef(nonlinear.fit)[2] %>% as.numeric(),
                         theta = coef(nonlinear.fit)[3] %>% as.numeric())

  return(coefficients)
}

midpoints <- tibble(xMid = c(11.8, 13, 14.5)) %>%
             mutate(coefficients = pmap(list(xMid),coefEst)) %>%
             unnest(coefficients)

expSim <- function(alphahat, betahat, thetahat, sigma, N = 20, xRange = c(0,20), yRange = c(1,100)){
  alpha = alphahat/(exp(sigma^2/2))
  beta  = betahat
  theta = thetahat
  expData <- tibble(x = seq(xRange[1],xRange[2], length.out = N),
                    y = alpha*exp(beta*x + rnorm(N,0,sigma)) + theta)
  return(expData)
}
