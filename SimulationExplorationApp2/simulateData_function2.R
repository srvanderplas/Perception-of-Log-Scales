require(tidyverse)
require(gridExtra)
require(scales)
library(purrr)

# Exponential ---------------------------------------------------------------------------------------
simulate.exponential <- 
  function(N, xRange = c(1,N), yRange = c(0,10), nReps, beta, theta = 0, muErr = 0, sdErr, errorType, ...){
    
    exp.data <- data.frame(x = rep(seq(xRange[1], xRange[2], length.out = N), nReps), y = NA)
    
    theta <- 0
    
    if(errorType %in% c("Mult", "mult", "Multiplicative", "multiplicative")){

      # This standardizes for different sdErr but not betas
      # alpha <- 1/(exp((sdErr^2)/2))
      
      # Different beta's have different starting values
      # alpha <- (yRange[2]/exp(beta*xRange[2]))/(exp((sdErr^2)/2))
      
      # Looks good on linear scale, but throws off log scale.
      theta <- (yRange[2]-yRange[1]*(exp(beta*(xRange[2]-xRange[1]))))/(1-exp(beta*xRange[2]+sdErr^2/2))
      alpha <- (yRange[1]-theta)/(exp(beta*xRange[1]+sdErr^2/2))
      
      exp.data$y <- alpha*exp(beta*exp.data$x + rnorm(N*nReps, muErr, sdErr)) + theta
    } else { 
      alpha <- 1
      if(errorType %in% c("Add", "add", "Additive", "additive")){
        exp.data$y <- alpha*exp(beta*exp.data$x) + theta + rnorm(N*nReps, muErr, sdErr)
      }
    }
    
    return(exp.data)
  }

# Quadratic ------------------------------------------------------------------------

simulate.quadratic <- 
  function(N, nReps, xRange = c(1,N), beta0, beta1, beta2, muErr = 0, sdErr, ...){
    
    quad.data <- data.frame(x = rep(seq(xRange[1], xRange[2], length.out = N), nReps), y = NA)
    quad.data$y <- beta0 + beta1*quad.data$x + beta2*quad.data$x^2 + rnorm(N*nReps, muErr, sdErr)
    
    return(quad.data)
  }

# Linear ------------------------------------------------------------------------

simulate.linear <- 
  function(N, nReps, xRange = c(1,N), beta0, beta1, muErr = 0, sdErr, ...){
    
    lin.data <- data.frame(x = rep(seq(xRange[1], xRange[2], length.out = N), nReps), y = NA)
    lin.data$y <- beta0 + beta1*lin.data$x + rnorm(N*nReps, muErr, sdErr)
    
    return(lin.data)
  }

# Overall Simulation Function ----------------------------------------------------------------

simulate.data <- 
  function(simulateFunction, ...){
    sim.data <- simulateFunction(...)
  }

# Trial -------------------------------------------------------------------

# sim.data1 <- simulate.data(simulateFunction = simulate.exponential, 
#                           N = 30, 
#                           nReps = 1,
#                           xRange = c(1,20),
#                           
#                           # Exponential Parameters
#                           beta = 0.3, 
# 
#                           # Quadratic Parameters
#                           beta0 = 0,
#                           beta1 = 0, 
#                           beta2 = 0.1,
#                           
#                           muErr = 0, 
#                           sdErr = 0.1,
#                           errorType = "mult")

# lin.plot1 <- sim.data1 %>%
#   ggplot(aes(x = x, y = y)) +
#   geom_point(shape = 1) +
#   scale_color_brewer(palette = "Paired") +
#   theme_bw() +
#   ggtitle("Linear: Low Variance")
# 
# log.plot1 <- sim.data1 %>%
#   ggplot(aes(x = x, y = y)) +
#   geom_point(shape = 1) +
#   scale_y_continuous(trans = "log10",
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x))) +
#   scale_color_brewer(palette = "Paired") +
#   theme_bw() +
#   ggtitle("Log: Low Variance")
# 
# sim.data2 <- simulate.data(simulateFunction = simulate.exponential, 
#                            N = 30, 
#                            nReps = 1,
#                            xRange = c(1,20),
#                            
#                            # Exponential Parameters
#                            beta = 0.3, 
#                            
#                            # Quadratic Parameters
#                            beta0 = 0,
#                            beta1 = 0, 
#                            beta2 = 0.1,
#                            
#                            muErr = 0, 
#                            sdErr = 0.2,
#                            errorType = "mult")
# 
# lin.plot2 <- sim.data2 %>%
#   ggplot(aes(x = x, y = y)) +
#   geom_point(shape = 1) +
#   scale_color_brewer(palette = "Paired") +
#   theme_bw() +
#   ggtitle("Linear: High Variance")
# 
# log.plot2 <- sim.data2 %>%
#   ggplot(aes(x = x, y = y)) +
#   geom_point(shape = 1) +
#   scale_y_continuous(trans = "log10",
#                      breaks = trans_breaks("log10", function(x) 10^x),
#                      labels = trans_format("log10", math_format(10^.x))) +
#   scale_color_brewer(palette = "Paired") +
#   theme_bw() +
#   ggtitle("Log: High Variance")
# 
# grid.arrange(lin.plot1, log.plot1, lin.plot2, log.plot2, ncol=2)
 
# Lack of Fit

calcLOF <-
  function(sim.data){
    if(nrow(sim.data)/length(unique(sim.data$x)) > 1){
      lof.mod <- lm(y ~ as.factor(x), data = sim.data)
      lof <- anova(lof.mod) %>%
        broom::tidy() %>%
        filter(term == "as.factor(x)") %>%
        select(statistic)
    } else {
      lof.mod <- NULL
      lof <- NULL
    }
    return(lof)
  }
