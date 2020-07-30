require(tidyverse)
require(gridExtra)
require(scales)
library(purrr)

# Exponential ---------------------------------------------------------------------------------------
simulate.exponential <- 
  function(N, xRange = c(1,N), nReps, beta, maxMag, muErr, sdErr, errorType, ...){
    
    exp.data <- data.frame(x = rep(seq(xRange[1], xRange[2], length.out = N), nReps), y = NA)
    
    alpha <- maxMag/(exp(beta*xRange[2]))
    theta <- 0
    
    if(errorType %in% c("Mult", "mult", "Multiplicative", "multiplicative")){
      exp.data$y <- alpha*exp(beta*exp.data$x + rnorm(N*nReps, muErr, sdErr)) + theta
    } else { 
      if(errorType %in% c("Add", "add", "Additive", "additive")){
        exp.data$y <- alpha*exp(beta*exp.data$x) + theta + rnorm(N*nReps, muErr, sdErr)
      }
    }
    
    return(exp.data)
  }

# Quadratic ------------------------------------------------------------------------

simulate.quadratic <- 
  function(N, nReps, xRange = c(1,N), beta0, beta1, beta2, muErr, sdErr, ...){
    
    quad.data <- data.frame(x = rep(seq(xRange[1], xRange[2], length.out = N), nReps), y = NA)
    quad.data$y <- beta0 + beta1*quad.data$x + beta2*quad.data$x^2 + rnorm(N*nReps, muErr, sdErr)
    
    return(quad.data)
  }

# Overall Simulation Function ----------------------------------------------------------------

simulate.data <- 
  function(simulateFunction, ...){
    sim.data <- simulateFunction(...)
  }

# Trial -------------------------------------------------------------------

sim.data <- simulate.data(simulateFunction = simulate.exponential, 
                          N = 30, 
                          nReps = 1,
                          xRange = c(1,30), 
                          maxMag = 1000,
                          
                          # Exponential Parameters
                          beta = 0.03, 

                          # Quadratic Parameters
                          beta0 = 0,
                          beta1 = 0, 
                          beta2 = 0.1,
                          
                          muErr = 0, 
                          sdErr = 0,
                          errorType = "mult")

lin.plot <- sim.data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 1) +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  ggtitle("Linear")

log.plot <- sim.data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 1) +
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  ggtitle("Log")

grid.arrange(lin.plot, log.plot, ncol=2)




fit.models <- 
  function(sim.data){
    
    #fit glm exponential model (BEWARE HERE)
    glm.mod <- glm(y ~ x, data = sim.data, family = gaussian(link="log"))
    
    # fit quadratic model
    quad.mod  <- lm(y ~ x + I(x^2), data = sim.data)
    
    # fit linear model
    lin.mod   <- lm(y ~ x, data = sim.data)
    
    # Calculate lack of fit
    if(nrow(sim.data)/length(unique(sim.data$x)) > 1){
      lof.mod <- lm(y ~ x + as.factor(x), data = sim.data)
      lof <- anova(lof.mod) %>% 
        as.data.frame() %>%
        filter(row.names(.) == "as.factor(x)")
    } else {
      lof.mod <- NULL
      lof <- NULL
    }
    
    # return(list(sim.data = sim.data, glm.mod = glm.mod, exp.mod = exp.mod, quad.mod = quad.mod, lin.mod = lin.mod, lof.mod = lof.mod, lof = lof))
    return(list(sim.data = sim.data, glm.mod = glm.mod, quad.mod = quad.mod, lin.mod = lin.mod, lof.mod = lof.mod, lof = lof))
  }

sim.fit <- fit.models(sim.data)
sim.fit
