require(tidyverse)
require(gridExtra)
require(scales)

# Exponential ---------------------------------------------------------------------------------------
simulate.exponential <- 
  function(N, xRange = c(1,N), nReps, alpha, beta, theta, muErr, sdErr, errorType, ...){
  
  exp.data <- data.frame(x = rep(seq(xRange[1], xRange[2], length.out = N), nReps), y = NA)
  
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

# Evaluate Fit -------------------------------------------------------------------------------

fit.models <- 
  function(sim.data){
      
      #fit glm exponential model (BEWARE HERE)
      glm.mod <- glm(y ~ x, data = sim.data, family = gaussian(link="log"))
    
      # fit nonlinear exponential model
      theta.0 <- min(sim.data$y) * 0.5  
      model.0 <- lm(log(y) ~ x, data=sim.data)  
      alpha.0 <- exp(coef(model.0)[1])
      beta.0 <- coef(model.0)[2]
      start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
      exp.mod   <- model <- nls(y ~ alpha * exp(beta * x) + theta , data = sim.data, start = start)
      
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
              
      return(list(sim.data = sim.data, glm.mod = glm.mod, exp.mod = exp.mod, quad.mod = quad.mod, lin.mod = lin.mod, lof.mod = lof.mod, lof = lof))
  }

# Trial -------------------------------------------------------------------

sim.data <- simulate.data(simulateFunction = simulate.exponential, 
                          N = 30, 
                          nReps = 5,
                          xRange = c(1,30), 
                          
                          # Exponential Parameters
                          alpha = 1,
                          beta = 0.1, 
                          theta = 0,
                          
                          # Quadratic Parameters
                          beta0 = 0,
                          beta1 = 0, 
                          beta2 = 0.1,
                          
                          muErr = 0, 
                          sdErr = 0.25,
                          errorType = "mult")

sim.fit <- fit.models(sim.data)

lin.fit <- c("Exponential (glm)", "Exponential (nonlinear)", "Linear", "Quadratic")

lin.plot <- sim.fit$sim.data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 1) +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  ggtitle("Linear")
if("Exponential (glm)" %in% lin.fit){
  lin.plot <- lin.plot + geom_line(aes(y = predict(sim.fit$glm.mod, type = "response"), color = "Exponential \n (glm)"))
} 
if("Exponential (nonlinear)" %in% lin.fit){
  lin.plot <- lin.plot + geom_line(aes(y = predict(sim.fit$exp.mod), color = "Exponential \n (nonlinear)"))
} 
if("Linear" %in% lin.fit){
  lin.plot <- lin.plot + geom_line(aes(y = predict(sim.fit$lin.mod), color = "Linear"))
} 
if("Quadratic" %in% lin.fit){
    lin.plot <- lin.plot + geom_line(aes(y = predict(sim.fit$quad.mod), color = "Quadratic"))
}

log.fit <- c("Exponential (glm)", "Exponential (nonlinear)", "Linear", "Quadratic")

log.plot <- sim.fit$sim.data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 1) +
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  scale_color_brewer(palette = "Paired") +
  theme_bw() +
  ggtitle("Log")
if("Exponential (glm)" %in% log.fit){
  log.plot <- log.plot + geom_line(aes(y = predict(sim.fit$glm.mod, type = "response"), color = "Exponential \n (glm)"))
} 
if("Exponential (nonlinear)" %in% log.fit){
  log.plot <- log.plot + geom_line(aes(y = predict(sim.fit$exp.mod), color = "Exponential \n (nonlinear)"))
} 
if("Linear" %in% log.fit){
  log.plot <- log.plot + geom_line(aes(y = predict(sim.fit$lin.mod), color = "Linear"))
} 
if("Quadratic" %in% log.fit){
  log.plot <- log.plot + geom_line(aes(y = predict(sim.fit$quad.mod), color = "Quadratic"))
}

grid.arrange(lin.plot, log.plot, ncol=2)

# Evaluate Fit ---------------------------------------------------------------------

calcLOF <- 
  function(sim.data){
    
    # Calculate lack of fit
    if(nrow(sim.data)/length(unique(sim.data$x)) > 1){
      lof.mod <- lm(y ~ as.factor(x), data = sim.data)
      lof <- anova(lof.mod) %>% 
        as.data.frame() %>%
        filter(row.names(.) == "as.factor(x)")
    } else {
      lof.mod <- NULL
      lof <- NULL
    }
    
    return(list(lof = lof))
  }

# calcLOF(sim.data)

evalFit <- function(samps, lofStat, simulateFunc, ...){
  
  eval.stats <- rep(NA, samps)
  
  for(i in 1:samps){
    samp.data <- simulateFunc(...)
    eval.stats[i] <- calcLOF(samp.data)$lof[lofStat] %>% as.numeric()
  }
  
  return(cbind(lower = quantile(eval.stats, 0.05), mean = mean(eval.stats), upper = quantile(eval.stats, 0.95)))
}

evalFit(samps = 1000, 
        lofStat = "F value", 
        simulateFunc = simulate.exponential, 
        N = 30, 
        nReps = 5,
        xRange = c(1,30), 
        
        # Exponential Parameters
        alpha = 1,
        beta = 0.1, 
        theta = 0,
        
        # Quadratic Parameters
        beta0 = 0,
        beta1 = 0, 
        beta2 = 0.1,
        
        muErr = 0, 
        sdErr = 0.25,
        errorType = "mult")


# Run 1000 replications -------------------------------------------------------------------

repEvalFit <- 
  function(parmAdjust, parmRange, parmBy, ...){
    
    parm.seq <- seq(parmRange[1], parmRange[2], parmBy)
    eval.data <- cbind(parm.seq, matrix(NA, nrow = length(parm.seq), ncol = 3))
    colnames(eval.data) <- c(parmAdjust, "lower", "mean", "upper")
    
    for(j in 1:length(parm.seq)){
      
      if(parmAdjust %in% c("beta")){
        eval.stats <- evalFit(beta = parm.seq[j], ...)
      }
      
      if(parmAdjust %in% c("N")){
        eval.stats <- evalFit(N = parm.seq[j], ...)
      }
      
      if(parmAdjust %in% c("sdErr")){
        eval.stats <- evalFit(sdErr = parm.seq[j], ...)
      }
      
      eval.data[j,2] <- eval.stats[1]
      eval.data[j,3] <- eval.stats[2]
      eval.data[j,4] <- eval.stats[3]
    }
    
    return(eval.data)
  }


eval.data <- repEvalFit(parmAdjust = "beta", 
           parmRange = c(0.05, 0.5),
           parmBy = 0.01,
           samps = 20, 
           lofStat = "F value", 
           simulateFunc = simulate.exponential, 
           N = 30, 
           nReps = 5,
           xRange = c(1,20), 
           
           # Exponential Parameters
           alpha = 1,
           # beta = 0.1
           theta = 0,
           
           # Quadratic Parameters
           beta0 = 0,
           beta1 = 0, 
           beta2 = 0.1,
           
           muErr = 0, 
           sdErr = 0.25,
           errorType = "mult")

eval.data %>%
  data.frame() %>%
  ggplot(aes(x = beta, y = mean)) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_point() +
  theme_bw() 


