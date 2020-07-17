library(tidyverse)

# ---------------------------------------------------------------------------
# SIMULATE EXPONENTIAL ------------------------------------------------------
# ---------------------------------------------------------------------------

# Based on GLM method

sim.exponential.glm <- function(N, beta = c(1, 1)){
  exp.data   <- data.frame(x = jitter(seq(1,N)), y = NA, sim.method = "GLM")
  exp.data$y <- rexp(N, rate = 1/exp(beta[1]+beta[2]*exp.data$x))
  return(exp.data)
}

# Based on algebraic method

sim.exponential.alg <- function(N, beta = c(1, 1), variability){
  exp.data   <- data.frame(x = jitter(seq(1,N)), y = NA, sim.method = "Algebraic")
  exp.data$y <- exp(beta[1]+beta[2]*exp.data$x) + rnorm(N, 0, 1)*variability
  return(exp.data)
}

# From Perception of Linear and Nonlinear Trends

# n = 9, 17, 33
# variability = 0, 0.22, 0.44, 0.66
sim.exponential.trial <- function(N, variability){
  
  exp.data <- data.frame(x = jitter(seq(1,N)), y = NA, sim.method = "Paper")
  exp.data$y <- 1.75 - 0.75*(log(37.5 - (exp.data$x + 4))/log(10)) + rnorm(N, 0, 1)*variability
  
  mod <- glm(y ~ x, data = exp.data[(exp.data$y > 0),], family = Gamma(link="log"))
  
  return(list(exp.data = exp.data, mod.summary = summary(mod, dispersion = 1)))
}


# ---------------------------------------------------------------------------
# COMPARE METHODS -----------------------------------------------------------
# ---------------------------------------------------------------------------
n = 33
variabilityVal = 0.22

exp.data.trial <- sim.exponential.trial(N = n, variability = variabilityVal)
exp.data.trial$mod.summary #be careful, set >0 restriction
exp.data.glm <- sim.exponential.glm(N = n, beta = c(-0.8, 0.037))
exp.data.alg <- sim.exponential.alg(N = n, beta = c(-0.8, 0.037), variability = variabilityVal)

data <- rbind(exp.data.glm, exp.data.trial$exp.data, exp.data.alg)
data

data %>%
  ggplot(aes(x = x, y = y, color = sim.method)) +
  geom_point() +
  scale_color_brewer(palette = "Paired") +
  theme_bw()

require(scales)

data %>%
  ggplot(aes(x = x, y = y, color = sim.method)) +
  geom_point() +
  scale_color_brewer(palette = "Paired") +
  scale_y_continuous(trans = "log10",
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme_bw()

# ---------------------------------------------------------------------------
# EVALUATE FIT --------------------------------------------------------------
# ---------------------------------------------------------------------------

evaluateFit <- function(dataName, sigma.sq){
  
  N = nrow(dataName)
  
  # Fit linear model to log data?
  log.fit <- lm(log(y)~x, data = dataName)
  
  #LOF... not sure this is right.
  sigma.hat.sq <- summary(log.fit)$sigma^2
  lof <- (N - 1)*sigma.hat.sq/sigma.sq
  
  # Curvature (Based on Perception of Linear and Nonlinear Trends)
  dataName$group <- cut_interval(dataName$y, 3) %>% as.numeric() %>% as.factor()
  curvature <- with(dataName, mean(y[group %in% c(2)]) - mean(y[group %in% c(1,3)]))
  
  return(list(Rsq = summary(log.fit)$r.squared, 
              SS = sum(summary(log.fit)$residuals), 
              lof = lof,
              curvature = curvature))
}

evaluateFit(exp.data.glm, sigma.sq = variabilityVal^2) #LOF isn't right here..
evaluateFit(exp.data.trial$exp.data, sigma.sq = variabilityVal^2)
evaluateFit(exp.data.alg, sigma.sq = variabilityVal^2)

testEvalFit <- function(reps, sigma.sq, func, ...){
  
  curvature <- rep(NA, reps)
  lof <- rep(NA, reps)
  
  for(i in 1:reps){
    trialData <- func(...)
    curvature[i] <- evaluateFit(trialData, sigma.sq)$curvature
    lof[i] <- evaluateFit(trialData, sigma.sq)$lof
  }
  
  return(list(lof = summary(lof), curvature = summary(curvature)))
}

# ---------------------------------------------------------------
# INCLUDE ALPHA -------------------------------------------------
# ---------------------------------------------------------------

sim.exponential.alg2 <- function(N, alpha = 1, beta = 1, variability){
  exp.data   <- data.frame(x = jitter(seq(1,N)), y = NA, sim.method = "Algebraic")
  exp.data$y <- alpha*exp(beta*exp.data$x) + rnorm(N, 0, 1)*variabilityVal^2
  return(exp.data)
}

trialData <- sim.exponential.alg2(N = 17, alpha = 1, beta = 0.1, variability = 0.2)
trialData %>% ggplot(aes(x = x, y = y)) + geom_point(pch = 1)
evaluateFit(trialData, sigma.sq = variabilityVal^2)

testEvalFit(reps = 1000, sigma.sq = variabilityVal^2, func = sim.exponential.alg2, N = 17, alpha = 1, beta = 0.1, variability = 1)
