# library(tidyverse)
# library(purrr)

# ------------------------------------------------------------------------------
# Exponential Data Generation --------------------------------------------------
# ------------------------------------------------------------------------------

expDataGen <- 
  function(beta, 
           sd, 
           points_choice = "partial", 
           points_end_scale = 0.5,
           N = 30,
           x_min = 0,
           x_max = 20,
           x_by  = 0.25){
    
    points_end_scale <- ifelse(points_choice == "full", 1, points_end_scale)
    
    # Set up x values
    xVals <- seq(0, x_max*points_end_scale, length.out = floor(N*1))
    xVals <- sample(xVals, N, replace = FALSE)
    xVals <- jitter(xVals)
    xVals <- ifelse(xVals < x_min, x_min, xVals)
    xVals <- ifelse(xVals > x_max, x_max, xVals)
    
    # Generate "good" errors
    repeat{
      errorVals <- rnorm(length(xVals), 0, sd)
      if(mean(errorVals[floor(N/3)]) < 2*sd & mean(errorVals[floor(N/3)] > -2*sd)){
        break
      }
    }
    
    # Simulate point data
    point_data <- tibble(x = xVals,
                         y = exp(x*beta + errorVals),
                         dataset = "point_data") %>%
      arrange(x)
    
    # Obtain starting value for beta
    lm.fit <- lm(log(y) ~ x, data = point_data)
    beta.0 <- coef(lm.fit)[1] %>% as.numeric()
    # Use NLS to fit a better line to the data
    start <- list(beta = beta.0)
    nonlinear.fit <- nls(y ~ exp(x*beta),
                         data = point_data, 
                         start = start)
    betahat <- coef(nonlinear.fit)[1] %>% as.numeric()
    
    # Simulate best fit line data
    line_data <- tibble(x = seq(x_min, x_max, x_by), 
                        y = exp(x*betahat),
                        dataset = "line_data")
    
    data <- list(point_data = point_data, line_data = line_data)
    
    return(data)
  }

# ------------------------------------------------------------------------------
# Linear Data Generation --------------------------------------------------
# ------------------------------------------------------------------------------

linearDataGen <- 
  function(y_xbar, 
           slope, 
           sigma, 
           points_choice = "full", 
           points_end_scale = 1,
           N = 30,
           x_min = 0,
           x_max = 20,
           x_by  = 0.25){
    
    points_end_scale <- ifelse(points_choice == "full", 1, points_end_scale)
    
    # Set up x values
    xVals <- seq(0, x_max*points_end_scale, length.out = floor(N*1))
    xVals <- sample(xVals, N, replace = FALSE)
    xVals <- jitter(xVals)
    xVals <- ifelse(xVals < x_min, x_min, xVals)
    xVals <- ifelse(xVals > x_max, x_max, xVals)
    
    # From slope intercept form
    # y-y_xbar = m(x-xbar)
    # y = m(x-xbar) + y_xbar = mx - mxbar + y_xbar
    yintercept = y_xbar - slope*mean(xVals)
    
    # Generate "good" errors
    repeat{
      errorVals <- rnorm(N, 0, sigma)
      if(mean(errorVals[floor(N/3)]) < 2*sigma & mean(errorVals[floor(N/3)] > -2*sigma)){
        break
      }
    }
    
    # Simulate point data
    point_data <- tibble(dataset = "point_data",
                         x = xVals,
                         y = yintercept + slope*x + errorVals) %>%
      arrange(x)
    
    # Obtain least squares regression coefficients
    lm.fit <- lm(y ~ x, data = point_data)
    yintercepthat <- coef(lm.fit)[1] %>% as.numeric()
    slopehat <- coef(lm.fit)[2] %>% as.numeric()
    
    # Simulate best fit line data
    line_data <- tibble(dataset = "line_data", 
                        x = seq(x_min, x_max, x_by), 
                        y = yintercepthat + slopehat*x)
    
    data <- list(point_data = point_data, line_data = line_data)
    
    return(data)
  }


# ------------------------------------------------------------------------------
# Simulate Data ----------------------------------------------------------------
# ------------------------------------------------------------------------------

library(RSQLite)
library(DBI)
sqlite.driver <- dbDriver("SQLite")

con <- dbConnect(sqlite.driver, dbname = "you_draw_it_data.db")
  exp_parameter_details        <- dbReadTable(con, "exp_parameter_details")
  eyefitting_parameter_details <- dbReadTable(con, "eyefitting_parameter_details")
dbDisconnect(con)
  
exp_data <- exp_parameter_details %>%
  mutate(data = purrr::pmap(list(beta  = beta, 
                                 sd    = sd, 
                                 points_choice    = points_choice, 
                                 points_end_scale = points_end_scale,
                                 N     = N, 
                                 x_min = x_min, 
                                 x_max = x_max, 
                                 x_by  = x_by), expDataGen)) %>%
  unnest(data) %>%
  unnest(data) %>%
  dplyr::select(dataset, parm_id, x, y)



eyefitting_data <- eyefitting_parameter_details %>%
  mutate(data = purrr::pmap(list(y_xbar = y_xbar, 
                                 slope = slope, 
                                 sigma = sigma, 
                                 x_min = x_min, 
                                 x_max = x_max), linearDataGen)) %>%
  unnest(data) %>%
  unnest(data) %>%
  dplyr::select(dataset, parm_id, x, y)

simulated_data <- rbind(exp_data, eyefitting_data)
