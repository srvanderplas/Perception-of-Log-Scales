# library(tidyverse)
# library(purrr)

# ------------------------------------------------------------------------------
# Exponential Data Generation --------------------------------------------------
# ------------------------------------------------------------------------------

expDataGen <-
  function(beta,
           sd,
           N = 30,
           x_min = 0,
           x_max = 20,
           x_by  = 0.25){
    
    # Set up x values
    xVals <- seq(0, x_max, length.out = floor(N*1))
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
           N = 30,
           x_min = 0,
           x_max = 20,
           x_by  = 0.25){
    
    # Set up x values
    xVals <- seq(0, x_max, length.out = floor(N*1))
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

# filename <- "you-draw-it-development/you-draw-it-pilot-app/you_draw_it_data.db"
filename <- "you_draw_it_data.db"
con <- dbConnect(sqlite.driver, dbname = filename)
# dbListTables(con)
  exp_parameter_details        <- dbReadTable(con, "exp_parameter_details")
  eyefitting_parameter_details <- dbReadTable(con, "eyefitting_parameter_details")
dbDisconnect(con)

exp_data <- exp_parameter_details %>%
  mutate(data = purrr::pmap(list(beta  = beta,
                                 sd    = sd,
                                 N     = N,
                                 x_min = x_min,
                                 x_max = x_max,
                                 x_by  = x_by), expDataGen)) %>%
  expand_grid(points_end = c(10, 15),
              linear = c("true", "false"),
              draw_start = 10,
              free_draw = FALSE) %>%
  unnest(data) %>%
  unnest(data) %>%
  mutate(y = ifelse(dataset == "point_data" & x > points_end, NA, y)) %>%
  na.omit() %>%
  mutate(parm_id = paste("beta", beta, "-", points_end, "-", linear, sep = "")) %>%
  nest(data = c("dataset", "x", "y")) %>%
  dplyr::select(parm_id, data, linear, free_draw, draw_start)

eyefitting_data <- eyefitting_parameter_details %>%
  mutate(data = purrr::pmap(list(y_xbar = y_xbar,
                                 slope = slope,
                                 sigma = sigma,
                                 x_min = x_min,
                                 x_max = x_max,
                                 x_by = x_by), linearDataGen)) %>%
  expand_grid(linear = "true",
              draw_start = NA,
              free_draw = TRUE) %>%
  unnest(data) %>%
  unnest(data) %>%
  nest(data = c("dataset", "x", "y")) %>%
  dplyr::select(parm_id, data, linear, free_draw, draw_start)

simulated_data <- rbind(exp_data, eyefitting_data) %>%
  mutate(order = sample(1:12, 12)) %>%
  arrange(order)
