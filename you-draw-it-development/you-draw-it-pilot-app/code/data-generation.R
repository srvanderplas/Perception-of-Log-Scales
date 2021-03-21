# library(tidyverse)
# library(purrr)

# Exponential Data Generation --------------------------------------------------

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
    xVals <- seq(0, x_max*points_end_scale, length.out = floor(N*3/4))
    xVals <- sample(xVals, N, replace = TRUE)
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

# Simulate Data ------------------------------------------------------
exp_parameters <- data.frame(beta = c(0.1, 0.23), sd = c(0.09, 0.25)) %>%
                  expand_grid(points_end_scale = c(0.5, 0.75), 
                              points_choice = "partial", 
                              N = 30, 
                              aspect_ratio = 1, 
                              free_draw = FALSE, 
                              x_min = 0,
                              x_max = 20,
                              x_by = 0.25,
                              ymin_scale = 0.5,
                              ymax_scale = 2,
                              draw_start_scale = 0.5,
                              #linear = c("true", "false")
                              ) %>%
                  rownames_to_column("parm_id")
  
  exp_data <- exp_parameters %>%
  mutate(data = purrr::pmap(list(beta  = beta, 
                                 sd    = sd, 
                                 points_choice    = points_choice, 
                                 points_end_scale = points_end_scale,
                                 N     = N, 
                                 x_min = x_min, 
                                 x_max = x_max, 
                                 x_by  = x_by), expDataGen)) %>%
  unnest(data) %>%
  unnest(data)


# point_data <- exp_data %>%
#   filter(dataset == "point_data", parm_id == 1)
# line_data <- exp_data %>%
#   filter(dataset == "line_data", parm_id == 1)
# data <- list(point_data = point_data, line_data = line_data)
# data_to_json(data)



