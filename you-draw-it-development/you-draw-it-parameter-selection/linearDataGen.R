data_to_json <- function(data) {
  jsonlite::toJSON(data, dataframe = "rows", auto_unbox = FALSE, rownames = TRUE)
} 

# Linear Data Generation --------------------------------------------------

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
    xVals <- seq(0, x_max*points_end_scale, length.out = floor(N*3/4))
    xVals <- sample(xVals, N, replace = TRUE)
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
    point_data <- tibble(data = "point_data",
                         x = xVals,
                         y = yintercept + slope*x + errorVals) %>%
                  arrange(x)
    
    # Obtain least squares regression coefficients
    lm.fit <- lm(y ~ x, data = point_data)
    yintercepthat <- coef(lm.fit)[1] %>% as.numeric()
    slopehat <- coef(lm.fit)[2] %>% as.numeric()
    
    # Simulate best fit line data
    line_data <- tibble(data = "line_data", 
                        x = seq(x_min, x_max, x_by), 
                        y = yintercepthat + slopehat*x)
    
    data <- list(point_data = point_data, line_data = line_data)
    
    return(data)
  }

# Test out simulating data
linearDataGen(y_xbar = 3.88, slope = 0.66, sigma = 1.3, x_min = 0, x_max = 20)

linear_data <- tibble(
  dataset = c("S", "F", "V", "N"),
  y_xbar = c(3.88, 3.9, 3.89, 4.11),
  slope  = c(0.66, 0.66, 1.98, -0.70),
  sigma  = c(1.3, 2.8, 1.5, 2.5),
  x_min   = c(0, 0, 0, 0),
  x_max   = c(20, 20, 20, 20)) %>%
  mutate(data = purrr::pmap(list(y_xbar = y_xbar, slope = slope, sigma = sigma, x_min = x_min, x_max = x_max), linearDataGen)) %>%
  unnest(data) %>%
  unnest(data)

line_data_S <- linear_data %>%
  filter(dataset == "S") %>%
  filter(data == "line_data")

point_data_S <- linear_data %>%
  filter(dataset == "S") %>%
  filter(data == "point_data")

data <- list(line_data = line_data_S, point_data = point_data_S)

# data_to_json(data)

plot(point_data_S$x, point_data_S$y)

write.csv(linear_data, "data\\youdrawit\\youdrawit-eyefitting-simdata-example.csv", row.names = F, na = "")
linear_data %>%
  filter(data == "point_data") %>%
  filter(dataset %in% c("F", "N", "S") | (x < 16 & x > 4)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_grid(~dataset) +
  theme_bw() +
  theme(aspect.ratio = 1)
