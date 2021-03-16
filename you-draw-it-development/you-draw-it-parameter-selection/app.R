# ----------------------------------------------------------------------------------------------------
# Load libraries -------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
library(shiny)
library(shinyjs)
# ALERT: REQUIRES VERSION 0.2.3
# url_r2d3v0.2.3 <- "https://cran.r-project.org/src/contrib/Archive/r2d3/r2d3_0.2.3.tar.gz"
# install.packages(url_r2d3v0.2.3, repos = NULL, type = 'source')
library(r2d3)
library(tidyverse)
library(purrr)
library(gridSVG)
library(lubridate)
library(readxl)

# ----------------------------------------------------------------------------------------------------
# Turn a list of data into a json file ---------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

data_to_json <- function(data) {
  jsonlite::toJSON(data, 
                   dataframe = "rows", 
                   auto_unbox = FALSE, 
                   rownames = TRUE)
} 

# ----------------------------------------------------------------------------------------------------
# Redefine drawr function --------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

drawr <- function(data, 
                  linear            = "true", 
                  draw_start        = NULL,
                  points_end        = NULL,
                  x_by              = 0.25,
                  free_draw         = T,
                  points            = "partial",
                  aspect_ratio      = 1.5,
                  title             = "", 
                  x_range           = NULL, 
                  y_range           = NULL,
                  x_lab             = "", 
                  y_lab             = "", 
                  drawn_line_color  = "steelblue",
                  data_tab1_color   = "steelblue", 
                  x_axis_buffer     = 0.01, 
                  y_axis_buffer     = 0.05,
                  show_finished     = T,
                  shiny_message_loc = NULL) {
  
  line_data  <- data$line_data
  point_data <- data$point_data
  
  x_min <- min(line_data$x)
  x_max <- max(line_data$x)
  y_min <- min(line_data$y)
  y_max <- max(line_data$y)
  
  if (is.null(x_range)) {
    x_buffer <- (x_max - x_min) * x_axis_buffer
    x_range <- c(x_min - x_buffer, x_max + x_buffer)
  }
  if (is.null(y_range)) {
    y_buffer <- (y_max - y_min) * y_axis_buffer
    y_range <- c(y_min - y_buffer, y_max + y_buffer)
    if (linear != "true") {
      if (y_range[1] <= 0) {
        y_range[1] <- min(y_min, y_axis_buffer)
      }
    }
  } else {
    if (y_range[1] > y_min | y_range[2] < y_max) {
      stop("Supplied y range doesn't cover data fully.")
    }
  }

  if ((draw_start <= x_min) | (draw_start >= x_max)) {
    stop("Draw start is out of data range.")
  }

  r2d3::r2d3(data   = data_to_json(data), 
             script = "main.js",
             dependencies = c("d3-jetpack"),
             options = list(draw_start        = draw_start, 
                            points_end        = points_end,
                            linear            = as.character(linear),
                            free_draw         = free_draw, 
                            points            = points,
                            aspect_ratio      = aspect_ratio,
                            pin_start         = T, 
                            x_range           = x_range,
                            x_by              = x_by,
                            y_range           = y_range,
                            line_style        = NULL,
                            data_tab1_color   = data_tab1_color, 
                            drawn_line_color  = drawn_line_color,
                            show_finished     = show_finished,
                            shiny_message_loc = shiny_message_loc,
                            title             = title)
             )
  
}

# ----------------------------------------------------------------------------------------------------
# Exponential Data Simulation ------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

expDataGen <- 
  function(beta, 
           sd, 
           points_choice = "partial", 
           points_end_scale = 0.5,
           N = 30,
           aspect_ratio = 1,
           free_draw = "false",
           x_min = 0,
           x_max = 20,
           x_by  = 0.25){
    
    points_end_scale <- ifelse(points_choice == "full", 1, points_end_scale)
    
    # Set up x values
    xVals <- seq(x_min, x_max*points_end_scale, length.out = floor(N*3/4))
    xVals <- sample(xVals, N, replace = TRUE)
    xVals <- jitter(xVals)
    xVals <- ifelse(xVals < 0, 0, xVals)
    xVals <- ifelse(xVals > 20, 20, xVals)
    
    # Generate "good" errors
    repeat{
      errorVals <- rnorm(length(xVals), 0, sd)
      if(mean(errorVals[floor(N/3)]) < 2*sd & mean(errorVals[floor(N/3)] > -2*sd)){
        break
      }
    }
    
    # Simulate point data
    point_data <- tibble(x = xVals,
                         y = exp(x*beta + errorVals)) %>%
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
                        y = exp(x*betahat))
    
    data <- list(point_data = point_data, line_data = line_data)
    
    return(data)
  }

# ----------------------------------------------------------------------------------------------------
# Linear Data Simulation -----------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

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
    xVals <- seq(x_min, x_max*points_end_scale, length.out = floor(N*3/4))
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

# ----------------------------------------------------------------------------------------------------
# User Interface -------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

ui <- navbarPage(
  "You Draw It Development",

# ----------------------------------------------------------------------------------------------------
  tabPanel(
    title = "Exponential Prediction (Log / Linear)",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "d3.css")
    ),
    fluidRow(
      column(
        width = 5,
        d3Output("shinydrawr_exp1.linear", height = "450px"),
        d3Output("shinydrawr_exp2.linear", height = "450px"),
        d3Output("shinydrawr_exp3.linear", height = "450px"),
        d3Output("shinydrawr_exp4.linear", height = "450px")
      ),
      column(
        width = 5,
        d3Output("shinydrawr_exp1.log", height = "450px"),
        d3Output("shinydrawr_exp2.log", height = "450px"),
        d3Output("shinydrawr_exp3.log", height = "450px"),
        d3Output("shinydrawr_exp4.log", height = "450px")
      ),
      column(
        width = 2,
        actionButton("reset", "Reset"),
        checkboxInput("show_finished", "Show Finished?", value = T),
        numericInput("N", "N Points:", min = 10, max = 50, value = 30, step = 5),
        sliderInput("ymin_scale", label = "y-range lower buffer:", min = 0.25, max = 1, step = 0.25, value = 0.5),
        sliderInput("ymax_scale", label = "y-range upper buffer:", min = 1, max = 4, step = 0.25, value = 2),
        
      )
    )
  ),
# ----------------------------------------------------------------------------------------------------
  tabPanel(
    title = "Eye Fitting Straight Lines in the Modern Era",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "d3.css")
    ),
    fluidRow(
      column(
        width = 5,
        d3Output("shinydrawr_S", height = "450px"),
        d3Output("shinydrawr_V", height = "450px")
      ),
      column(
        width = 5,
        d3Output("shinydrawr_F", height = "450px"),
        d3Output("shinydrawr_N", height = "450px")
      ),
      column(
        width = 2,
        actionButton("eyefitting_reset", "Reset"),
        checkboxInput("eyefitting_show_finished", "Show Finished?", value = T),
        numericInput("eyefitting_N", "N Points:", min = 20, max = 50, value = 30, step = 5),
        numericInput("eyefitting_by", "By:", min = 0.25, max = 20, value = 0.5, step = 0.25),
      )
    )
  )
# ----------------------------------------------------------------------------------------------------
)

# ----------------------------------------------------------------------------------------------------
# Server ---------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session){
  
# ----------------------------------------------------------------------------------------------------

  # Exponential Simulation Parameter Values --------------------------------------
  exp_parameters <- reactive({
    
    input$reset
 
    data.frame(beta = c(0.1, 0.23), sd = c(0.09, 0.25)) %>%
    expand_grid(points_end_scale = c(0.5, 0.75), 
                points_choice = "partial", 
                N = input$N, 
                aspect_ratio = 1, 
                free_draw = FALSE, 
                x_min = 0,
                x_max = 20,
                x_by = 0.25,
                ymin_scale = input$ymin_scale,
                ymax_scale = input$ymax_scale,
                draw_start_scale = 0.5,
                linear = c("true", "false"))
    
  })
  
  # ---------------------------------------------------------------------------------
  # Data set 1 ----------------------------------------------------------------------
  # ---------------------------------------------------------------------------------
  
  exp_data1 <- reactive({
    
    parms <- exp_parameters()[1,]
    
    expDataGen(beta  = parms$beta, 
               sd    = parms$sd, 
               points_choice    = parms$points_choice, 
               points_end_scale = parms$points_end_scale,
               N     = parms$N, 
               x_min = parms$x_min, 
               x_max = parms$x_max, 
               x_by  = parms$x_by)
    })
  
  # Linear Scale (1) ------------------------------------------------------------------
  
  message_loc_exp1.linear <- session$ns("drawr_message")
  output$shinydrawr_exp1.linear <- r2d3::renderD3({
    
    parms   <- exp_parameters()[1,]
    data    <- exp_data1()
    y_range <- range(data$line_data[,"y"]) * c(parms$ymin_scale, parms$ymax_scale)
    x_range <- range(data$line_data[,"x"])
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_exp1.linear,
          x_range           = x_range,
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # Log Scale (1) ---------------------------------------------------------------------
  
  message_loc_exp1.log <- session$ns("drawr_message")
  output$shinydrawr_exp1.log <- r2d3::renderD3({

    parms   <- exp_parameters()[2,]
    data    <- exp_data1()
    y_range <- range(data$line_data[,"y"]) * c(parms$ymin_scale, parms$ymax_scale)
    x_range <- range(data$line_data[,"x"])
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_exp1.log,
          x_range           = x_range,
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )

  })

  # ---------------------------------------------------------------------------------
  # Data set 2 ----------------------------------------------------------------------
  # ---------------------------------------------------------------------------------

  exp_data2 <- reactive({
    
    parms <- exp_parameters()[3,]
    
    expDataGen(beta  = parms$beta, 
               sd    = parms$sd, 
               points_choice    = parms$points_choice, 
               points_end_scale = parms$points_end_scale,
               N     = parms$N, 
               x_min = parms$x_min, 
               x_max = parms$x_max, 
               x_by  = parms$x_by)
  })

  # Linear Scale (2) ------------------------------------------------------------------
  
  message_loc_exp2.linear <- session$ns("drawr_message")
  output$shinydrawr_exp2.linear <- r2d3::renderD3({
    
    parms   <- exp_parameters()[3,]
    data    <- exp_data2()
    y_range <- range(data$line_data[,"y"]) * c(parms$ymin_scale, parms$ymax_scale)
    x_range <- range(data$line_data[,"x"])
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_exp1.linear,
          x_range           = x_range,
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # Log Scale (2) ---------------------------------------------------------------------
  
  message_loc_exp2.log <- session$ns("drawr_message")
  output$shinydrawr_exp2.log <- r2d3::renderD3({
    
    parms   <- exp_parameters()[4,]
    data    <- exp_data2()
    y_range <- range(data$line_data[,"y"]) * c(parms$ymin_scale, parms$ymax_scale)
    x_range <- range(data$line_data[,"x"])
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_exp1.log,
          x_range           = x_range,
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })

  # ---------------------------------------------------------------------------------
  # Data set 3 ----------------------------------------------------------------------
  # ---------------------------------------------------------------------------------
  
  exp_data3 <- reactive({
    
    parms <- exp_parameters()[5,]
    
    expDataGen(beta  = parms$beta, 
               sd    = parms$sd, 
               points_choice    = parms$points_choice, 
               points_end_scale = parms$points_end_scale,
               N     = parms$N, 
               x_min = parms$x_min, 
               x_max = parms$x_max, 
               x_by  = parms$x_by)
  })
  
  # Linear Scale (3) ------------------------------------------------------------------
  
  message_loc_exp3.linear <- session$ns("drawr_message")
  output$shinydrawr_exp3.linear <- r2d3::renderD3({
    
    parms   <- exp_parameters()[5,]
    data    <- exp_data3()
    y_range <- range(data$line_data[,"y"]) * c(parms$ymin_scale, parms$ymax_scale)
    x_range <- range(data$line_data[,"x"])
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_exp1.linear,
          x_range           = x_range,
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # Log Scale (3) ---------------------------------------------------------------------
  
  message_loc_exp3.log <- session$ns("drawr_message")
  output$shinydrawr_exp3.log <- r2d3::renderD3({
    
    parms   <- exp_parameters()[6,]
    data    <- exp_data3()
    y_range <- range(data$line_data[,"y"]) * c(parms$ymin_scale, parms$ymax_scale)
    x_range <- range(data$line_data[,"x"])
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_exp1.log,
          x_range           = x_range,
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })

  # ---------------------------------------------------------------------------------
  # Data set 4 ----------------------------------------------------------------------
  # ---------------------------------------------------------------------------------
  
  exp_data4 <- reactive({
    
    parms <- exp_parameters()[7,]
    
    expDataGen(beta  = parms$beta, 
               sd    = parms$sd, 
               points_choice    = parms$points_choice, 
               points_end_scale = parms$points_end_scale,
               N     = parms$N, 
               x_min = parms$x_min, 
               x_max = parms$x_max, 
               x_by  = parms$x_by)
  })
  
  # Linear Scale (4) ------------------------------------------------------------------
  
  message_loc_exp4.linear <- session$ns("drawr_message")
  output$shinydrawr_exp4.linear <- r2d3::renderD3({
    
    parms   <- exp_parameters()[7,]
    data    <- exp_data4()
    y_range <- range(data$line_data[,"y"]) * c(parms$ymin_scale, parms$ymax_scale)
    x_range <- range(data$line_data[,"x"])
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_exp1.linear,
          x_range           = x_range,
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # Log Scale (4) ---------------------------------------------------------------------
  
  message_loc_exp4.log <- session$ns("drawr_message")
  output$shinydrawr_exp4.log <- r2d3::renderD3({
    
    parms   <- exp_parameters()[8,]
    data    <- exp_data4()
    y_range <- range(data$line_data[,"y"]) * c(parms$ymin_scale, parms$ymax_scale)
    x_range <- range(data$line_data[,"x"])
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_exp1.log,
          x_range           = x_range,
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })

# ----------------------------------------------------------------------------------------------------
# Eye Fitting Straight Lines Replication Simulation Function -----------------------------------------
# ----------------------------------------------------------------------------------------------------


  linear_data <- reactive({

    input$eyefitting_reset
      
    tibble(
      dataset = c("S", "F", "V", "N"),
      y_xbar = c(3.88, 3.9, 3.89, 4.11),
      slope  = c(0.66, 0.66, 1.98, -0.70),
      sigma  = c(1.3, 2.8, 1.5, 2.5),
      x_min   = c(0, 0, 4, 0),
      x_max   = c(20, 20, 16, 20),
      N       = input$eyefitting_N,
      x_by    = input$eyefitting_by) %>%
      mutate(data = purrr::pmap(list(y_xbar = y_xbar, slope = slope, sigma = sigma, x_min = x_min, x_max = x_max, x_by = x_by, N = N), linearDataGen)) %>%
      unnest(data) %>%
      unnest(data)
  })

  linear_y_range <- reactive({
    linear_data <- linear_data()
    range(linear_data$y) * c(1.2, 1.2)
  })

  linear_x_range <- reactive({
    linear_data <- linear_data()
    c(min(linear_data$x) - 1, max(linear_data$x) + 1)
  })

  # S ----------------------------------------------------------

  message_loc_S <- session$ns("drawr_message")
  output$shinydrawr_S <- r2d3::renderD3({

    line_data_S <- linear_data() %>%
      filter(dataset == "S") %>%
      filter(data == "line_data")
    
    point_data_S <- linear_data() %>%
      filter(dataset == "S") %>%
      filter(data == "point_data")
    
    data <- list(line_data = line_data_S, point_data = point_data_S)

    drawr(data              = data,
          aspect_ratio      = 1,
          linear            = "true",
          free_draw         = TRUE,
          points            = "full",
          x_by              = 0.25,
          draw_start        = 1,
          points_end        = 20,
          show_finished     = input$eyefitting_show_finished,
          shiny_message_loc = message_loc_S,
          x_range           = linear_x_range(),
          y_range           = linear_y_range(),
          title             = "S"
    )

  })

  # F ----------------------------------------------------------
  
  message_loc_F <- session$ns("drawr_message")
  output$shinydrawr_F <- r2d3::renderD3({
    
    line_data_F <- linear_data() %>%
      filter(dataset == "F") %>%
      filter(data == "line_data")
    
    point_data_F <- linear_data() %>%
      filter(dataset == "F") %>%
      filter(data == "point_data")
    
    data <- list(line_data = line_data_F, point_data = point_data_F)
    
    drawr(data              = data,
          aspect_ratio      = 1,
          linear            = "true",
          free_draw         = TRUE,
          points            = "full",
          x_by              = 0.25,
          draw_start        = 1,
          points_end        = 20,
          show_finished     = input$eyefitting_show_finished,
          shiny_message_loc = message_loc_S,
          x_range           = linear_x_range(),
          y_range           = linear_y_range(),
          title             = "F"
    )
    
  })

  # V ----------------------------------------------------------
  
  message_loc_V <- session$ns("drawr_message")
  output$shinydrawr_V <- r2d3::renderD3({
    
    line_data_V <- linear_data() %>%
      filter(dataset == "V") %>%
      filter(data == "line_data")
    
    point_data_V <- linear_data() %>%
      filter(dataset == "V") %>%
      filter(data == "point_data")
    
    data <- list(line_data = line_data_V, point_data = point_data_V)
    
    drawr(data              = data,
          aspect_ratio      = 1,
          linear            = "true",
          free_draw         = TRUE,
          points            = "full",
          x_by              = 0.25,
          draw_start        = 5.1,
          points_end        = 20,
          show_finished     = input$eyefitting_show_finished,
          shiny_message_loc = message_loc_S,
          x_range           = linear_x_range(),
          y_range           = linear_y_range(),
          title             = "V"
    )
    
  })
   
  # N ----------------------------------------------------------
  
  message_loc_N <- session$ns("drawr_message")
  output$shinydrawr_N <- r2d3::renderD3({
    
    line_data_N <- linear_data() %>%
      filter(dataset == "N") %>%
      filter(data == "line_data")
    
    point_data_N <- linear_data() %>%
      filter(dataset == "N") %>%
      filter(data == "point_data")
    
    data <- list(line_data = line_data_N, point_data = point_data_N)
    
    drawr(data              = data,
          aspect_ratio      = 1,
          linear            = "true",
          free_draw         = TRUE,
          points            = "full",
          x_by              = 0.25,
          draw_start        = 1,
          points_end        = 20,
          show_finished     = input$eyefitting_show_finished,
          shiny_message_loc = message_loc_S,
          x_range           = linear_x_range(),
          y_range           = linear_y_range(),
          title             = "N"
    )
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
