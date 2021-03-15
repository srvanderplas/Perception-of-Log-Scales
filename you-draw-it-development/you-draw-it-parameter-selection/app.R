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
# # Redefine drawr function --------------------------------------------------------------------------
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
  
  plot_data <- dplyr::select(data, x, y, ypoints)
  x_min <- min(plot_data$x)
  x_max <- max(plot_data$x)
  y_min <- min(plot_data$y)
  y_max <- max(plot_data$y)
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

  r2d3::r2d3(data   = plot_data, 
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
        d3Output("shinydrawr_1.linear", height = "450px"),
        d3Output("shinydrawr_2.linear", height = "450px"),
        d3Output("shinydrawr_3.linear", height = "450px"),
        d3Output("shinydrawr_4.linear", height = "450px")
      ),
      column(
        width = 5,
        d3Output("shinydrawr_1.log", height = "450px"),
        d3Output("shinydrawr_2.log", height = "450px"),
        d3Output("shinydrawr_3.log", height = "450px"),
        d3Output("shinydrawr_4.log", height = "450px")
      ),
      column(
        width = 2,
        actionButton("reset", "Reset"),
        checkboxInput("show_finished", "Show Finished?", value = T),
        numericInput("Npoints", "N Points:", min = 10, max = 50, value = 30, step = 5),
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
        # numericInput("eyefitting_Npoints", "N Points:", min = 10, max = 50, value = 30, step = 5),
        # numericInput("eyefitting_by", "By:", min = 0.25, max = 5, value = 0.25, step = 0.25),
        # sliderInput("eyefitting_ymin_scale", label = "y-range lower buffer:", min = 0.25, max = 1, step = 0.25, value = 0.5),
        # sliderInput("eyefitting_ymax_scale", label = "y-range upper buffer:", min = 1, max = 4, step = 0.25, value = 2),
      )
    )
  )
# ----------------------------------------------------------------------------------------------------
)

# ----------------------------------------------------------------------------------------------------
# Server ---------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
# ----------------------------------------------------------------------------------------------------

  # ---------------------------------------------------------------------------------
  # Parameter Values ----------------------------------------------------------------
  # ---------------------------------------------------------------------------------
  
  parameterVals <- reactive({
    
    input$reset
    
    beta             = c(0.1, 0.23)
    sd               = c(0.09, 0.25)
    Npoints          = input$Npoints
    points_choice    = "partial"
    aspect_ratio     = 1
    linear           = c("true", "false")
    free_draw        = FALSE
    x_min            = 0
    x_max            = 20
    x_by             = 0.25
    ymin_scale       = input$ymin_scale
    ymax_scale       = input$ymax_scale
    draw_start_scale = 0.5
    points_end_scale = c(0.5, 0.75)
    
    data.frame(beta = beta, sd = sd) %>%
    expand_grid(points_end_scale, 
                points_choice, 
                Npoints, 
                aspect_ratio, 
                free_draw, 
                x_min,
                x_max,
                x_by,
                ymin_scale,
                ymax_scale,
                draw_start_scale,
                linear)
    
  })
  
  # ---------------------------------------------------------------------------------
  # Data set 1 ----------------------------------------------------------------------
  # ---------------------------------------------------------------------------------
  
  data_1 <- reactive({
    
    parms <- parameterVals()[1,]
    
                # generate line data
                line_data <- tibble(x = seq(parms$x_min, parms$x_max, parms$x_by), 
                                    y = exp(x*parms$beta)
                                    )
                
                # generate point data
                if(parms$points_choice == "full"){
                  xVals <- sample(line_data$x, parms$Npoints, replace = F)
                } else {
                  xVals <- sample(line_data$x[line_data$x <= max(line_data$x)*parms$points_end_scale], parms$Npoints, replace = F)
                }
                repeat{
                  errorVals <- rnorm(length(xVals), 0, parms$sd)
                  if(mean(errorVals[10]) < 2*parms$sd & mean(errorVals[10] > -2*parms$sd)){
                    break
                  }
                }
                point_data <- tibble(x = xVals,
                                     ypoints = exp(x*parms$beta + errorVals)
                                     )
                
                # combine line & point data
                full_join(line_data, point_data, by = "x") %>%
                  mutate(ypoints = ifelse(is.na(ypoints), -999, ypoints))
                
      })
  
  # Linear Scale --------------------------------------------------------------------
  
  message_loc_1.linear <- session$ns("drawr_message")
  output$shinydrawr_1.linear <- r2d3::renderD3({
    
    parms   <- parameterVals()[1,]
    data    <- data_1()
    y_range <- range(data$y) * c(parms$ymin_scale, parms$ymax_scale)
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_1.log,
          x_range           = range(data$x),
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # Log Scale -----------------------------------------------------------------------
  
  message_loc_1.log <- session$ns("drawr_message")
  output$shinydrawr_1.log <- r2d3::renderD3({
    
    parms   <- parameterVals()[2,]
    data    <- data_1()
    y_range <- range(data$y) * c(parms$ymin_scale, parms$ymax_scale)
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_1.log,
          x_range           = range(data$x),
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # ---------------------------------------------------------------------------------
  # Data set 2 ----------------------------------------------------------------------
  # ---------------------------------------------------------------------------------
  
  data_2 <- reactive({
    
    parms <- parameterVals()[3,]
    
    # generate line data
    line_data <- tibble(x = seq(parms$x_min, parms$x_max, parms$x_by), 
                        y = exp(x*parms$beta)
    )
    
    # generate point data
    if(parms$points_choice == "full"){
      xVals <- sample(line_data$x, parms$Npoints, replace = F)
    } else {
      xVals <- sample(line_data$x[line_data$x <= max(line_data$x)*parms$points_end_scale], parms$Npoints, replace = F)
    }
    repeat{
      errorVals <- rnorm(length(xVals), 0, parms$sd)
      if(mean(errorVals[10]) < 2*parms$sd & mean(errorVals[10] > -2*parms$sd)){
        break
      }
    }
    point_data <- tibble(x = xVals,
                         ypoints = exp(x*parms$beta + errorVals)
    )
    
    # combine line & point data
    full_join(line_data, point_data, by = "x") %>%
      mutate(ypoints = ifelse(is.na(ypoints), -999, ypoints))
    
  })
  
  # Linear Scale --------------------------------------------------------------------
  
  message_loc_2.linear <- session$ns("drawr_message")
  output$shinydrawr_2.linear <- r2d3::renderD3({
    
    parms   <- parameterVals()[3,]
    data    <- data_2()
    y_range <- range(data$y) * c(parms$ymin_scale, parms$ymax_scale)
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_1.log,
          x_range           = range(data$x),
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # Log Scale -----------------------------------------------------------------------
  
  message_loc_2.log <- session$ns("drawr_message")
  output$shinydrawr_2.log <- r2d3::renderD3({
    
    parms   <- parameterVals()[4,]
    data    <- data_2()
    y_range <- range(data$y) * c(parms$ymin_scale, parms$ymax_scale)
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_1.log,
          x_range           = range(data$x),
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # ---------------------------------------------------------------------------------
  # Data set 3 ----------------------------------------------------------------------
  # ---------------------------------------------------------------------------------
  
  data_3 <- reactive({
    
    parms <- parameterVals()[5,]
    
    # generate line data
    line_data <- tibble(x = seq(parms$x_min, parms$x_max, parms$x_by), 
                        y = exp(x*parms$beta)
    )
    
    # generate point data
    if(parms$points_choice == "full"){
      xVals <- sample(line_data$x, parms$Npoints, replace = F)
    } else {
      xVals <- sample(line_data$x[line_data$x <= max(line_data$x)*parms$points_end_scale], parms$Npoints, replace = F)
    }
    repeat{
      errorVals <- rnorm(length(xVals), 0, parms$sd)
      if(mean(errorVals[10]) < 2*parms$sd & mean(errorVals[10] > -2*parms$sd)){
        break
      }
    }
    point_data <- tibble(x = xVals,
                         ypoints = exp(x*parms$beta + errorVals)
    )
    
    # combine line & point data
    full_join(line_data, point_data, by = "x") %>%
      mutate(ypoints = ifelse(is.na(ypoints), -999, ypoints))
    
  })
  
  # Linear Scale --------------------------------------------------------------------
  
  message_loc_3.linear <- session$ns("drawr_message")
  output$shinydrawr_3.linear <- r2d3::renderD3({
    
    parms   <- parameterVals()[5,]
    data    <- data_3()
    y_range <- range(data$y) * c(parms$ymin_scale, parms$ymax_scale)
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_1.log,
          x_range           = range(data$x),
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # Log Scale -----------------------------------------------------------------------
  
  message_loc_3.log <- session$ns("drawr_message")
  output$shinydrawr_3.log <- r2d3::renderD3({
    
    parms   <- parameterVals()[6,]
    data    <- data_3()
    y_range <- range(data$y) * c(parms$ymin_scale, parms$ymax_scale)
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_1.log,
          x_range           = range(data$x),
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # ---------------------------------------------------------------------------------
  # Data set 4 ----------------------------------------------------------------------
  # ---------------------------------------------------------------------------------
  
  data_4 <- reactive({
    
    parms <- parameterVals()[7,]
    
    # generate line data
    line_data <- tibble(x = seq(parms$x_min, parms$x_max, parms$x_by), 
                        y = exp(x*parms$beta)
    )
    
    # generate point data
    if(parms$points_choice == "full"){
      xVals <- sample(line_data$x, parms$Npoints, replace = F)
    } else {
      xVals <- sample(line_data$x[line_data$x <= max(line_data$x)*parms$points_end_scale], parms$Npoints, replace = F)
    }
    repeat{
      errorVals <- rnorm(length(xVals), 0, parms$sd)
      if(mean(errorVals[10]) < 2*parms$sd & mean(errorVals[10] > -2*parms$sd)){
        break
      }
    }
    point_data <- tibble(x = xVals,
                         ypoints = exp(x*parms$beta + errorVals)
    )
    
    # combine line & point data
    full_join(line_data, point_data, by = "x") %>%
      mutate(ypoints = ifelse(is.na(ypoints), -999, ypoints))
    
  })
  
  # Linear Scale --------------------------------------------------------------------
  
  message_loc_4.linear <- session$ns("drawr_message")
  output$shinydrawr_4.linear <- r2d3::renderD3({
    
    parms   <- parameterVals()[7,]
    data    <- data_4()
    y_range <- range(data$y) * c(parms$ymin_scale, parms$ymax_scale)
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_1.log,
          x_range           = range(data$x),
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
  # Log Scale -----------------------------------------------------------------------
  
  message_loc_4.log <- session$ns("drawr_message")
  output$shinydrawr_4.log <- r2d3::renderD3({
    
    parms   <- parameterVals()[8,]
    data    <- data_4()
    y_range <- range(data$y) * c(parms$ymin_scale, parms$ymax_scale)
    
    drawr(data              = data,
          aspect_ratio      = parms$aspect_ratio,
          linear            = parms$linear,
          free_draw         = parms$free_draw,
          points            = parms$points_choice,
          x_by              = parms$x_by,
          draw_start        = parms$x_max*parms$draw_start_scale,
          points_end        = parms$x_max*parms$points_end_scale,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_1.log,
          x_range           = range(data$x),
          y_range           = y_range,
          title             = paste("Beta: ", parms$beta, "; sd: ", parms$sd, "; Points End: ", parms$points_end_scale, "; Linear: ", parms$linear, sep = "")
    )
    
  })
  
# ----------------------------------------------------------------------------------------------------
# Eye Fitting Straight Lines Replication Simulation Function -----------------------------------------
# ----------------------------------------------------------------------------------------------------
  
  
  ydi_data <- reactive({
    
  input$eyefitting_reset
    
  linearYDI_simFunc <- 
    function(y_xbar, slope, sigma, xmin = 0, xmax = 20, by = 0.25, N = 30, points_end_scale = 1){
      
      xLine <- seq(xmin, xmax, by = by)
      xPoint <- sample(xLine[xLine <= xmax*points_end_scale], size = N, replace = F)
      # From slope intercept form
      # y-y_xbar = m(x-xbar)
      # y = m(x-xbar) + y_xbar = mx - mxbar + y_xbar
      yintercept = y_xbar - slope*mean(xPoint)
      
      line_data <- tibble(x = xLine,
                          y = yintercept + slope*x)
      
      # generate point data
      repeat{
        errorVals <- rnorm(N, 0, sigma)
        if(mean(errorVals[10]) < 2*sigma & mean(errorVals[10] > -2*sigma)){
          break
        }
      }
      point_data <- tibble(x = xPoint,
                           ypoints = yintercept + slope*x + errorVals)
      
      data <- full_join(line_data, point_data, by = "x")%>%
        mutate(ypoints = ifelse(is.na(ypoints), -999, ypoints))
      
      # return(list(line_data = line_data, point_data = point_data, data = data))
      return(data)
      
    }
  
  ydi_parms <- data.frame(
    dataset = c("S", "F", "V", "N"),
    y_xbar = c(3.88, 3.9, 3.89, 4.11),
    slope  = c(0.66, 0.66, 1.98, -0.70),
    sigma  = c(1.3, 2.8, 1.5, 2.5),
    xmin   = c(0, 0, 5, 0),
    xmax   = c(20, 20, 15, 20)
  )

    ydi_parms %>%
      mutate(data = purrr::pmap(list(y_xbar, slope, sigma, xmin, xmax), linearYDI_simFunc)) %>%
      unnest(data)
  })
  
  y_range <- reactive({
    ydi_data <- ydi_data()
    range(ydi_data$ypoints[ydi_data$ypoints > -999]) * c(1, 1)
  })
  
  x_range <- reactive({
    ydi_data <- ydi_data()
    range(ydi_data$x)
  })
  
  # S ----------------------------------------------------------
  
  message_loc_S <- session$ns("drawr_message")
  output$shinydrawr_S <- r2d3::renderD3({
    
    data    <- ydi_data() %>%
      filter(dataset == "S")
    
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
          x_range           = x_range(),
          y_range           = y_range(),
          title             = "S"
    )
    
  })
  
  # F ----------------------------------------------------------
  
  message_loc_F <- session$ns("drawr_message")
  output$shinydrawr_F <- r2d3::renderD3({
    
    data    <- ydi_data() %>%
      filter(dataset == "F")
    
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
          x_range           = x_range(),
          y_range           = y_range(),
          title             = "F"
    )
    
  })
  
  # V ----------------------------------------------------------
  
  message_loc_V <- session$ns("drawr_message")
  output$shinydrawr_V <- r2d3::renderD3({
    
    data    <- ydi_data() %>%
      filter(dataset == "V")
    
    drawr(data              = data,
          aspect_ratio      = 1,
          linear            = "true",
          free_draw         = TRUE,
          points            = "full",
          x_by              = 0.25,
          draw_start        = 5.1,
          points_end        = 15,
          show_finished     = input$eyefitting_show_finished,
          shiny_message_loc = message_loc_S,
          x_range           = x_range(),
          y_range           = y_range(),
          title             = "V"
    )
    
  })
  
  # F ----------------------------------------------------------
  
  message_loc_N <- session$ns("drawr_message")
  output$shinydrawr_N <- r2d3::renderD3({
    
    data    <- ydi_data() %>%
      filter(dataset == "N")
    
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
          x_range           = x_range(),
          y_range           = y_range(),
          title             = "N"
    )
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
