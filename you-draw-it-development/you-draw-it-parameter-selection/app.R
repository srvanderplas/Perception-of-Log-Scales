# Load libraries
library(shiny)
library(shinyjs)
library(r2d3) # ALERT: REQUIRES VERSION 
# url_r2d3v0.2.3 <- "https://cran.r-project.org/src/contrib/Archive/r2d3/r2d3_0.2.3.tar.gz"
# install.packages(url_r2d3v0.2.3, repos = NULL, type = 'source')
library(tidyverse)
library(gridSVG)
library(lubridate)
library(readxl)
library(DT)

# Redefine drawr function
drawr <- function(data, 
                  linear = "true", 
                  draw_start = mean(data$x),
                  points_end = max(data$x)*(3/4),
                  x_by = 0.25,
                  free_draw = T,
                  points = "partial",
                  aspect_ratio = 1.5,
                  title = "", 
                  x_range = NULL, 
                  y_range = NULL,
                  x_lab = "", 
                  y_lab = "", 
                  drawn_line_color = "orangered",
                  data_tab1_color = "steelblue", 
                  x_axis_buffer = 0.01, 
                  y_axis_buffer = 0.05,
                  show_finished = T,
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

  r2d3::r2d3(plot_data, "main.js",
             dependencies = c("d3-jetpack"),
             options = list(draw_start = draw_start, 
                            points_end = points_end,
                            linear = as.character(linear),
                            free_draw = free_draw, 
                            points = points,
                            aspect_ratio = aspect_ratio,
                            pin_start = T, 
                            x_range = x_range,
                            x_by = x_by,
                            y_range = y_range,
                            line_style = NULL,
                            data_tab1_color = data_tab1_color, 
                            drawn_line_color = drawn_line_color,
                            show_finished = show_finished,
                            shiny_message_loc = shiny_message_loc)
             )
  
}

ui <- navbarPage(
  "You Draw It Development",
  
  tabPanel(
    title = "Parameter Selection",
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "d3.css")
    ),
    fluidRow(
      column(
        width = 4,
        d3Output("shinydrawr_1.linear", height = "350px"),
      ),
      column(
        width = 4,
        d3Output("shinydrawr_1.log", height = "350px")
      ),
      column(
        width = 2,
        checkboxInput("show_finished", "Show Finished?", value = T)
      )
    )
  )
  # ---- End UI --------------------------------------------------------------
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Provides a line for the data. (no errors)
  data1 <- reactive({
    
                beta <- 0.2
                sd   <- 0.15
                points_choice <- "partial"
                Npoints <- 30
    
                # generate line data
                line_data <- tibble(x = seq(1, 20, 0.25), 
                                    y = exp(x*beta)
                                    )
                
                # generate point data
                if(points_choice == "full"){
                  xVals <- sample(line_data$x, Npoints, replace = F)
                } else {
                  xVals <- sample(line_data$x[line_data$x <= max(line_data$x)*0.75], Npoints, replace = F)
                }
                repeat{
                  errorVals <- rnorm(length(xVals), 0, sd)
                  if(mean(errorVals[10]) < 2*sd & mean(errorVals[10] > -2*sd)){
                    break
                  }
                }
                point_data <- tibble(x = xVals,
                                     ypoints = exp(x*beta + errorVals)
                                     )
                
                full_join(line_data, point_data, by = "x") %>%
                  mutate(ypoints = ifelse(is.na(ypoints), 0, ypoints))
                
                })
  
  message_loc_1.linear <- session$ns("drawr_message")
  output$shinydrawr_1.linear <- r2d3::renderD3({
    
    data <- data1()
    y_range <- range(data$y) * c(0.5, 3)
    
    drawr(data              = data,
          aspect_ratio      = 1,
          linear            = "true",
          free_draw         = FALSE,
          points            = "partial",
          x_by              = 0.25,
          draw_start        = 10,
          points_end        = 15,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_1.linear,
          x_range           = range(data$x),
          y_range           = y_range,
          drawn_line_color  = "steelblue"
          )
  })
  
  message_loc_1.log <- session$ns("drawr_message")
  output$shinydrawr_1.log <- r2d3::renderD3({
    
    data <- data1()
    y_range <- range(data$y) * c(0.5, 3)
    
    drawr(data              = data,
          aspect_ratio      = 1,
          linear            = "false",
          free_draw         = FALSE,
          points            = "partial",
          x_by              = 0.25,
          draw_start        = 10,
          points_end        = 15,
          show_finished     = input$show_finished,
          shiny_message_loc = message_loc_1.log,
          x_range           = range(data$x),
          y_range           = y_range,
          drawn_line_color  = "steelblue"
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
