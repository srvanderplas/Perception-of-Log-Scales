# Load libraries
library(shiny)
library(shinyjs)
library(r2d3)
library(tidyverse)
library(gridSVG)
library(lubridate)
library(readxl)

# Redefine drawr function
drawr <- function(data, 
                  linear = "true", 
                  draw_start = mean(data$x),
                  free_draw = T,
                  points = "half",
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
                  # show_finished = F,
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
                            linear = as.character(linear),
                            free_draw = free_draw, 
                            points = points,
                            aspect_ratio = aspect_ratio,
                            pin_start = T, 
                            x_range = x_range, 
                            y_range = y_range,
                            line_style = NULL,
                            data_tab1_color = data_tab1_color, 
                            drawn_line_color = drawn_line_color,
                            shiny_message_loc = shiny_message_loc)
             )
  
}


# Define UI for application that draws a histogram
ui <- navbarPage(
  "You Draw It Development",
  
  # ---- Tab 1 ---------------------------------------------------------------
  tabPanel(
    title = "Test",
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "d3.css")
    ),
    fluidRow(
      column(
        width = 9,
        # This is our "wrapper"
        d3Output("shinydrawr"),
        helpText("What will this line look like for the rest of the x values?
                  Using your mouse, draw on the plot to fill in the values,
                  maintaining the previous trend.")
      ),
      column(
        width = 3,
        # drawr_linear_axis is the linear/log checkbox. This has an if statement attached to it. 
        checkboxInput("drawr_linear_axis", "Linear Y Axis?", value = T),
        checkboxInput("free_draw_box", "Free Draw?", value = F),
        radioButtons("points_choice", "Points?", choices = c("full", "half", "none"), selected = "full"),
        sliderInput("aspect_ratio_slider", "Aspect Ratio:", min = 1, max = 4, value = 1.5, step = 0.1),
        sliderInput("draw_start_slider", "Draw Start?", min = 4, max = 19, value = 10, step = 1),
        sliderInput("ymag_range", label = "y-range buffer:", min = 0.5, max = 2, value = c(0.7, 1.3)),
        hr(),
        numericInput("by", "Stepby:", min = 0.05, max = 0.5, value = 0.2, step = 0.05),
        numericInput("beta", "Beta:", min = 0.01, max = 0.5, value = 0.1, step = 0.01),
        numericInput("sd", "SD:", min = 0.01, max = 0.25, value = 0.1, step = 0.01),
        numericInput("Npoints", "N Points:", min = 20, max = 50, value = 30, step = 5),
        hr(),
        p("Recorded Data:"),
        # drawrmessage is the table id for the Recorded data
        tableOutput("drawrmessage")
      )
    )
  )
  # ---- End UI --------------------------------------------------------------
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # message_loc and drawr_message
  # Somehow takes the points the user drew and records them.
  message_loc <- session$ns("drawr_message")
  drawn_data <- shiny::reactiveVal()
  
  # Provides a line for the data. (no errors)
  dataInput <- reactive({
                
                # generate line data
                line_data <- tibble(x = seq(1, 20, input$by), 
                                    y = exp(x*input$beta)
                                    )
                
                # generate point data
                point_data <- tibble(x = sample(line_data$x, input$Npoints, replace = F),
                                     ypoints = exp(x*input$beta + rnorm(length(x), 0, input$sd))
                                     )
                
                full_join(line_data, point_data, by = "x") %>%
                  mutate(ypoints = ifelse(is.na(ypoints), 0, ypoints))
                
                })
  
  # shinydrawer is the id of our "wrapper" this is what draws our line
  output$shinydrawr <- r2d3::renderD3({
    
    data <- dataInput()
    y_range <- range(data$y) * c(as.numeric(input$ymag_range[1]), as.numeric(input$ymag_range[2]))
    
    # if linear box is checked, then T else F
    islinear <- ifelse(input$drawr_linear_axis, "true", "false")

    # Use redef'd drawr function...r2d3 is built into here.. how do we add points???
    drawr(data              = data,
          aspect_ratio      = input$aspect_ratio_slider,
          linear            = islinear, # see function above
          free_draw         = input$free_draw_box,
          points            = input$points_choice,
          draw_start        = input$draw_start_slider, # we define this at the top of the app
          shiny_message_loc = message_loc,
          x_range           = range(data$x), # covers the range of the sequence we define
          y_range           = y_range, # we define this above to span our data + some
          drawn_line_color  = "steelblue" # color the user's line is drawn in
          )
  })

  # Clears Recorded Data table when you toggle between log and linear
  shiny::observeEvent(input$drawr_linear_axis, {
    drawn_data(NULL)
  })

  # Clears Recorded Data table when you toggle between free draw
  shiny::observeEvent(input$free_draw_box, {
    drawn_data(NULL)
  })
  
  # creates data set that contains the x value, actual y value, and drawn y value (drawn = input$drawr_message
  # for the x values >= starting draw point
  # WHAT DOES THE %>% drawn_data() do?? does that rename it so we can reference this?
  shiny::observeEvent(input$drawr_message, {
    
    if(input$free_draw_box){
    dataInput() %>%
      # dplyr::filter(x >= input$draw_start_slider) %>%
      dplyr::mutate(drawn = input$drawr_message) %>%
      dplyr::select(x, y, drawn) %>%
      drawn_data()
    } else{
      dataInput() %>%
        dplyr::filter(x >= input$draw_start_slider) %>%
        dplyr::mutate(drawn = input$drawr_message) %>%
        dplyr::select(x, y, drawn) %>%
        drawn_data()
    }
    
  })

  # Fills in the Recorded Data table... see id drawrmessage in developer tools on browser.
  output$drawrmessage <- renderTable({
    drawn_data()
  })

}

# Run the application
shinyApp(ui = ui, server = server)
