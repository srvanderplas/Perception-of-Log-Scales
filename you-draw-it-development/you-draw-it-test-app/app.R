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
  
  plot_data <- dplyr::select(data, x, y)
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
                            pin_start = T, 
                            x_range = x_range, 
                            y_range = y_range,
                            line_style = NULL,
                            data_tab1_color = data_tab1_color, 
                            drawn_line_color = drawn_line_color,
                            free_draw = F, 
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
  
  # ---- Tab 1 ---------------------------------------------------------------
  
  # Provides a line for the data. (no errors)
  data_tab1 <- tibble(x = 1:30, y = exp((x-15)/5))

  # Provides the yrange for the plot (spans the data points + some)
  y_range <- range(data_tab1$y) * c(.7, 1.1)

  # Tells us where the user can start drawing
  draw_start <- 20

  # Let's talk about message_loc and drawr_message??
  # Somehow takes the points the user drew and records them.
  message_loc <- session$ns("drawr_message") ################################################ HERE
  drawn_data <- shiny::reactiveVal()

  # shinydrawer is the id of our "wrapper" this is what draws our line
  output$shinydrawr <- r2d3::renderD3({
    # if linear box is checked, then T else F
    islinear <- ifelse(input$drawr_linear_axis, "true", "false")
    # Use redef'd drawr function...r2d3 is built into here.. how do we add points???
    drawr(data = data_tab1,
          linear = islinear, # see function above
          draw_start = draw_start, # we define this at the top of the app
          shiny_message_loc = message_loc,
          x_range = range(data_tab1$x), # covers the range of the sequence we define
          y_range = y_range, # we define this above to span our data + some
          drawn_line_color = "skyblue", # color the user's line is drawn in
          data_tab1_color = "blue" # color the original line is drawn in.
          )
  })

  # Clears Recorded Data table when you toggle between log and linear
  shiny::observeEvent(input$drawr_linear_axis, {
    drawn_data(NULL)
  })

  # creates data set that contains the x value, actual y value, and drawn y value (drawn = input$drawr_message
  # for the x values >= starting draw point
  # WHAT DOES THE %>% drawn_data() do?? does that rename it so we can reference this?
  shiny::observeEvent(input$drawr_message, { ################################################ HERE
    data_tab1 %>%
      dplyr::filter(x >= draw_start) %>%
      dplyr::mutate(drawn = input$drawr_message) %>%
      drawn_data()
  })

  # Fills in the Recorded Data table... see id drawrmessage in developer tools on browser.
  output$drawrmessage <- renderTable({
    drawn_data()
  })

}

# Run the application
shinyApp(ui = ui, server = server)
