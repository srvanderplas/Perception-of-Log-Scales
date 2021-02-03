# LOAD LIBRARIES
library(shiny)
library(r2d3)
library(tibble)
library(tidyverse)

# DEFINE UI FOR APPLICATION
ui <- fluidPage(
    # verbatimTextOutput("selected"),
    d3Output("d3", width = "70%")
)

# DEFINE SERVER LOGIC REQIRED TO DRAW PLOT
server <- function(input, output) {
    
    data <- reactive({
        
        ### Nice straight line (no errors)
        tibble(x = seq(1, 25, .2), y = exp((x-15)/30))
        
        ### Points simulated by exponential model from lineup study
        # read.csv("example-data-from-lineup.csv") %>%
        #     filter(.sample == 1)
        
        ### Used with bar.js (default internet example)
        # c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
    })
    
    output$d3 <- renderD3({
        
        ### You draw it js code used to draw a line
        r2d3(
            data(),
            options = list(free_draw = FALSE, draw_start = 10, pin_start = TRUE, x_range = c(0,28), y_range = c(.5,3), line_style = list(strokeWidth = 4), data_line_color = 'steelblue', drawn_line_color = 'orangered', show_finished = TRUE, shiny_message_loc = 'my_shiny_app', linear = 'true'), dependencies = c('d3-jetpack'),
            script = "you-draw-it.js"
        )
        
        # Emily's attempt at drawing a line
        # r2d3(
        #     data(),
        #     script = "line.js"
        # )
        
        # Default internet example
        # r2d3(
        #     data(),
        #     script = "bar.js"
        # )
    })
    
    ### Used with bar.js for activity (default internet example)
    # output$selected <- renderText({
        # bar_number <- as.numeric(req(input$bar_clicked))
        # if (bar_number > 0) cos(bar_number)
    # })
    
}

# RUN THE APPLICATION
shinyApp(ui = ui, server = server)
