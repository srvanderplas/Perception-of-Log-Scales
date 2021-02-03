# LOAD LIBRARIES
library(shiny)
library(r2d3)
library(tibble)

# DEFINE UI FOR APPLICATION
ui <- fluidPage(
    # verbatimTextOutput("selected"),
    d3Output("d3")
)

# DEFINE SERVER LOGIC REQIRED TO DRAW PLOT
server <- function(input, output) {
    
    data <- reactive({
        tibble(x = seq(1, 25, .5), y = exp((x-15)/30))
        # c(0.1, 0.2, 0.3, 0.4, 0.2)
    })
    
    output$d3 <- renderD3({
        r2d3(
            data(),
            script = "line.js"
        )
    })
    
    # output$selected <- renderText({
        # bar_number <- as.numeric(req(input$bar_clicked))
        # if (bar_number > 0) cos(bar_number)
    # })
    
}

# RUN THE APPLICATION
shinyApp(ui = ui, server = server)
