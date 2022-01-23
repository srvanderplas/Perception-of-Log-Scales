library(shiny)
library(tidyverse)
library(patchwork)
library(scales)
library(plyr)
library(here)
library(nlme)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Single Population
    
    onePop_data <- reactive({
      
      set.seed(56156)
      if (input$multiplicativeErrors){
        data <- tibble(x = seq(input$yearRange[1], input$yearRange[2], 1),
                       y = input$alpha0*exp(input$beta0*(x-input$yearRange[1]) + rnorm(length(x), mean = 0, sd = input$sigma0)) + input$theta0,
                       y0 = input$alpha0*exp(input$beta0*(x-input$yearRange[1])) + input$theta0) %>%
                mutate(x0 = x - input$yearRange[1])
      } else if (!input$multiplicativeErrors){
        data <- tibble(x = seq(input$yearRange[1], input$yearRange[2], 1),
                       y = input$alpha0*exp(input$beta0*(x-input$yearRange[1])) + input$theta0 + rnorm(length(x), mean = 0, sd = input$sigma0),
                       y0 = input$alpha0*exp(input$beta0*(x-input$yearRange[1])) + input$theta0) %>%
          mutate(x0 = x - input$yearRange[1])
      }
      
      if (input$trendline) {
        theta0 <- min(data$y0)
        lm0 <- lm(log(y - theta0) ~ x0, data = data)
        alpha0 <- exp(coef(lm0)[1])
        beta0  <- coef(lm0)[2]
        start0 <- list(alpha = alpha0, beta = beta0, theta = theta0)
        nls.mod <- nls(y ~ alpha*exp(beta*x0) + theta,
                       data = data,
                       start = start0)
        data$yhat = fitted(nls.mod)
      }
      
      data
    })
    
    output$nlsCoef_table <- renderText({
      
      data <- onePop_data()
      
      if (input$trendline) {
        theta0 <- min(data$y0)
        lm0 <- lm(log(y - theta0) ~ x0, data = data)
        alpha0 <- exp(coef(lm0)[1])
        beta0  <- coef(lm0)[2]
        start0 <- list(alpha = alpha0, beta = beta0, theta = theta0)
        nls.mod <- nls(y ~ alpha*exp(beta*x0) + theta,
                        data = data,
                        start = start0)
        coef(nls.mod)
      }
    })
    
    # SET UP Y AXIS TICK MARKS
    yMax <- reactive({
      data <- onePop_data()
      round_any(max(data$y), 1000, f = ceiling)
    })
    
    yMin <- reactive({
      data <- onePop_data()
      
      if(input$scale == "Linear"){
        0
      } else if (input$scale %in% c("Log10", "Log2")){
        1
      }
      
    })
    
    yTicks <- reactive({
      data <- onePop_data()
      
      if(input$scale == "Linear"){
        1000
      } else if (input$scale %in% c("Log10", "Log2")){
        1
      } 
      
    })
    
    output$popRange <- renderUI({
      tagList(
          numericInput("popMin",
                      "Population Min",
                      value = yMin()
                      ),
          numericInput("popMax",
                       "Population Max",
                       value = yMax()
                      ),
          numericInput("tickmarks",
                       "Population Tick Marks",
                       value = yTicks()
          )
      )
    })
    
    output$onePop_plot <- renderPlot({
      
      data <- onePop_data()
      
      basePlot <- data %>%
        ggplot(aes(x = x, y = y)) +
        geom_point() +
        theme_bw() +
        theme(aspect.ratio = 1,
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12)
              )+
        scale_x_continuous("Year", expand = c(0.01,0.01))
      
      if(input$trendline){
        basePlot <- basePlot +
          geom_line(aes(y = yhat), color = "blue", size = 1) +
          geom_line(aes(y = y0), color = "orange2", size = 1)
      }
      
      if (input$scale == "Linear"){
        basePlot +
          scale_y_continuous("Population", 
                             limits = c(input$popMin, input$popMax),
                             breaks = seq(0,input$popMax,input$tickmarks),
                             labels = comma)
          # scale_y_continuous("Population", labels = comma)
      } else if (input$scale == "Log10"){
        basePlot +
          scale_y_continuous("Population", 
                             limits = c(input$popMin, input$popMax), 
                             trans = "log10",
                             breaks = 10^seq(0,10000,input$tickmarks),
                             labels = comma
                             )
      } else if (input$scale == "Log2"){
        basePlot +
          scale_y_continuous("Population", 
                             limits = c(input$popMin, input$popMax), 
                             trans = "log2",
                             breaks = 2^seq(0,10000,input$tickmarks),
                             labels = comma
                             )
      }

    })

})
