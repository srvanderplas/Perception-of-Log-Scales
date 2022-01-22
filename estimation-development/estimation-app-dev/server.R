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
      data <- tibble(x = seq(input$yearRange[1], input$yearRange[2], 1),
                     y = input$alpha0*exp(input$beta0*(x-input$yearRange[1]) + rnorm(length(x), mean = 0, sd = input$sigma0)) + input$gamma0) %>%
              mutate(x0 = x - input$yearRange[1] + 10)
      
      lm0 <- lm(log(y) ~ x0, data = data)
      alpha0 <- exp(coef(lm0)[1] %>% as.numeric)
      beta0  <- coef(lm0)[2] %>% as.numeric
      gamma0 <- min(data$y)
      start0 <- list(alpha = alpha0, beta = beta0, gamma = gamma0)
      nlme.mod <- nls(y ~ alpha*exp(beta*x0) + gamma,
                      data = data,
                      start = start0)
      
      data$yhat = predict(nlme.mod)
      
      data
      
    })
    
    yMax <- reactive({
      data <- onePop_data()
      round_any(max(data$y), 1000, f = ceiling)
    })
    
    yMin <- reactive({
      data <- onePop_data()
      
      if(input$scale == "Linear"){
        0
      } else if (input$scale == "Log10"){
        round_any(min(data$y), 10, f = floor)
      } else if (input$scale == "Log2") {
        round_any(min(data$y), 2, f = floor)
      }
      
    })
    
    output$popRange <- renderUI({
      sliderInput("yRange",
                  "Population Range",
                  min = yMin()*-1.1,
                  max = yMax()*1.1,
                  value = c(yMin(),yMax()),
                  step = 2)
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
          geom_line(aes(y = yhat), color = "blue")
      }
      
      if (input$scale == "Linear"){
        basePlot +
          scale_y_continuous("Population", limits = c(input$yRange[1], input$yRange[2]), labels = comma)
      } else if (input$scale == "Log10"){
        basePlot +
          scale_y_continuous("Population", limits = c(input$yRange[1], input$yRange[2]), trans = "log10")
      } else if (input$scale == "Log2"){
        basePlot +
          scale_y_continuous("Population", limits = c(input$yRange[1], input$yRange[2]), trans = "log2")
      }

    })

})
