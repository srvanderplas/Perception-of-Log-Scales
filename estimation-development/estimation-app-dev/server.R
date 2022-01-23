library(shiny)
library(tidyverse)
library(patchwork)
library(scales)
library(plyr)
library(here)
library(nlme)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # --------------------------------------------------------------------------
    # One Population -----------------------------------------------------------
    # --------------------------------------------------------------------------
  
    # Output simulated data set
    onePop_data <- reactive({
      
      set.seed(56156)
      
      # simulate data with multiplicative errors
      if (input$multiplicativeErrors){
        
        data <- tibble(x = seq(input$yearRange[1], input$yearRange[2], 1),
                       y = input$alpha0*exp(input$beta0*(x-input$yearRange[1]) + rnorm(length(x), mean = 0, sd = input$sigma0)) + input$theta0,
                       y0 = input$alpha0*exp(input$beta0*(x-input$yearRange[1])) + input$theta0) %>%
                mutate(x0 = x - input$yearRange[1])
        
      # simulate data with additive errrors
      } else if (!input$multiplicativeErrors){
        
        data <- tibble(x = seq(input$yearRange[1], input$yearRange[2], 1),
                       y = input$alpha0*exp(input$beta0*(x-input$yearRange[1])) + input$theta0 + rnorm(length(x), mean = 0, sd = input$sigma0),
                       y0 = input$alpha0*exp(input$beta0*(x-input$yearRange[1])) + input$theta0) %>%
          mutate(x0 = x - input$yearRange[1])
        
      }
      
      # fit a trendline to the data using nls
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
    
    # output nls coefficients
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
    
    # Set up y-axis range and tickmarks
    yMin <- reactive({
      data <- onePop_data()
      
      if(input$scale == "Linear"){
        0
      } else if (input$scale %in% c("Log10", "Log2")){
        1
      }
      
    })
    
    yMax <- reactive({
      data <- onePop_data()
      round_any(max(data$y), 1000, f = ceiling)
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
    
    # Output scatterplot of data
    output$onePop_plot <- renderPlot({
      
      data <- onePop_data()
      
      # create base scatterplot
      basePlot <- data %>%
        ggplot(aes(x = x, y = y)) +
        geom_point() +
        theme_bw() +
        theme(aspect.ratio = 1,
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12)
              )+
        scale_x_continuous("Year", expand = c(0.01,0.01))
      
      # add the trendline if specified
      if(input$trendline){
        
        basePlot <- basePlot +
          geom_line(aes(y = yhat), color = "blue", size = 1) +
          geom_line(aes(y = y0), color = "orange2", size = 1)
        
      }
      
      # adjust y scale based on specifications (linear/log, range, tick marks)
      if (input$scale == "Linear"){
        
        basePlot +
          scale_y_continuous("Population", 
                             limits = c(input$popMin, input$popMax),
                             breaks = seq(0,input$popMax,input$tickmarks),
                             labels = comma)
        
      } else if (input$scale == "Log10"){
        
        basePlot +
          scale_y_continuous("Population", 
                             limits = c(input$popMin, input$popMax), 
                             trans = "log10",
                             breaks = 10^seq(0,10000,input$tickmarks),
                             labels = comma)
        
      } else if (input$scale == "Log2"){
        
        basePlot +
          scale_y_continuous("Population", 
                             limits = c(input$popMin, input$popMax), 
                             trans = "log2",
                             breaks = 2^seq(0,10000,input$tickmarks),
                             labels = comma)
      }
    })
    
    # --------------------------------------------------------------------------
    # Two Populations ----------------------------------------------------------
    # --------------------------------------------------------------------------
    
    twoPop_data <- reactive({
      
      set.seed(56156)
      datapoints <- tibble(x = seq(3000, 3050, 1),
                           pop1 = input$alpha1*exp(input$beta1*(x-3000) + rnorm(length(x), 0, input$sigma1)) + input$theta1,
                           pop2 = input$alpha2*exp(input$beta2*(x-3000) + rnorm(length(x), 0, input$sigma2)) + input$theta2,
                     ) %>%
        pivot_longer(cols = c("pop1", "pop2"),
                     names_to = "group",
                     values_to = "population")
      
      datalines <- tibble(x = seq(3000, 3050, 1),
                          pop1 = input$alpha1*exp(input$beta1*(x-3000)) + input$theta1,
                          pop2 = input$alpha2*exp(input$beta2*(x-3000)) + input$theta2
      ) %>%
        pivot_longer(cols = c("pop1", "pop2"),
                     names_to = "group",
                     values_to = "trendline")
      
      data <- full_join(datapoints, datalines, by = c("x", "group"))
      
      data
      
    })
    
    output$twoPop_plot <- renderPlot({
      
      data <- twoPop_data()
      
      baseplot <- data %>%
        ggplot(aes(x = x, y = population, color = group, group = group)) +
        geom_point() +
        theme_bw() +
        theme(aspect.ratio = 1,
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              legend.position = "bottom"
        ) +
        scale_x_continuous("Year") +
        scale_color_manual(values = c("navy", "orange2"))
      
      if(input$trendline2){
        
        baseplot <- baseplot + 
          geom_line(aes(y = trendline), size = 1)
        
      }
      
      if (input$scale2 == "Linear"){
        
        baseplot + 
          scale_y_continuous("Population", labels = comma)
        
      } else if (input$scale2 == "Log10"){
        
        baseplot + 
          scale_y_continuous("Population", 
                             labels = comma,
                             trans = "log10",
                             breaks = 10^seq(0,10000,1)
                             )
        
      } else if (input$scale2 == "Log2"){
        
        baseplot + 
          scale_y_continuous("Population", 
                             labels = comma,
                             trans = "log2",
                             breaks = 2^seq(0,10000,1)
                             )
        
      }
      
    })

})
