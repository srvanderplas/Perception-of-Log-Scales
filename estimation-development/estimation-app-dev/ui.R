library(shiny)
library(tidyverse)
library(shiny)
library(tidyverse)
library(patchwork)
library(scales)
library(here)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Estimation Task Development"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("numPops",
                        "Number of Populations",
                        choices = c("One", "Two")),
            
            conditionalPanel("input.numPops == 'One'",
                             
              helpText(h5("Elementary: literal reading of the data")),
              numericInput("qE1",
                        "What is the population of creatures in year 3010?",
                        value = NA,
                        min = 0,
                        max = 10000000),
              numericInput("qE2",
                           "In what year does the population of creatures reach 4,000?",
                           value = NA,
                           min = 3000,
                           max = 3050),
              
              helpText(h5("Intermediate: reading between the data")),
              numericInput("qI1",
                           "How many creatures did the population increase by between 3020 and 3040? (additive)",
                           value = NA,
                           min = 0,
                           max = 10000000),
              numericInput("qI2",
                           "How many times more creatures are there in 3040 than in 3020? (multiplicative)",
                           value = NA,
                           min = 0,
                           max = 50),
              numericInput("qI3",
                           "How many years does it take for the initial population of creatures in 3000 to double? (multiplicative)",
                           value = NA,
                           min = 0,
                           max = 100),
              textInput("qI4",
                        "Between 3030 and 3040, how does the population of creatures change? (open-ended; look for wording)")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          conditionalPanel("input.numPops == 'One'",
            column(width = 7,
              helpText(h4("Scenario: words go here.")),
              tabPanel("Single Population", plotOutput("onePop_plot", height = "500px"))
            ),
            column(width = 5,
                   helpText(h4("Data Simulation")),
                   withMathJax(helpText("$$\\text{population} = \\alpha\\cdot e^{\\beta\\cdot (\\text{year} - \\text{min year}) + \\epsilon} + \\gamma$$")),
                   withMathJax(helpText("$$\\text{where } \\epsilon \\sim N(0, \\sigma^2)$$")),
                   
                   fluidRow(
                   column(width = 3,
                          numericInput("alpha0", 
                                       "$$\\alpha$$",
                                       value = 5,
                                       min = 1, 
                                       max = 50)
                   ),
                   column(width = 3,
                          numericInput("beta0", 
                                       "$$\\beta$$",
                                       value = 0.15,
                                       min = 0, 
                                       max = 5,
                                       step = 0.05)
                   ),
                   column(width = 3,
                          numericInput("gamma0", 
                                       "$$\\gamma$$",
                                       value = 30,
                                       min = 0, 
                                       max = 500),
                   ),
                   column(width = 3,
                          numericInput("sigma0", 
                                       "$$\\sigma$$",
                                       value = 0.25,
                                       min = 0, 
                                       max = 3,
                                       step = 0.05),
                   )
                   ),
                   
                   sliderInput("yearRange", 
                               "Year Range", 
                               min = 3000,
                               max = 3100,
                               value = c(3000, 3050)),
                   
                   helpText(h4("Graph Aesthetics")),
                   selectInput("scale",
                               "Scale",
                               choices = c("Linear", "Log10", "Log2"),
                               selected = "Linear"),
                   uiOutput("popRange"),
                   checkboxInput("trendline",
                                 "Trendline",
                                 value = F)
            )
          )
        )
    )
))
