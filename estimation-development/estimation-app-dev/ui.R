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

    # --------------------------------------------------------------------------
    # Sidebar ------------------------------------------------------------------
    # --------------------------------------------------------------------------
    
    sidebarLayout(
        sidebarPanel(
            selectInput("numPops",
                        "Number of Populations",
                        choices = c("One", "Two")),
            
            # ------------------------------------------------------------------
            # ONE POPULATION ---------------------------------------------------
            # ------------------------------------------------------------------
            
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
            ),
            
            # ------------------------------------------------------------------
            # TWO POPULATIONS --------------------------------------------------
            # ------------------------------------------------------------------
            
            conditionalPanel("input.numPops == 'Two'",
                             
              helpText(h5("Elementary: literal reading of the data")),

              numericInput("q2E3",
                           "What is the population when both groups are equal?",
                           value = NA,
                           min = 0,
                           max = 10000000),
              
              numericInput("q2E3",
                           "How many years does it take for the population of group 1 to equal the population of group 2?",
                           value = NA,
                           min = 0,
                           max = 100),
              
              helpText(h5("Intermediate: reading between the data")),
              
              numericInput("q2I1",
                           "How many more creatures did group 2 have than group 1 in 3020? (additive)",
                           value = NA,
                           min = 0,
                           max = 10000000),
              
              numericInput("q2I2",
                           "How many times larger was the starting population of group 2 than group 1? (multiplicative)",
                           value = NA,
                           min = 0,
                           max = 10000000),
              
              textInput("q2I3",
                        "Considering the two curves of the graph only as marks on a piece of paper, how do the changes in these two curves compare? (open-ended)"
                        ),
              
              helpText(h5("Advanced: reading beyond the data")),
              
              textInput("q2A1",
                        "From 3000 to 3020, what which population had a higher reproductive rate?"
                        ),
              
              textInput("q2A1",
                        "Which population do you expect to be larger in 3100?"
              ),
            )
        ),
        
    # --------------------------------------------------------------------------
    # MAIN ---------------------------------------------------------------------
    # --------------------------------------------------------------------------
    
    mainPanel(
      
      # ------------------------------------------------------------------------
      # ONE POPULATION ---------------------------------------------------------
      # ------------------------------------------------------------------------
      conditionalPanel("input.numPops == 'One'",
        
        # Output Plot ----------------------------------------------------------
        
        column(width = 7,
          img(src='tribbles.png', width="100%", align = "center"),
          plotOutput("onePop_plot", height = "500px"),
          verbatimTextOutput("nlsCoef_table")
        ),
        
        # Data & Plot Specifications -------------------------------------------
        
        column(width = 5,
               helpText(h4("Data Simulation")),
               withMathJax(helpText("$$\\text{population} = \\alpha\\cdot e^{\\beta\\cdot (\\text{year} - \\text{min year})} + \\theta$$")),
               checkboxInput("multiplicativeErrors",
                             "Multiplicative Errors",
                              value = T),
                  
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
                        numericInput("theta0", 
                                     "$$\\theta$$",
                                     value = 30,
                                     min = -50, 
                                     max = 500)
                 ),
                 column(width = 3,
                        numericInput("sigma0", 
                                     "$$\\sigma$$",
                                     value = 0.25,
                                     min = 0, 
                                     max = 3,
                                     step = 0.05)
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
      ),
      
      # ------------------------------------------------------------------------
      # TWO POPULATIONS ---------------------------------------------------------
      # ------------------------------------------------------------------------
      
      conditionalPanel("input.numPops == 'Two'",
                       
          column(width = 7,
                 helpText(h4("Scenario: words go here.")),
                 plotOutput("twoPop_plot", height = "500px")
                 ),
          
          column(width = 5,
                 
                 helpText(h4("Data Simulation")),
                 
                 withMathJax(helpText("$$\\text{population 1} = \\alpha_1\\cdot e^{\\beta_1\\cdot (\\text{year} - \\text{min year}) + \\epsilon} + \\theta_1$$")),
                 
                 fluidRow(
                   column(width = 3,
                          numericInput("alpha1", 
                                       "$$\\alpha_1$$",
                                       value = 5,
                                       min = 1, 
                                       max = 50)
                   ),
                   column(width = 3,
                          numericInput("beta1", 
                                       "$$\\beta_1$$",
                                       value = 0.15,
                                       min = 0, 
                                       max = 5,
                                       step = 0.01)
                   ),
                   column(width = 3,
                          numericInput("theta1", 
                                       "$$\\theta_1$$",
                                       value = 30,
                                       min = -50, 
                                       max = 500)
                   ),
                   column(width = 3,
                          numericInput("sigma1", 
                                       "$$\\sigma_1$$",
                                       value = 0.25,
                                       min = 0, 
                                       max = 3,
                                       step = 0.05)
                   )
                 ),
                 
                 withMathJax(helpText("$$\\text{population 2} = \\alpha_2\\cdot e^{\\beta_2\\cdot (\\text{year} - \\text{min year}) + \\epsilon} + \\theta_2$$")),
                 
                 fluidRow(
                   column(width = 3,
                          numericInput("alpha2", 
                                       "$$\\alpha_2$$",
                                       value = 5,
                                       min = 1, 
                                       max = 50)
                   ),
                   column(width = 3,
                          numericInput("beta2", 
                                       "$$\\beta_2$$",
                                       value = 0.12,
                                       min = 0, 
                                       max = 5,
                                       step = 0.01)
                   ),
                   column(width = 3,
                          numericInput("theta2", 
                                       "$$\\theta_2$$",
                                       value = 300,
                                       min = -50, 
                                       max = 500)
                   ),
                   column(width = 3,
                          numericInput("sigma2", 
                                       "$$\\sigma_2$$",
                                       value = 0.25,
                                       min = 0, 
                                       max = 3,
                                       step = 0.05)
                   )
                 ),
                 
                 helpText(h4("Graph Aesthetics")),
                 
                 selectInput("scale2",
                             "Scale",
                             choices = c("Linear", "Log10", "Log2"),
                             selected = "Linear"),
                 
                 checkboxInput("trendline2",
                               "Trendline",
                               value = T)
                 
          )
      )
    )
  )
))
