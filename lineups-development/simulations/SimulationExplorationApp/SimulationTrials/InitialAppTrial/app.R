library(shiny)
library(shinyjs)
library(tidyverse)
library(gridExtra)
library(simglm)

# Modify function to accomodate exponential as an outcome type.
transform_outcome2 <- function (outcome, type, ...) 
{
    if (type %in% c("logistic", "binary")) {
        probability <- exp(outcome)/(1 + exp(outcome))
        rbinom(length(outcome), size = 1, prob = probability)
    } 
    else {
        if (type %in% c("exp", "exponential")) {
            rexp(length(outcome), rate = 1/exp(outcome))
        }
        else {
            if (type %in% c("count", "poisson")) {
                rpois(length(outcome),  lambda = exp(outcome))
            }
            else {
                purrr::map()
            }
        }
    }
}

generate_response2 <- function (data, sim_args, keep_intermediate = TRUE, ...) {
    outcome_name <- parse_formula(sim_args)[["outcome"]]
    outcome_type <- sim_args[["outcome_type"]]
    fixed_formula <- parse_formula(sim_args)[["fixed"]]
    fixed_vars <- attr(terms(fixed_formula), "term.labels")
    if (any(grepl("^factor\\(", fixed_vars))) {
        fixed_vars <- gsub("factor\\(|\\)$", "",
                           fixed_vars)
    }
    if (any(grepl("^ns\\(", fixed_vars))) {
        fixed_vars <- gsub("ns\\(|\\,.+\\)$", "",
                           fixed_vars)
    }
    if (any(grepl("^poly\\(", fixed_vars))) {
        fixed_vars <- gsub("poly\\(|\\,.+\\)", "",
                           fixed_vars)
    }
    if (any(grepl(":", fixed_vars))) {
        fixed_vars <- gsub(":", "\\.", fixed_vars)
    }
    if (any(grepl("^ns|^poly", attr(terms(fixed_formula),
                                    "term.labels")))) {
        fixed_vars <- poly_ns_names(sim_args)
    }
    if (any(unlist(lapply(seq_along(sim_args[["fixed"]]),
                          function(xx) sim_args[["fixed"]][[xx]]$var_type)) ==
            "factor")) {
        num_levels <- lapply(seq_along(sim_args[["fixed"]]),
                             function(xx) sim_args[["fixed"]][[xx]][["levels"]])
        num_levels <- purrr::modify_if(num_levels, is.character,
                                       length)
        if (any(unlist(lapply(seq_along(sim_args[["fixed"]]),
                              function(xx) num_levels[[xx]] > 2 & sim_args[["fixed"]][[xx]][["var_type"]] ==
                              "factor")))) {
            fixed_vars <- factor_names(sim_args, fixed_vars)
        }
    }
    Xmat <- dplyr::select(data, fixed_vars)
    if (any(grepl("Intercept", names(data)))) {
        Xmat <- cbind(data["X.Intercept."], Xmat)
    }
    fixed_outcome <- as.matrix(Xmat) %*% sim_args[["reg_weights"]]
    if (length(parse_formula(sim_args)[["randomeffect"]]) !=
        0) {
        random_formula <- parse_formula(sim_args)[["randomeffect"]]
        random_formula_parsed <- parse_randomeffect(random_formula)
        random_effects_names <- names(sim_args[["randomeffect"]])
        random_formula <- lapply(seq_along(random_formula_parsed[["random_effects"]]),
                                 function(xx) as.formula(random_formula_parsed[["random_effects"]][xx]))
        Zmat <- lapply(random_formula, model.matrix, data = data) %>%
            lapply(., data.frame) %>% dplyr::bind_cols()
        rand_effects <- dplyr::select(data, random_effects_names)
        random_effects <- rowSums(rand_effects * Zmat)
    }
    else {
        random_effects <- NULL
        random_effects <- 0
    }
    if (keep_intermediate) {
        response_outcomes <- data.frame(fixed_outcome = fixed_outcome,
                                        random_effects = random_effects)
        data <- cbind(data, response_outcomes, row.names = NULL)
    }
    if (is.null(data[["error"]])) {
        data["error"] <- 0
    }
    outcome <- as.numeric(unlist(fixed_outcome + random_effects +
                                     data["error"]))
    if (!is.null(sim_args[["outcome_type"]])) {
        trans_outcome <- transform_outcome2(outcome, type = sim_args[["outcome_type"]])
        data <- cbind(data, untransformed_outcome = outcome)
        data[outcome_name] <- trans_outcome
    }
    else {
        data[outcome_name] <- outcome
    }
    data
}

generateDat_expSimglm <- function(n = 70, errVar = 25, xMin = 0, xMax = 50, regWeights = c(10, 0.1)){
    sim_arguments <- list(
        formula = y ~ 1 + x1,
        error = list(variance = errVar),
        fixed = list(x1 = list(var_type = 'ordinal', levels = xMin:xMax)),
        sample_size = n,
        reg_weights = regWeights,
        outcome_type = "exp"
    )
    
    return(simulate_fixed(data = NULL, sim_arguments) %>% 
               simulate_error(sim_arguments) %>%
               generate_response2(sim_arguments)
    )
}

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Simulation Exploration",
    
    # ---- Tab 1 ---------------------------------------------------------------
    tabPanel(
        title = "Manual Simulation",
        fluidRow(
            column(
                width = 3,
                selectInput("functionalForm_tab1", "Function Form:", c("Exponential")),
                selectInput("errorType_tab1", "Error Type:", c("Additive", "Multiplicative")),
                sliderInput("xRange_tab1", "x-Axis Range", min = 0, max = 200, value = c(0, 30)),
                sliderInput("n_tab1", "Sample Size (n):", min = 5, max = 200, value = 30),
                numericInput("coef_tab1", "Growth Coefficient:", value = 0.1, min = 0, max = 5, step = 0.05),
                numericInput("muErr_tab1", "Error Mean:", value = 0, min = -5, max = 20, step = 1),
                numericInput("sdErr_tab1", "Error StdDev:", value = 0.25, min = 0, max = 10, step = 0.05)
            ),
            column(
                width = 8,
                plotOutput("Plot_Manual", height = "600px")
            )
        )
    ),
    
    # ---- Tab 2 ---------------------------------------------------------------
    
    tabPanel(
        title = "library(simglm) Simulation",
        fluidRow(
            column(
                width = 3,
                selectInput("functionalForm_tab2", "Function Form:", c("Exponential", "Quadratic")),
                sliderInput("xRange_tab2", "x-Axis Range", min = 0, max = 200, value = c(0, 30)),
                sliderInput("n_tab2", "Sample Size (n):", min = 5, max = 200, value = 30),
                numericInput("varErr_tab2", "Error StdDev:", value = 0.25, min = 0, max = 1000, step = 0.05),
                helpText("Enter Regression Weights"),
                helpText("Exponential: (intercept, x)"),
                helpText("Quadratic: (intercept, x, x^2)"),
                selectizeInput("regWeights_tab2", "Enter a vector of coefficients", choices = NULL, multiple = TRUE, options = list(create = TRUE))
            ),
            column(
                width = 8,
                plotOutput("Plot_simglm", height = "600px")
            )
        )
    )
    ,
    
    # ---- Tab 3 ---------------------------------------------------------------
    
    tabPanel(
        title = "'Stroup' Simulation",
        fluidRow(
            column(
                width = 3,
                selectInput("functionalForm_tab3", "Function Form:", c("Exponential")),
                sliderInput("xRange_tab3", "x-Axis Range", min = 0, max = 200, value = c(0, 30)),
                sliderInput("n_tab3", "Sample Size (n):", min = 5, max = 200, value = 30),
                numericInput("coef_tab3", "Growth Coefficient:", value = 0.5, min = 0, max = 5, step = 0.05),
                numericInput("muErr_tab3", "Error Mean:", value = 0, min = 0, max = 20, step = 0.05),
                numericInput("sdErr_tab3", "Error StdDev:", value = 0.25, min = 0, max = 10, step = 0.05)
            ),
            column(
                width = 8,
                plotOutput("Plot_Stroup", height = "600px")
            )
        )
    )
    # ---- End UI --------------------------------------------------------------
)

# Define server
server <- function(input, output, session) {
    
    # ---- Tab 1 ---------------------------------------------------------------
    
    generateDat_Manual <- 
        function(n, coef, muErr, sdErr, xRange, errorType){
            
            x <- runif(n, xRange[1], xRange[2])
            yRaw   <- exp(coef*x)
            yError <-  rnorm(n, muErr, sdErr)
            if(errorType %in% c("Additive")){
                y      <- yRaw + yError
            } else {
                if(errorType %in% c("Multiplicative")){
                    y      <- yRaw*yError 
                }
            }
            
            return(cbind(x, y) %>% as.data.frame())
        }
    
    data_tab1 <- reactive({
        generateDat_Manual(n = input$n_tab1, coef = input$coef_tab1, muErr = input$muErr_tab1, sdErr = input$sdErr_tab1, xRange = input$xRange_tab1, errorType = input$errorType_tab1)
        
    })
    
    output$Plot_Manual <- renderPlot({
        df <- data_tab1()
        
        linearPlot_Manual <- df %>%
            ggplot(aes(x = x, y = y)) +
            geom_point() +
            # geom_smooth(se = F, color = "black", method = loess) + 
            theme_bw() +
            ggtitle(paste("Linear ", input$functionalForm_tab1, ": Manual", sep = ""))  
        
        logPlot_Manual    <- df %>%
            ggplot(aes(x = x, y = y)) +
            geom_point() +
            # geom_smooth(se = F, color = "black", method = loess) +
            theme_bw() + 
            scale_y_log10() +
            ggtitle(paste("Log ", input$functionalForm_tab1, ": Manual", sep = ""))
        
        grid.arrange(linearPlot_Manual, logPlot_Manual, ncol=2)
    })
    
    # ---- Tab 2 ---------------------------------------------------------------
    
    generateDat_simglm <- 
        function(n, varErr, xRange, regWeights, functionalForm){
            
            if(functionalForm %in% c("Quadratic")){
                sim_arguments <- list(
                    formula = y ~ 1 + x1,
                    error = list(variance = varErr),
                    fixed = list(x1 = list(var_type = 'ordinal', levels = xRange[1]:xRange[2])),
                    sample_size = n
                )
                
                sim_arguments2 <- list(
                    formula = y ~ 1 + x1 + x2,
                    sample_size = n,
                    reg_weights = regWeights
                )
                
                return(simulate_fixed(data = NULL, sim_arguments) %>% 
                           mutate(x2 = x1*x1) %>%
                           simulate_error(sim_arguments) %>%
                           generate_response(sim_arguments2)
                )
                
            } else {
                if(functionalForm %in% "Exponential"){
                    sim_arguments <- list(
                        formula = y ~ 1 + x1,
                        error = list(variance = varErr),
                        fixed = list(x1 = list(var_type = 'ordinal', levels = xRange[1]:xRange[2])),
                        sample_size = n,
                        reg_weights = regWeights,
                        outcome_type = "exp"
                    )
                    
                    return(simulate_fixed(data = NULL, sim_arguments) %>% 
                               simulate_error(sim_arguments) %>%
                               generate_response2(sim_arguments)
                    )
                }
            }
            
        }
    
    data_tab2 <- reactive({
        generateDat_simglm(functionalForm = input$functionalForm_tab2, n = input$n_tab2, regWeights = as.numeric(input$regWeights_tab2), varErr = input$varErr_tab2, xRange = input$xRange_tab2)
        
    })
    
    output$Plot_simglm <- renderPlot({
        df <- data_tab2()
        
        linearPlot_simglm <- df %>%
            ggplot(aes(x = x1, y = y)) +
            geom_point() +
            # geom_smooth(se = F, color = "black", method = loess) + 
            theme_bw() +
            ggtitle(paste("Linear ", input$functionalForm_tab2, ": simglm", sep = "")) +
            scale_x_continuous("x")
        
        logPlot_simglm    <- df %>%
            ggplot(aes(x = x1, y = y)) +
            geom_point() +
            # geom_smooth(se = F, color = "black", method = loess) +
            theme_bw() + 
            scale_y_log10() +
            ggtitle(paste("Log ", input$functionalForm_tab2, ": simglm", sep = "")) +
            scale_x_continuous("x")
        
        grid.arrange(linearPlot_simglm, logPlot_simglm, ncol=2)
    })
    
    # ---- Tab 3 ---------------------------------------------------------------
    
    generateDat_Stroup <- 
        function(n, muErr, sdErr, coef, xRange){
            x <- runif(n, xRange[1], xRange[2])
            e_i <- rnorm(n, muErr, sdErr)
            eta <- x*coef + e_i
            y   <- rexp(length(eta), rate = 1/exp(eta)) 
            return(cbind(x, y) %>% as.data.frame())
        }
    
    data_tab3 <- reactive({
        generateDat_Stroup(n = input$n_tab3, coef = input$coef_tab3, sdErr = input$sdErr_tab3, muErr = input$muErr_tab3, xRange = input$xRange_tab3)
        
    })
    
    output$Plot_Stroup <- renderPlot({
        df <- data_tab3()
        
        linearPlot_Stroup <- df %>%
            ggplot(aes(x = x, y = y)) +
            geom_point() +
            # geom_smooth(se = F, color = "black", method = loess) + 
            theme_bw() +
            ggtitle(paste("Linear ", input$functionalForm_tab2, ": Stroup", sep = ""))
        
        logPlot_Stroup    <- df %>%
            ggplot(aes(x = x, y = y)) +
            geom_point() +
            # geom_smooth(se = F, color = "black", method = loess) +
            theme_bw() + 
            scale_y_log10() +
            ggtitle(paste("Log ", input$functionalForm_tab2, ": Stroup", sep = ""))
        
        grid.arrange(linearPlot_Stroup, logPlot_Stroup, ncol=2)
    })
    
    # ---- End Server --------------------------------------------------------------
}

# Run the application
shinyApp(ui = ui, server = server)
