# ----------------------------------------------------------------------------------------------------
# Load Libraries  ------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(ggplot2)
library(lubridate)
library(dplyr)
library(RSQLite)
library(here)
library(tidyverse)
# ALERT: REQUIRES VERSION 0.2.3
# url_r2d3v0.2.3 <- "https://cran.r-project.org/src/contrib/Archive/r2d3/r2d3_0.2.3.tar.gz"
# install.packages(url_r2d3v0.2.3, repos = NULL, type = 'source')
# install.packages("r2d3")
library(r2d3)
# library(purrr)
library(gridSVG)
# library(lubridate)
# library(readxl)

# ----------------------------------------------------------------------------------------------------
# Turn a list of data into a json file ---------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

data_to_json <- function(data) {
    jsonlite::toJSON(data, 
                     dataframe = "rows", 
                     auto_unbox = FALSE, 
                     rownames = TRUE)
} 

# ----------------------------------------------------------------------------------------------------
# Redefine drawr function --------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

drawr <- function(data, 
                  linear            = "true", 
                  draw_start        = NULL,
                  points_end        = NULL,
                  x_by              = 0.25,
                  free_draw         = T,
                  points            = "partial",
                  aspect_ratio      = 1.5,
                  title             = "", 
                  x_range           = NULL, 
                  y_range           = NULL,
                  x_lab             = "", 
                  y_lab             = "", 
                  drawn_line_color  = "steelblue",
                  data_tab1_color   = "steelblue", 
                  x_axis_buffer     = 0.01, 
                  y_axis_buffer     = 0.05,
                  show_finished     = T,
                  shiny_message_loc = NULL) {
    
    line_data  <- data$line_data
    point_data <- data$point_data
    
    x_min <- min(line_data$x)
    x_max <- max(line_data$x)
    y_min <- min(line_data$y)
    y_max <- max(line_data$y)
    
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
    
    r2d3::r2d3(data   = data_to_json(data), 
               script = "www/js/shinydrawr-r2d3v0.2.3.js",
               dependencies = c("d3-jetpack"),
               options = list(draw_start        = draw_start, 
                              points_end        = points_end,
                              linear            = as.character(linear),
                              free_draw         = free_draw, 
                              points            = points,
                              aspect_ratio      = aspect_ratio,
                              pin_start         = T, 
                              x_range           = x_range,
                              x_by              = x_by,
                              y_range           = y_range,
                              line_style        = NULL,
                              data_tab1_color   = data_tab1_color, 
                              drawn_line_color  = drawn_line_color,
                              show_finished     = show_finished,
                              shiny_message_loc = shiny_message_loc,
                              title             = title)
    )
    
}

# ----------------------------------------------------------------------------------------------------
# Exponential Data Simulation ------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

expDataGen <- 
    function(beta, 
             sd, 
             points_choice = "partial", 
             points_end_scale = 0.5,
             N = 30,
             aspect_ratio = 1,
             free_draw = "false",
             x_min = 0,
             x_max = 20,
             x_by  = 0.25){
        
        points_end_scale <- ifelse(points_choice == "full", 1, points_end_scale)
        
        # Set up x values
        xVals <- seq(x_min, x_max*points_end_scale, length.out = floor(N*3/4))
        xVals <- sample(xVals, N, replace = TRUE)
        xVals <- jitter(xVals)
        xVals <- ifelse(xVals < 0, 0, xVals)
        xVals <- ifelse(xVals > 20, 20, xVals)
        
        # Generate "good" errors
        repeat{
            errorVals <- rnorm(length(xVals), 0, sd)
            if(mean(errorVals[floor(N/3)]) < 2*sd & mean(errorVals[floor(N/3)] > -2*sd)){
                break
            }
        }
        
        # Simulate point data
        point_data <- tibble(x = xVals,
                             y = exp(x*beta + errorVals)) %>%
            arrange(x)
        
        # Obtain starting value for beta
        lm.fit <- lm(log(y) ~ x, data = point_data)
        beta.0 <- coef(lm.fit)[1] %>% as.numeric()
        # Use NLS to fit a better line to the data
        start <- list(beta = beta.0)
        nonlinear.fit <- nls(y ~ exp(x*beta),
                             data = point_data, 
                             start = start)
        betahat <- coef(nonlinear.fit)[1] %>% as.numeric()
        
        # Simulate best fit line data
        line_data <- tibble(x = seq(x_min, x_max, x_by), 
                            y = exp(x*betahat))
        
        data <- list(point_data = point_data, line_data = line_data)
        
        return(data)
    }


experiment_name <- "emily-log-you-draw-it-pilot-app"

# add resource paths so Shiny can see them
addResourcePath("parameter_details", "parameter_details")
# addResourcePath("trials", "trials")
addResourcePath("examples", "examples")

# define folders
parameters_folder <- "parameter_details" # subfolders for data, pdf, png, svg. picture_details.csv in this folder
# trials_folder <- "trials" # subfolders for svg. picture_details_trial.csv in this folder


window_dim_min <- 400 #c(800, 600) # width, height

con <- dbConnect(SQLite(), dbname = "you_draw_it_exp_data.db")
experiment <- dbReadTable(con, "experiment_details")
if (nrow(experiment) > 1) {
    experiment <- experiment[nrow(experiment),]
    warning("Multiple rows in the experiment_details table. Only the last row will be used.")
}
dbDisconnect(con)


shinyServer(function(input, output, session) {
# This needs to be run every connection, not just once.
    source("code/randomization.R")

    # reactive values to control the trials
    values <- reactiveValues(
        experiment = experiment$experiment,
        question = experiment$question,
        pics = NULL,
        submitted = FALSE, 
        choice = NULL,
        starttime = NULL,
        trialsreq  = experiment$trials_req,
        trialsleft = experiment$trials_req,
        ydipp   = experiment$ydi_pp,
        ydippleft = experiment$ydi_pp,
        parms = 0,
        parm_id = 1,
        result = "")

    output$debug <- renderText({experiment$question})

    # Show other text input box if other is selected
     # observe({
     #     if (length(values$reasons) == 1) {
     #         updateCheckboxInput(session, "otheronly", value = TRUE)
     #         updateTextInput(session, "other", label = "Reason")
     #     }
     # })

    # Provide experiment-appropriate reasoning boxes
     # observe({
     #     updateCheckboxGroupInput(session, "reasoning",
     #                              choices = values$reasons, selected = NA)
     # })

    # Only start experiment when consent is provided
    observeEvent(input$beginexp, {
        if (input$consent) updateCheckboxInput(session, "welcome", value = TRUE)
    })

    # Provide a message if the browser is too small
    observeEvent(input$dimension, {
        if (any(input$dimension < window_dim_min))
            showModal(
                modalDialog(
                    title = "Window Size is too small",
                    sprintf("You must view this experiment in a browser window which is at least %s x %s", window_dim_min, window_dim_min),
                    size = "s",
                    easyClose = T
                )
            )
        else {
            removeModal()
        }
    })

    # Title header
    output$welcome_header <- renderText("Welcome to a Survey on Graphical Inference")

    # ---- Introduction --------------------------------------------------------
    # Welcome text and instructions
    output$welcome_text <- renderUI({
        HTML("This web site is designed to conduct a survey on graphical inference which will help us understand human perception of graphics for use in communicating statistics.<br/><br/>
               The following examples illustrate the types of questions you may encounter during this experiment.")
    })

    # ---- Example -------------------------------------------------------------
    output$example1_q <- renderText({
        return(paste0("Example 1: ", values$question))
    })

    output$example1_plot <- renderImage({
        if (is.null(values$experiment)) return(NULL)

        list(src = file.path("examples", "example1.png"),
             contentType = 'image/png',
             style = "max-width:100%; max-height:100%"
             )
    }, deleteFile = FALSE)

    output$example2_q <- renderText({
        return(paste0("Example 2: ", values$question))
    })

    output$example2_plot <- renderImage({
        if (is.null(values$experiment)) return(NULL)

        list(src = file.path("examples", "example2.png"),
             contentType = 'image/png',
             style = "max-width:100%; max-height: 100%")
    }, deleteFile = FALSE)

    # ---- Demographic information ---------------------------------------------
    output$demo_text <- renderText({
        return("Please fill out the demographic information to begin.")
    })

    # add demographic information to the database
    observeEvent(input$submitdemo, {
        if (!is.null(input$nickname) && nchar(input$nickname) > 0 && !any(input$dimension < window_dim_min)) {
            con <- dbConnect(SQLite(), dbname = "you_draw_it_exp_data.db")

            age <- ifelse(is.null(input$age), "", input$age)
            gender <- ifelse(is.null(input$gender), "", input$gender)
            academic_study <- ifelse(is.null(input$education), "", input$education)

            demoinfo <- data.frame(nick_name = input$nickname,
                                   age = age,
                                   gender = gender,
                                   academic_study = academic_study,
                                   ip_address = "")

            dbWriteTable(con, "users", demoinfo, append = TRUE, row.names = FALSE)
            dbDisconnect(con)

            updateCheckboxInput(session, "ready", value = TRUE)
        }
    })



    # ---- Question Flow -------------------------------------------------------
    output$question <- renderText({
        return(values$question)
    })

    # Output info on how many trials/lineups left
    output$status <- renderText({
        paste(
            ifelse(values$trialsleft > 0, "Trial", ""),
            "Plot",
            ifelse(values$trialsleft > 0,
                   paste(values$trialsreq - values$trialsleft + 1, "of", values$trialsreq),
                   paste(values$ydipp - values$ydippleft + 1, "of", values$ydipp)))
    })

    # Enable submit button if the experiment progresses to ___ stage
    observe({
        # if (is.null(input$response_no) || input$response_no == "") {
            enable("submit")
        # }
    })

    observeEvent(input$submit, {
        response <- as.character(input$response_no)

        if (
            # nchar(response) > 0 &&
            # all(strsplit(response, ",")[[1]] %in% 1:20) &&
            values$ydippleft > 0 &&
            # (length(input$reasoning) > 0 || (nchar(input$other) > 0)) &&
            # nchar(input$certain) > 0 &&
            !any(input$dimension < window_dim_min)) {

            # Things to do when responses are all filled in and submitted
            # disable("submit")

            # reason <- input$reasoning
            # if ("Other" %in% reason || input$otheronly) {
            #     reason <- c(reason, input$other)
            # }
            # reason <- paste(reason, collapse = ", ")

            # values$choice <- response

            if (values$trialsleft == 0 && values$ydippleft > 0) {
                # This applies to the lineups, not to the trials
                values$result <- "Submitted!"

                test <- data.frame(ip_address = input$ipid, 
                                   nick_name = input$nickname,
                                   start_time = values$starttime, 
                                   end_time = now(),
                                   parm_id = values$parm_id,
                                   x = 0,
                                   y = 0,
                                   ydrawn = 0
                                   # response_no = values$choice,
                                   # conf_level = input$certain,
                                   # choice_reason = reason,
                                   # description = values$experiment
                                   )

                # Write results to database
                con <- dbConnect(SQLite(), dbname = "you_draw_it_exp_data.db")
                dbWriteTable(con, "feedback", test, append = TRUE, row.names = FALSE)
                dbDisconnect(con)

                # Update variables for next trial
                values$ydippleft <- values$ydippleft - 1
                values$choice <- ""

                # Generate completion code
                if (values$ydippleft == 0) {
                    # rand1 <- sample(letters, 3, replace = TRUE)
                    # rand2 <- sample(LETTERS, 3, replace = TRUE)
                    # rand3 <- sample(1:9, 3, replace = TRUE)
                    #
                    # code <- paste(sample(c(rand1, rand2, rand3)), collapse = "")

                    values$question <- "All done! Congratulations!"
                    #values$question <- paste("All done! Congratulations! Please click the URL to complete the study:")
                    updateCheckboxInput(session, "done", value = TRUE)
                }

            } else {
                # This applies to the trials, not the lineups
                if (any(strsplit(values$choice, ",")[[1]] %in% values$correct)) {
                    values$trialsleft <- values$trialsleft - 1
                    values$result <- "Correct! :)"
                } else {
                    values$result <- "Incorrect :("
                }
            }

            values$submitted <- TRUE
        } else {
            # Don't let them move ahead without doing the trial
            showNotification("Please fill in all of the boxes.")
        }
    })

    message_loc <- session$ns("drawr_message")
    drawn_data <- shiny::reactiveVal()
    # This renders the you draw it graph
    output$shinydrawr <- r2d3::renderD3({
        if (values$ydippleft == 0 || !input$ready || any(input$dimension < window_dim_min)) return(NULL)

        withProgress(
            # Message: Loading (trial) plot i of n
            message = paste(values$result, "Loading",
                            ifelse(values$trialsleft > 0, "trial", ""), "plot",
                            ifelse(values$trialsleft > 0,
                                   paste(values$trialsreq - values$trialsleft + 1, "of", values$trialsreq),
                                   paste(values$ydipp - values$ydippleft + 1, "of", values$ydipp))),
            expr = {
            values$submitted

            values$starttime <- now()
            trial <- as.numeric(values$trialsleft > 0)

            # plotpath <- ifelse(values$trialsleft > 0, "trials", "plots")

            con <- dbConnect(SQLite(), dbname = "you_draw_it_exp_data.db")
            # dbListTables(db_con)
            exp_parameter_details <- dbReadTable(con,"exp_parameter_details")

            # I suspect this logic could be improved with dbplyr...
            # if (trial == 0 && is.null(values$parms)) {
            #     # Create order of trials
            #     orderby <- paste0("ORDER BY CASE parm_id ",
            #                       paste("WHEN", parm_ids, "THEN", 0:(values$ydipp - 1), collapse = " "),
            #                       " END")
            #     # Get picture details
            #     values$pics <- dbGetQuery(
            #         con, paste0("SELECT * FROM picture_details WHERE experiment = '", values$experiment,
            #                     "' AND trial = ", trial, " AND parm_id IN (", paste(parm_ids, collapse = ","), ") ",
            #                     orderby))
            # 
            #     nextplot <- values$parms[1,]
            # } else if (trial == 0 && !is.null(values$parms)) {
                # nextplot <- values$parms[values$ydipp - values$ydippleft + 1,]
            # } else if (trial == 1 && is.null(values$trial_pics)) {
            #     # Get trial pictures
            #     orderby <- paste0("ORDER BY CASE parm_id ",
            #                       paste("WHEN", trial_parm_ids, "THEN", 0:(length(trial_parm_ids) - 1), collapse = " "),
            #                       " END")
            #     values$trial_pics <- dbGetQuery(
            #         con, paste0("SELECT * FROM picture_details WHERE experiment = '", values$experiment,
            #                     "' AND trial = ", trial, " AND parm_id IN (", paste(trial_parm_ids, collapse = ","), ") ",
            #                     orderby))
            #     nextplot <- values$trial_pics[1,]
            # } else if (trial == 1 && !is.null(values$trial_pics)) {
            #     nextplot <- values$trial_pics[values$trialsreq - values$trialsleft + 1,]
            # }

            dbDisconnect(con)

            # Update reactive values
            values$parm_id <- parm_ids[values$ydipp - values$ydippleft + 1]
            # values$correct <- strsplit(as.character(nextplot$obs_plot_location), ",")[[1]]

            # Reset UI selections
            values$submitted <- FALSE

            # updateSelectizeInput(
            #     session, "certain",
            #     choices = c("", "Very Uncertain", "Uncertain", "Neutral", "Certain", "Very Certain"),
            #     selected = NULL)
            # updateTextInput(session, "response_no", value = "")
            # updateTextInput(session, "other", value = "")
            # updateCheckboxGroupInput(session, "reasoning", selected = NA)

            # if (is.null(nextplot$pic_name)) return(NULL)

            # Read svg and remove width/height
            # tmp <- readLines(file.path(plotpath, "svg", basename(nextplot$pic_name)))
            # tmp[2] <- str_replace(tmp[2], "width=.*? height=.*? viewBox", "viewBox")

            # Include the picture
            # div(
            #     class="full-lineup-container",
            #     HTML(tmp)
            # )
            
            parms   <- exp_parameters_details[values$parm_id,]
            
            # Generate Data
            data <- expDataGen(beta  = parms$beta, 
                       sd    = parms$sd, 
                       points_choice    = parms$points_choice, 
                       points_end_scale = parms$points_end_scale,
                       N     = parms$N, 
                       x_min = parms$x_min, 
                       x_max = parms$x_max, 
                       x_by  = parms$x_by)
            
            y_range <- range(data$line_data[,"y"]) * c(parms$ymin_scale, parms$ymax_scale)
            x_range <- range(data$line_data[,"x"])
            
            # Include the you draw it graph
            drawr(data              = data,
                  aspect_ratio      = parms$aspect_ratio,
                  linear            = parms$linear,
                  free_draw         = parms$free_draw,
                  points            = parms$points_choice,
                  x_by              = parms$x_by,
                  draw_start        = parms$x_max*parms$draw_start_scale,
                  points_end        = parms$x_max*parms$points_end_scale,
                  show_finished     = input$show_finished,
                  shiny_message_loc = message_loc,
                  x_range           = x_range,
                  y_range           = y_range)

            }) # end WithProgress
    }) # end renderD3
    
    shiny::observeEvent(input$drawr_message, {
        
            tibble(drawn = input$drawr_message) %>%
                drawn_data()
        
    })
    
    output$drawrmessage <- DT::renderDataTable({
        drawn_data()
    })
    
}) # End app definition
