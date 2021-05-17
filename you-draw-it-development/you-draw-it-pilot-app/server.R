# ----------------------------------------------------------------------------------------------------
# Load Libraries  ------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(ggplot2)
library(lubridate)
library(dplyr)
library(RSQLite)
library(DBI)
sqlite.driver <- dbDriver("SQLite")
library(here)
library(tidyverse)
library(purrr)
# Make sure to use d3_version = 5, version of r2d3 doesn't matter....
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
                  linear           = "true",
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
               script = "www/js/shinydrawr-d3v5.js",
               dependencies = c("d3-jetpack"),
               d3_version = "5",
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
# Set up Experiment -----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

experiment_name <- "emily-log-you-draw-it-pilot-app"

# add resource paths so Shiny can see them
# addResourcePath("parameter_details", "parameter_details")
# addResourcePath("trials", "trials")
addResourcePath("examples", "examples")

# define folders
# parameters_folder <- "parameter_details" # subfolders for data, pdf, png, svg. picture_details.csv in this folder
# trials_folder <- "trials" # subfolders for svg. picture_details_trial.csv in this folder


window_dim_min <- c(800, 600) # width, height

con <- dbConnect(sqlite.driver, dbname = "you_draw_it_data.db")
experiment <- dbReadTable(con, "experiment_details")
if (nrow(experiment) > 1) {
    experiment <- experiment[nrow(experiment),]
    warning("Multiple rows in the experiment_details table. Only the last row will be used.")
}
# exp_parameter_details <- dbReadTable(con, "exp_parameter_details")
dbDisconnect(con)


shinyServer(function(input, output, session) {
# This needs to be run every connection, not just once.
    study_starttime = now()
    source("code/data-generation.R")

    # reactive values to control the trials
    values <- reactiveValues(
        experiment = experiment$experiment,
        question = experiment$question,
        practicetext = "",
        practicegif_file = "",
        pics = NULL,
        submitted = FALSE,
        done_drawing = FALSE,
        choice = NULL,
        starttime = NULL,
        practicereq  = nrow(practice_data),
        practiceleft = nrow(practice_data),
        ydipp   = experiment$ydi_pp,
        ydippleft = experiment$ydi_pp,
        parms = 0,
        taskNum = NA,
        linear  = NULL,
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
               The following examples illustrate the types of questions you may encounter during this experiment.<br/>
             <b>If the gifs do not show up, refresh the page and they should appear.</b>")
    })

    # ---- Example -------------------------------------------------------------
    output$example1_q <- renderText({
        return(paste0("Example 1: ", values$question))
    })


    output$example2_q <- renderText({
        return(paste0("Example 2: ", values$question))
    })

    # ---- Demographic information ---------------------------------------------
    output$demo_text <- renderText({
        return("Please fill out the demographic information to begin.")
    })

    # add demographic information to the database
    observeEvent(input$submitdemo, {
        if (!is.null(input$nickname) && nchar(input$nickname) > 0 && !any(input$dimension < window_dim_min)) {
            con <- dbConnect(sqlite.driver, dbname = "you_draw_it_data.db")

            age <- ifelse(is.null(input$age), "", input$age)
            gender <- ifelse(is.null(input$gender), "", input$gender)
            academic_study <- ifelse(is.null(input$education), "", input$education)
            recruitment <- ifelse(is.null(input$recruitment), "", input$recruitment)

            demoinfo <- data.frame(nick_name = input$nickname,
                                   study_starttime = study_starttime,
                                   age = age,
                                   gender = gender,
                                   academic_study = academic_study,
                                   recruitment = recruitment,
                                   ip_address = ""
                                   )

            dbWriteTable(con, "users", demoinfo, append = TRUE, row.names = FALSE)

            simulated_data_db <- simulated_data %>%
                unnest(data) %>%
                dplyr::select(parm_id, dataset, x, y) %>%
                mutate(ip_address = input$ipid,
                       nick_name = input$nickname,
                       study_starttime = study_starttime,
                       parm_id = as.character(parm_id)
                )

            dbWriteTable(con, "simulated_data", simulated_data_db, append = TRUE, row.names = FALSE)

            dbDisconnect(con)

            updateCheckboxInput(session, "ready", value = TRUE)
        }
    })



    # ---- Question Flow -------------------------------------------------------
    output$question <- renderText({
        return(values$question)
    })

    output$isPractice <- reactive({
        values$practiceleft > 0
    })
    outputOptions(output, 'isPractice', suspendWhenHidden = FALSE)

    output$practicetext <- renderText({
        return(values$practicetext)
    })

    output$practicegif <- renderImage({
        # Return a list
        list(src = values$practicegif_file,
             alt = "",
             width = 350)
    }, deleteFile = FALSE)

    # Output info on how many practices/lineups left
    output$status <- renderText({
        paste(
            ifelse(values$practiceleft > 0, "Practice", ""),
            "Plot",
            ifelse(values$practiceleft > 0,
                   paste(values$practicereq - values$practiceleft + 1, "of", values$practicereq),
                   paste(values$ydipp - values$ydippleft + 1, "of", values$ydipp)))
    })

    # Enable submit button if the experiment progresses to ___ stage
    observe({
        if (!(values$done_drawing)) {
            enable("submit")
        }
    })

    observeEvent(input$submit, {
        # response <- as.character(input$response_no)

        if (
            # nchar(response) > 0 &&
            # all(strsplit(response, ",")[[1]] %in% 1:20) &&
            values$ydippleft > 0 &&
            # (length(input$reasoning) > 0 || (nchar(input$other) > 0)) &&
            values$done_drawing &&
            !any(input$dimension < window_dim_min)) {

            # Things to do when you draw it line is finished and submitted
            disable("submit")

            if (values$practiceleft == 0 && values$ydippleft > 0) {
                # This applies to the you draw it plots, not to the practices
                values$result <- "Submitted!"

                test <- drawn_data() %>%
                            mutate(ip_address = input$ipid,
                                   nick_name = input$nickname,
                                   study_starttime = study_starttime,
                                   start_time = values$starttime,
                                   end_time = now(),
                                   parm_id = as.character(parm_id)
                                   )

                # Write results to database
                con <- dbConnect(sqlite.driver, dbname = "you_draw_it_data.db")
                dbWriteTable(con, "feedback", test, append = TRUE, row.names = FALSE)
                dbDisconnect(con)

                # Update variables for next trial
                values$ydippleft <- values$ydippleft - 1
                values$choice <- ""

                # Generate completion code
                if (values$ydippleft == 0) {

                    values$question <- "All done! Congratulations!"
                    #values$question <- paste("All done! Congratulations! Please click the URL to complete the study:")
                    updateCheckboxInput(session, "done", value = TRUE)
                }

            } else {
                # This applies to the practices, not the you draw it's
                values$practiceleft <- values$practiceleft - 1
            }

            values$submitted <- TRUE
        } else {
            # Don't let them move ahead without doing the trial
            showNotification("Please finish drawing the trend for the entire yellow box region.")
        }
    })

    # Create storage for response values
    message_loc <- session$ns("drawr_message")
    drawn_data <- shiny::reactiveVal()
    line_data_storage <- shiny::reactiveVal()
    done_drawing  <- shiny::reactiveVal()

    # This renders the you draw it graph
    output$shinydrawr <- r2d3::renderD3({
        if (values$ydippleft == 0 || !input$ready || any(input$dimension < window_dim_min)) return(NULL)
        input$reset

        withProgress(
            # Message: Loading (trial) plot i of n
            message = paste(values$result, "Loading",
                            ifelse(values$practiceleft > 0, "trial", ""), "plot",
                            ifelse(values$practiceleft > 0,
                                   paste(values$practicereq - values$practiceleft + 1, "of", values$practicereq),
                                   paste(values$ydipp - values$ydippleft + 1, "of", values$ydipp))),
            expr = {
            values$submitted

            values$starttime <- now()
            trial <- as.numeric(values$practiceleft > 0)

            # Reset UI selections
            values$submitted    <- FALSE
            values$done_drawing <- FALSE

            # This part applies to only the practice rounds
            if(values$practiceleft > 0){
                # Update reactive values
                practiceID <- (values$practicereq - values$practiceleft + 1)

                # Obtain Parameters & Data
                values$practicetext  <- practice_text[practiceID] %>% as.character()
                values$practicegif_file  <- practicegif_files[practiceID] %>% as.character()
                isLinear     <- practice_data[practiceID, "linear"] %>% as.character()
                isFreeDraw   <- practice_data[practiceID, "free_draw"] %>% as.logical()
                drawStart    <- practice_data[practiceID, "draw_start"] %>% as.numeric()
                showFinished <- practice_data[practiceID, "show_finished"] %>% as.logical()

                point_data <- practice_data[practiceID,] %>%
                    unnest(data) %>%
                    filter(dataset == "point_data")

                line_data <- practice_data[practiceID,] %>%
                    unnest(data) %>%
                    filter(dataset == "line_data")

                data <- list(point_data = point_data, line_data = line_data)

                    # Set up ranges
                    y_range <- c(min(data$point_data[,"y"]), max(max(data$point_data[,"y"]), max(data$line_data[,"y"]))) * c(0.5, 1.5)
                    x_range <- c(0,20)

                # Include the you draw it graph
                drawr(data              = data,
                      aspect_ratio      = 1,
                      linear            = isLinear,
                      free_draw         = isFreeDraw,
                      points            = "full",
                      x_by              = 0.25,
                      draw_start        = drawStart,
                      # points_end        = 0.5,
                      show_finished     = showFinished,
                      shiny_message_loc = message_loc,
                      x_range           = x_range,
                      y_range           = y_range)

            } else {
            # This part applies to only the you draw it's, not the practice rounds
                # Update reactive values
                taskID <- (values$ydipp - values$ydippleft + 1)

                # Obtain Parameters & Data
                isLinear   <- simulated_data[taskID, "linear"] %>% as.character()
                isFreeDraw <- simulated_data[taskID, "free_draw"] %>% as.logical()
                drawStart  <- simulated_data[taskID, "draw_start"] %>% as.numeric()

                point_data <- simulated_data[taskID,] %>%
                    unnest(data) %>%
                    filter(dataset == "point_data")

                line_data <- simulated_data[taskID,] %>%
                    unnest(data) %>%
                    filter(dataset == "line_data")

                data <- list(point_data = point_data, line_data = line_data)

                if(isFreeDraw){

                    # Store data for feedback later
                    line_data %>%
                        select(parm_id, linear, x, y) %>%
                        line_data_storage()

                    # Set up ranges
                    y_range <- eyefitting_yrange * c(1.1, 1.1)
                    x_range <- c(0,20)

                } else {

                    # Store data for feedback later
                    line_data %>%
                        select(parm_id, linear, x, y) %>%
                        filter(x >= drawStart) %>%
                        line_data_storage()

                    # Set up ranges
                    y_range <- range(data$line_data[,"y"]) * c(0.5, 2)
                    x_range <- range(data$line_data[,"x"])
                }

                # Include the you draw it graph
                drawr(data              = data,
                      aspect_ratio      = 1,
                      linear            = isLinear,
                      free_draw         = isFreeDraw,
                      points            = "full",
                      x_by              = 0.25,
                      draw_start        = drawStart,
                      # points_end        = 0.5,
                      show_finished     = FALSE,
                      shiny_message_loc = message_loc,
                      x_range           = x_range,
                      y_range           = y_range)
            }
        })
    }) # end renderD3

    shiny::observeEvent(input$drawr_message, {

        values$done_drawing <- TRUE

        if(values$practiceleft == 0){
            line_data <- line_data_storage()

            line_data %>%
            mutate(ydrawn = input$drawr_message) %>%
                select(parm_id, x, y, ydrawn, linear) %>%
                drawn_data()
        }

    })

}) # End app definition
