library(shiny)
library(shinyjs)
library(ggplot2)
library(lubridate)
library(dplyr)
library(RSQLite)
library(here)
library(tidyverse)

experiment_name <- "emily-log-1"

# add resource paths so Shiny can see them
addResourcePath("plots", "plots")
addResourcePath("trials", "trials")
addResourcePath("examples", "examples")

# define folders
plots_folder <- "plots" # subfolders for data, pdf, png, svg. picture_details.csv in this folder
trials_folder <- "trials" # subfolders for svg. picture_details_trial.csv in this folder


window_dim_min <- 400 #c(800, 600) # width, height

con <- dbConnect(SQLite(), dbname = "exp_data.db")
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
        submitted = FALSE, choice = NULL,
        reasons = strsplit(experiment$reasons, ",")[[1]],
        starttime = NULL,
        trialsreq = experiment$trials_req,
        trialsleft = experiment$trials_req,
        lpp = experiment$lpp,
        lppleft = experiment$lpp,
        pic_id = 0, choice = NULL,
        correct = NULL, result = "")

    output$debug <- renderText({experiment$question})

    # Show other text input box if other is selected
    observe({
        if (length(values$reasons) == 1) {
            updateCheckboxInput(session, "otheronly", value = TRUE)
            updateTextInput(session, "other", label = "Reason")
        }
    })

    # Provide experiment-appropriate reasoning boxes
    observe({
        updateCheckboxGroupInput(session, "reasoning",
                                 choices = values$reasons, selected = NA)
    })

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
             style = "max-width:100%; max-height = 100%")
    }, deleteFile = FALSE)

    output$example1_a <- renderUI({
        return(HTML(paste0("
            Your choice: <b>Plot 4</b><br/>
            Reasoning: <b>", values$reasons[1], "</b><br/>
            How certain are you: <b>Very Certain</b><br/>
        ")))
    })

    output$example2_q <- renderText({
        return(paste0("Example 2: ", values$question))
    })

    output$example2_plot <- renderImage({
        if (is.null(values$experiment)) return(NULL)

        list(src = file.path("examples", "example2.png"),
             contentType = 'image/png',
             style = "max-width:100%; max-height = 100%")
    }, deleteFile = FALSE)

    output$example2_a <- renderUI({
        return(HTML(paste0("
                    Your choice: <b>Plot 1</b><br/>
                    Reasoning: <b>", values$reasons[2], "</b><br/>
                    How certain are you: <b>Certain</b><br/>
                    ")))
    })

    # ---- Demographic information ---------------------------------------------
    output$demo_text <- renderText({
        return("Please fill out the demographic information to begin.")
    })

    # add demographic information to the database
    observeEvent(input$submitdemo, {
        if (!is.null(input$nickname) && nchar(input$nickname) > 0 && !any(input$dimension < window_dim_min)) {
            con <- dbConnect(SQLite(), dbname = "exp_data.db")

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
                   paste(values$lpp - values$lppleft + 1, "of", values$lpp)))
    })

    # Enable submit button if the experiment progresses to ___ stage
    observe({
        if (is.null(input$response_no) || input$response_no == "") {
            enable("submit")
        }
    })

    observeEvent(input$submit, {
        response <- as.character(input$response_no)


        if (nchar(response) > 0 &&
            all(strsplit(response, ",")[[1]] %in% 1:20) &&
            values$lppleft > 0 &&
            (length(input$reasoning) > 0 || (nchar(input$other) > 0)) &&
            nchar(input$certain) > 0 &&
            !any(input$dimension < window_dim_min)) {

            # Things to do when responses are all filled in and submitted
            disable("submit")

            reason <- input$reasoning
            if ("Other" %in% reason || input$otheronly) {
                reason <- c(reason, input$other)
            }
            reason <- paste(reason, collapse = ", ")

            values$choice <- response

            if (values$trialsleft == 0 && values$lppleft > 0) {
                # This applies to the lineups, not to the trials
                values$result <- "Submitted!"

                test <- data.frame(ip_address = input$ipid, nick_name = input$nickname,
                                   start_time = values$starttime, end_time = now(),
                                   pic_id = values$pic_id,
                                   response_no = values$choice,
                                   conf_level = input$certain,
                                   choice_reason = reason,
                                   description = values$experiment)

                # Write results to database
                con <- dbConnect(SQLite(), dbname = "exp_data.db")
                dbWriteTable(con, "feedback", test, append = TRUE, row.names = FALSE)
                dbDisconnect(con)

                # Update variables for next trial
                values$lppleft <- values$lppleft - 1
                values$choice <- ""

                # Generate completion code
                if (values$lppleft == 0) {
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

    # This renders the trial/lineup image
    output$lineup <- renderUI({
        if (values$lppleft == 0 || !input$ready || any(input$dimension < window_dim_min)) return(NULL)

        withProgress(
            # Message: Loading (trial) plot i of n
            message = paste(values$result, "Loading",
                            ifelse(values$trialsleft > 0, "trial", ""), "plot",
                            ifelse(values$trialsleft > 0,
                                   paste(values$trialsreq - values$trialsleft + 1, "of", values$trialsreq),
                                   paste(values$lpp - values$lppleft + 1, "of", values$lpp))),
            expr = {
            values$submitted

            values$starttime <- now()
            trial <- as.numeric(values$trialsleft > 0)

            plotpath <- ifelse(values$trialsleft > 0, "trials", "plots")

            con <- dbConnect(SQLite(), dbname = "exp_data.db")

            # I suspect this logic could be improved with dbplyr...
            if (trial == 0 && is.null(values$pics)) {
                # Create order of trials
                orderby <- paste0("ORDER BY CASE pic_id ",
                                  paste("WHEN", pic_ids, "THEN", 0:(values$lpp - 1), collapse = " "),
                                  " END")
                # Get picture details
                values$pics <- dbGetQuery(
                    con, paste0("SELECT * FROM picture_details WHERE experiment = '", values$experiment,
                                "' AND trial = ", trial, " AND pic_id IN (", paste(pic_ids, collapse = ","), ") ",
                                orderby))

                nextplot <- values$pics[1,]
            } else if (trial == 0 && !is.null(values$pics)) {
                nextplot <- values$pics[values$lpp - values$lppleft + 1,]
            } else if (trial == 1 && is.null(values$trial_pics)) {
                # Get trial pictures
                orderby <- paste0("ORDER BY CASE pic_id ",
                                  paste("WHEN", trial_pic_ids, "THEN", 0:(length(trial_pic_ids) - 1), collapse = " "),
                                  " END")
                values$trial_pics <- dbGetQuery(
                    con, paste0("SELECT * FROM picture_details WHERE experiment = '", values$experiment,
                                "' AND trial = ", trial, " AND pic_id IN (", paste(trial_pic_ids, collapse = ","), ") ",
                                orderby))
                nextplot <- values$trial_pics[1,]
            } else if (trial == 1 && !is.null(values$trial_pics)) {
                nextplot <- values$trial_pics[values$trialsreq - values$trialsleft + 1,]
            }

            dbDisconnect(con)

            # Update reactive values
            values$pic_id <- nextplot$pic_id
            values$correct <- strsplit(as.character(nextplot$obs_plot_location), ",")[[1]]

            # Reset UI selections
            values$submitted <- FALSE

            updateSelectizeInput(
                session, "certain",
                choices = c("", "Very Uncertain", "Uncertain", "Neutral", "Certain", "Very Certain"),
                selected = NULL)
            updateTextInput(session, "response_no", value = "")
            updateTextInput(session, "other", value = "")
            updateCheckboxGroupInput(session, "reasoning", selected = NA)

            if (is.null(nextplot$pic_name)) return(NULL)

            # Read svg and remove width/height
            tmp <- readLines(file.path(plotpath, "svg", basename(nextplot$pic_name)))
            tmp[2] <- str_replace(tmp[2], "width=.*? height=.*? viewBox", "viewBox")

            # Include the picture
            div(
                class="full-lineup-container",
                HTML(tmp)
            )

            # div(
            #     class = "full-lineup-container",
            #     img(src = file.path(plotpath, "svg", basename(nextplot$pic_name)),
            #         style = "max-width:100%; max-height = 100%")
            # )

            }) # end WithProgress
    }) # end renderUI
}) # End app definition
