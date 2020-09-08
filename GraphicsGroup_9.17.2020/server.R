library(shiny)
library(shinyjs)
library(ggplot2)
library(lubridate)
library(dplyr)
library(RSQLite)

shinyServer(function(input, output, session) {
    
    values <- reactiveValues(question = "", experiment = "", pics = NULL, submitted = FALSE, choice = NULL, reasons = NULL, starttime = NULL, trialsreq = 0, trialsleft = 0, lpp = 0, lppleft = 0, pic_id = 0, choice = NULL, correct = NULL, result = "")
    
    experiment_choices <- reactive({
        con <- dbConnect(SQLite(), dbname = "data/turk.db")
        
        experiments <- dbReadTable(con, "experiment_details")
        
        dbDisconnect(con)
        
        return(sort(experiments$experiment, decreasing = TRUE))
    })
    
    output$downloadDB <- downloadHandler(
        filename = function() { paste0(values$experiment, "_results.csv") },
        content = function(conn) {
            con <- dbConnect(SQLite(), dbname = "data/turk.db")
            feedback <- dbReadTable(con, "feedback")
            
            this_feedback <- feedback %>%
                filter(description == values$experiment)
            
            write.csv(this_feedback, conn, row.names = FALSE)
        }
    )
    
    observe({
        updateSelectizeInput(session, "expname", choices = experiment_choices())
    })
    
    output$debug <- renderText({return(values$question)})
    
    observeEvent(input$confirmexp, {
        if (nchar(input$expname) > 0) {
            con <- dbConnect(SQLite(), dbname = "data/turk.db")
            
            experiments <- dbReadTable(con, "experiment_details")
            myexp <- experiments[experiments$experiment == input$expname,]
            
            values$experiment <- myexp[1,"experiment"]
            values$question <- myexp[1,"question"]
            values$reasons <- strsplit(myexp[1,"reasons"], ",")[[1]]
            values$lpp <- myexp[1,"lpp"]
            values$lppleft <- myexp[1,"lpp"]
            values$trialsreq <- myexp[1,"trials_req"]
            values$trialsleft <- myexp[1,"trials_req"]
            
            dbDisconnect(con)
            
            #enable("downloadDB")
            
            updateCheckboxInput(session, "expchosen", value = TRUE)
            source(file.path("experiments", input$expname, "randomization.R"))
        }
    })

    observe({
        if (length(values$reasons) == 1) {
            updateCheckboxInput(session, "otheronly", value = TRUE)
            updateTextInput(session, "other", label = "Reason")
        }
    })
    
    observe({
        updateCheckboxGroupInput(session, "reasoning", choices = values$reasons, selected = NA)
    })
    
    observeEvent(input$beginexp, {
        if (input$consent) updateCheckboxInput(session, "welcome", value = TRUE)
    })
    
    observeEvent(input$submitdemo, {
        if (!is.null(input$turk) && nchar(input$turk) > 0) {
            con <- dbConnect(SQLite(), dbname = "data/turk.db")
            
            age <- ifelse(is.null(input$age), "", input$age)
            gender <- ifelse(is.null(input$gender), "", input$gender)
            academic_study <- ifelse(is.null(input$education), "", input$education)

            demoinfo <- data.frame(nick_name = input$turk, 
                                   age = age,
                                   gender = gender,
                                   academic_study = academic_study,
                                   ip_address = "")
            
            dbWriteTable(con, "users", demoinfo, append = TRUE, row.names = FALSE)
            dbDisconnect(con)
            
            updateCheckboxInput(session, "ready", value = TRUE)
        }
    })
    
    output$welcome_header <- renderText({
        return("Welcome to a Survey on Graphical Inference")
    })
    
    output$welcome_text <- renderUI({
        return(HTML("This web site is designed to conduct a survey on graphical inference which will help us understand human perception of graphics for use in communicating statistics.<br/><br/>
               
               The following examples illustrate the types of questions you may encounter during this experiment."))
    })
    
    output$example1_q <- renderText({
        return(paste0("Example 1: ", values$question))
    })
    
    output$example1_plot <- renderImage({
        if (is.null(values$experiment)) return(NULL)
        
        list(src = file.path("experiments", values$experiment, "examples", "example1.png"),
             contentType = 'image/png')
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
        
        list(src = file.path("experiments", values$experiment, "examples", "example2.png"),
             contentType = 'image/png')
    }, deleteFile = FALSE)
    
    output$example2_a <- renderUI({
        return(HTML(paste0("
                    Your choice: <b>Plot 1</b><br/>
                    Reasoning: <b>", values$reasons[2], "</b><br/>
                    How certain are you: <b>Certain</b><br/>
                    ")))
    })
    
    output$demo_text <- renderText({
        return("Please fill out the demographic information to begin.")
    })
    
    output$question <- renderText({
        return(values$question)
    })
    
    output$status <- renderText({
        return(paste(ifelse(values$trialsleft > 0, "Trial", ""), "Plot", ifelse(values$trialsleft > 0, paste(values$trialsreq - values$trialsleft + 1, "of", values$trialsreq), paste(values$lpp - values$lppleft + 1, "of", values$lpp))))
    })
    
    observe({
        if (input$response_no == "") {
            enable("submit")
        }
    })
    
    observeEvent(input$submit, {
        response <- as.character(input$response_no)
        if (nchar(response) > 0 && all(strsplit(response, ",")[[1]] %in% 1:20) && 
            values$lppleft > 0 && (length(input$reasoning) > 0 || (nchar(input$other) > 0)) && nchar(input$certain) > 0) {
            
            disable("submit")
            
            reason <- input$reasoning
            if ("Other" %in% reason || input$otheronly) {
                reason <- c(reason, input$other)  
            }
            reason <- paste(reason, collapse = ", ")
            
            values$choice <- response

            if (values$trialsleft == 0 && values$lppleft > 0) {
                values$result <- "Submitted!"
                
                test <- data.frame(ip_address = "", nick_name = input$turk, start_time = values$starttime, end_time = now(), 
                                   pic_id = values$pic_id, response_no = values$choice, conf_level = input$certain, 
                                   choice_reason = reason, description = values$experiment)
                
                con <- dbConnect(SQLite(), dbname = "data/turk.db")
                
                dbWriteTable(con, "feedback", test, append = TRUE, row.names = FALSE)
                dbDisconnect(con)
                
                values$lppleft <- values$lppleft - 1
                values$choice <- ""
                
                if (values$lppleft == 0) {
                    rand1 <- sample(letters, 3, replace = TRUE)
                    rand2 <- sample(LETTERS, 3, replace = TRUE)
                    rand3 <- sample(1:9, 3, replace = TRUE)
                    
                    code <- paste(sample(c(rand1, rand2, rand3)), collapse = "")
                    
                    values$question <- paste("All done! Congratulations! Your code is", code)
                    #values$question <- paste("All done! Congratulations! Please click the URL to complete the study:")
                    updateCheckboxInput(session, "done", value = TRUE)
                }
            } else {
                if (any(strsplit(values$choice, ",")[[1]] %in% values$correct)) {
                    values$trialsleft <- values$trialsleft - 1
                    values$result <- "Correct! :)"
                } else {
                    values$result <- "Incorrect :("
                }
            }            
            
            values$submitted <- TRUE
        } else {
            showNotification("Fill in all of the boxes.")
        }
    })
        
    output$lineup <- renderUI({
        if (values$lppleft == 0 || !input$ready) return(NULL)
        
        withProgress(message = paste(values$result, "Loading", ifelse(values$trialsleft > 0, "trial", ""), "plot", ifelse(values$trialsleft > 0, paste(values$trialsreq - values$trialsleft + 1, "of", values$trialsreq), paste(values$lpp - values$lppleft + 1, "of", values$lpp))), expr = {            
            values$submitted
            
            values$starttime <- now()
            trial <- as.numeric(values$trialsleft > 0)
            
            plotpath <- ifelse(values$trialsleft > 0, "trials", "plots")
            
            con <- dbConnect(SQLite(), dbname = "data/turk.db")
            
            if (trial == 0 && is.null(values$pics)) {
                orderby <- paste0("ORDER BY CASE pic_id ", paste("WHEN", pic_ids, "THEN", 0:(values$lpp - 1), collapse = " "), " END")
                values$pics <- dbGetQuery(con, paste0("SELECT * FROM picture_details WHERE experiment = '", values$experiment, "' AND trial = ", trial, " AND pic_id IN (", paste(pic_ids, collapse = ","), ") ", orderby))
                nextplot <- values$pics[1,]
            } else if (trial == 0 && !is.null(values$pics)) {
                nextplot <- values$pics[values$lpp - values$lppleft + 1,]
            } else if (trial == 1 && is.null(values$trial_pics)) {
                orderby <- paste0("ORDER BY CASE pic_id ", paste("WHEN", trial_pic_ids, "THEN", 0:(length(trial_pic_ids) - 1), collapse = " "), " END")
                values$trial_pics <- dbGetQuery(con, paste0("SELECT * FROM picture_details WHERE experiment = '", values$experiment, "' AND trial = ", trial, " AND pic_id IN (", paste(trial_pic_ids, collapse = ","), ") ", orderby))
                nextplot <- values$trial_pics[1,]
            } else if (trial == 1 && !is.null(values$trial_pics)) {
                nextplot <- values$trial_pics[values$trialsreq - values$trialsleft + 1,]
            }
            
            dbDisconnect(con)
            
            values$pic_id <- nextplot$pic_id
            values$correct <- strsplit(nextplot$obs_plot_location, ",")[[1]]
            
            values$submitted <- FALSE
            
            updateSelectizeInput(session, "certain", choices = c("", "Very Uncertain", "Uncertain", "Neutral", "Certain", "Very Certain"), selected = NULL)
            updateTextInput(session, "response_no", value = "")
            updateTextInput(session, "other", value = "")
            updateCheckboxGroupInput(session, "reasoning", selected = NA)
            
            HTML(readLines(file.path("experiments", values$experiment, plotpath, nextplot$pic_name)))
        })
    })
})
