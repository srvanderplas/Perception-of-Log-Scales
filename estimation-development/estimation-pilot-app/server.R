# ----------------------------------------------------------------------------------------------------
# Load Libraries  ------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

# Shiny Specific
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyhelper)

# Data Management and Plotting
library(tidyverse)
library(scales)
library(purrr)
library(lubridate)

# Data Importing and Exporting
library(readr)
library(RSQLite)
library(DBI)
sqlite.driver <- dbDriver("SQLite")

# ----------------------------------------------------------------------------------------------------
# Set up Experiment -----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

experiment_name <- "emily-log-estimation-pilot-app"

# implement window dimensions
window_dim_min <- c(600, 600) # width, height

# connect to database
con <- dbConnect(sqlite.driver, dbname = "estimation_data.db")

experiment <- dbReadTable(con, "experiment_details")
if (nrow(experiment) > 1) {
    experiment <- experiment[nrow(experiment),]
    warning("Multiple rows in the experiment_details table. Only the last row will be used.")
}

simulated_data  <- dbReadTable(con,"simulated_data")
estimation_questions <- dbReadTable(con, "estimation_questions")
scenario_text_data <- dbReadTable(con, "scenario_text_data")

dbDisconnect(con)


# ----------------------------------------------------------------------------------------------------
# Shiny Server ---------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

shinyServer(function(input, output, session) {

  # Randomization --------------------------------------------------------------
  # This needs to be run every connection, not just once.
  
  rand <- tibble(
    dataset = sample(c("dataset1", "dataset2"), 2, replace = F),
    creature = sample(unique(estimation_questions$qtext[estimation_questions$q_id == "scenario"]), 2, replace = F),
    scale    = sample(c("linear", "log2"), 2, replace = F)
  ) 
  
  randomization1 <- rand[1,] %>%
    expand_grid(q_id = c("scenario", "Q0", sample(c("QE1", "QE2", "QI1", "QI2", "QI3"), 5)))
  
  randomization2 <- rand[2,] %>%
    expand_grid(q_id = c("scenario", "Q0", sample(c("QE1", "QE2", "QI1", "QI2", "QI3"), 5)))
  
  randomization <- rbind(randomization1, randomization2) %>%
    left_join(estimation_questions, by = c("creature", "q_id"))
  
  # Startrek (Tribbles)
  # stardate = 4523.3 (universe year = Fri Jan 11 2267 21:58:18)
  # https://trekguide.com/Stardates.htm
  # 1 week span: Jan 11, 2267 (4516.69) to Jan 18, 2267 (4567.19) = 50.5 stardates
  # 4516.69 - 3000 (+ 1516.69)
  # Let's round to 4500 (+ 1500)
  
  # Star Wars (Ewoks)
  # Galactic Civil War 0 - 5 ABY....
  # https://starwars.fandom.com/wiki/Timeline_of_galactic_history
  # Start at 0 ABY (After the Battle of Yavin) (- 3000)
  
  simulated_data <- rand %>%
    right_join(simulated_data, by = "dataset") %>%
    mutate(date = ifelse(creature == "tribble", x + 1500, x - 3000))
  
    # This needs to be run every connection, not just once.
    study_starttime = now()

    # reactive values to control the trials
    values <- reactiveValues(
        experiment = experiment$experiment,
        question   = "",
        scenario   = as.character(scenario_text_data[scenario_text_data$creature == as.character(randomization[1, "creature"]), "text"]),
        qreq       = experiment$num_qs,
        qleft      = experiment$num_qs,
        qcounter   = 1,
        trialsreq  = experiment$trialsreq,
        trialsleft = NA,

        submitted = FALSE,
        starttime = NULL,
        
        scale         = as.character(randomization[1, "scale"]),
        q_id          = as.character(randomization[1, "q_id"]),
        creature_name = as.character(randomization[1, "creature"]),
        dataset_id    = as.character(randomization[1, "dataset"])
        )

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
    
    
    # ---- Demographic information ---------------------------------------------
    output$demo_text <- renderText({
        return("Please fill out the demographic information to begin.")
    })

    # add demographic information to the database
    observeEvent(input$submitdemo, {
      
        if (!is.null(input$nickname) && nchar(input$nickname) > 0 && !any(input$dimension < window_dim_min)) {
          
            con <- dbConnect(sqlite.driver, dbname = "estimation_data.db")
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

            dbDisconnect(con)

            updateCheckboxInput(session, "ready", value = TRUE)
        }
    })

    # --------------------------------------------------------------------------
    # ---- Question Flow -------------------------------------------------------
    # --------------------------------------------------------------------------
    
    # Enable submit button if the experiment progresses to ___ stage
    observe({
      if (is.null(input$response) || input$response == "") {
        enable("submit")
      }
    })

    # Saves responses to feedback database
    observeEvent(input$submit, {
      
        if (values$qleft > 0 &&
            (!is.null(input$question_text) && input$question_text != "" && !is.na(input$question_text)) &&
            values$q_id != "scenario" &&
            !any(input$dimension < window_dim_min)) {

            # Things to do when estimate is given and submitted
            # disable("submit")

                values$result <- "Submitted!"

                response_data <-    tibble(ip_address      = input$ipid,
                                           nick_name       = input$nickname,
                                           study_starttime = study_starttime,
                                           start_time      = values$starttime,
                                           end_time        = now(),
                                           order           = values$qcounter,
                                           q_id            = values$q_id,
                                           creature        = values$creature_name,
                                           dataset         = values$dataset_id,
                                           scale           = values$scale,
                                           response        = input$question_text,
                                           scratchpad      = input$notes
                                           )

                # Write results to database
                con <- dbConnect(sqlite.driver, dbname = "estimation_data.db")
                dbWriteTable(con, "feedback", response_data, append = TRUE, row.names = FALSE)
                dbDisconnect(con)

                # Update variables for next question
                values$qleft    = values$qleft - 1
                values$qcounter = values$qcounter + 1
                
                values$creature_name = as.character(randomization[values$qcounter, "creature"])
                values$dataset_id    = as.character(randomization[values$qcounter, "dataset"])
                values$scale         = as.character(randomization[values$qcounter, "scale"])
                values$q_id          = as.character(randomization[values$qcounter, "q_id"])
                values$scenario      = as.character(scenario_text_data[scenario_text_data == values$creature_name, "text"])
                
            if (values$qcounter > values$qreq) { # Generate completion code
            
                values$scenario <- "All done! Congratulations!"
                #values$scenario <- paste("All done! Congratulations! Please click the URL to complete the study:")
                updateCheckboxInput(session, "done", value = TRUE)
            }

            values$submitted <- TRUE
            
            } else if (values$q_id == "scenario") {
              
              # Update variables for next question
              values$qleft    = values$qleft - 1
              values$qcounter = values$qcounter + 1
              values$result   = NULL
              
              values$creature_name = as.character(randomization[values$qcounter, "creature"])
              values$scale         = as.character(randomization[values$qcounter, "scale"])
              values$q_id          = as.character(randomization[values$qcounter, "q_id"])
              values$scenario      = as.character(scenario_text_data[scenario_text_data == values$creature_name, "text"])
              
              values$submitted <- TRUE
              
            } else {
                # Don't let them move ahead without answering the question
                showNotification("Please provide an answer.")
            }
    })
    
    observeEvent(input$calcEval, {
      calc_data <-    tibble(ip_address      = input$ipid,
                             nick_name       = input$nickname,
                             study_starttime = study_starttime,
                             q_id            = values$q_id,
                             creature        = values$creature_name,
                             dataset         = values$dataset_id,
                             scale           = values$scale,
                             expression      = input$calc,
                             evaluated       = calculationVals())
      
      # Write results to database
      con <- dbConnect(sqlite.driver, dbname = "estimation_data.db")
      dbWriteTable(con, "calc_feedback", calc_data, append = TRUE, row.names = FALSE)
      dbDisconnect(con)
    })
    
    # Output info on which question/page you're on
    output$status <- renderText({
      paste("Question", values$qcounter, "of", values$qreq)
    })
    
    # This renders the scenario text
    output$scenario_text <- renderUI({
      
      if (values$q_id == "scenario" || values$qcounter > values$qreq) {
        HTML(values$scenario)
      }
      
    })

    # This renders the question text
    output$question_textUI <- renderUI({
      input$submit
      values$starttime <- now()
      
      # Reset UI selections
      # values$submitted    <- FALSE
      
      if (values$q_id == "Q0") {
        
        tagList(
          textInput("question_text",
                    randomization[values$qcounter, "qtext"],
                    value = ""),
          # %>%
          # helper(type = "markdown",
          #        icon = "question-circle",
          #        colour = "black",
          #        title = "Plot",
          #        content = c("In words, provide a description of the population.")),
          
          bsTooltip("question_text", "In words, provide a description of the population."),

        )
        
      } else if (values$q_id != "Q0" && values$q_id != "scenario") {
        
        tagList(
          numericInput("question_text",
                       randomization[values$qcounter, "qtext"],
                       value = ""),
          
          bsTooltip(id = "question_text", 
                    title = "Provide a numerical approximation.")
        )
        
      } else if (values$q_id == "scenario") {
      
        helpText(h5(paste("Hit 'Submit' to begin answering questions about the", 
                       str_to_title(as.character(randomization[values$qcounter, "creature"])),
                       "population.", sep = " "))
        )
     }
      
    }) # End Render Question Text
    
    # This renders the plot
    output$data_plot <- renderPlot({
              
            # create base scatterplot
      
            basePlot <- simulated_data %>%
              filter(creature == values$creature_name) %>%
              ggplot(aes(x = date, y = y)) +
              geom_point() +
              theme_bw() +
              theme(aspect.ratio = 1,
                    axis.title = element_text(size = 14),
                    axis.text = element_text(size = 12)
              )
            
            if(values$creature_name == "tribble"){
              
              basePlot <- basePlot +
                scale_x_continuous("Stardate", expand = c(0.01,0.01))
              
            } else if (values$creature_name == "ewok"){
              
              basePlot <- basePlot +
                scale_x_continuous("ABY \n (After the Battle of Yavin)", expand = c(0.01,0.01))
              
            }
            
            if(values$scale == "linear"){
              
            finalPlot <- basePlot + 
              scale_y_continuous(paste(str_to_title(values$creature_name), "Population \n (Linear Scale)"),
                                 limits = c(100, 55000),
                                 breaks = seq(0, 55000, 5000),
                                 labels = comma,
                                 minor_breaks = c())
            
            } else if (values$scale == "log2"){
              
            finalPlot <- basePlot + 
              scale_y_continuous(paste(str_to_title(values$creature_name), "Population \n (Log Scale)"),
                                 trans = "log2",
                                 limits = c(100, 55000),
                                 breaks = 2^seq(0,10000,1),
                                 labels = comma,
                                 minor_breaks = c())
            
            }
            
            finalPlot
        
    }) # End renderPlot
    
    # This outputs either plot or tribble/ewok image
    output$figure <- renderUI({
      
      if (values$q_id != "scenario" && values$qcounter <= values$qreq) {
        
        plotOutput("data_plot", height = "500px")
        
      } else if (values$qcounter > values$qreq) {
        imagepath <- "ewok-tribble.jpg"
        tags$figure(
          div(img(src = imagepath, width="60%"), style="text-align: center"),
          br(),
          tags$figcaption("Artwork modified from @allison_horst", style = "text-align: center; color: lightgray"))
      }
      
      else if (values$q_id == "scenario") {
        
        imagepath <- paste(values$creature_name, ".jpg", sep = "")
        tags$figure(
          div(img(src = imagepath, width="60%"), style="text-align: center"),
          br(),
          tags$figcaption("Artwork modified from @allison_horst", style = "text-align: center; color: lightgray"))
        
      }
      
    })
    
    output$simple_calculator <- renderUI({
      input$submit

      if (values$q_id != "scenario" && values$qcounter <= values$qreq) {
        if(values$q_id != "scenario" && values$q_id != "Q0") {
          
          tagList(
            
            helpText(h5("Below are resources for you to use as you are making numerical approximations.")),
            br(),
            
            column(width = 9,
            textInput("calc",
                      "Basic Calculator (e.g. 2 + 2 = 4)",
                      value = "")),
            
            column(width = 3,
            actionButton("calcEval", "Evaluate")),
            
          )
        }
      }
    })
    
    calculationVals <- eventReactive(input$calcEval, {
      
      if(!is.null(input$calc) && input$calc != "" & is.numeric(eval(parse(text=input$calc)))) {
        eval(parse(text=input$calc)) %>% as.character()
      } else {
        "Please enter in a valid expression."
      }
      
    })
    
    output$calculation <- renderText({
      input$submit

      if (values$q_id != "scenario" && values$qcounter <= values$qreq) {
        calculationVals()
      }
    })
  
    output$notepad <- renderUI({
      input$submit
      
      if (values$q_id != "scenario" && values$qcounter <= values$qreq) {
        
        if(values$q_id != "scenario" && values$q_id != "Q0") {
          tagList(
            textAreaInput("notes", "Scratchpad", "Put scratch-work here...", width = "500px", height = "250px"),
            # actionButton("examplePopup", "Show Examples", onclick = "window.open('examples-popup.png')")
            actionButton("examplePopup", "Show Examples")
          )
        }
      }
    })
    
    observeEvent(input$examplePopup, {
      showModal(modalDialog(
        includeHTML("www/test.html"),
        easyClose = TRUE,
        size = "l"
      )
      )
    })
    
}) # End app definition
