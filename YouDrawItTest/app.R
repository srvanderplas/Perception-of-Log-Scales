library(shiny)
library(shinyjs)
library(r2d3)
library(tidyverse)
library(gridSVG)
library(lubridate)

# Data Sets for Estimation Task
source(here::here("YouDrawItTest", "format_estimation_data.R"))

inputUserid <- function(inputId, value='') {
  #   print(paste(inputId, "=", value))
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
  )
}

inputIp <- function(inputId, value=''){
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
  )
}


# Redefine drawr function
drawr <- function(data, linear = "true", draw_start = mean(data$x),
                  title = "", x_range = NULL, y_range = NULL,
                  x_lab = "", y_lab = "", drawn_line_color = "orangered",
                  data_line_color = "steelblue", x_axis_buffer = 0.01, y_axis_buffer = 0.05,
                  # show_finished = F,
                  shiny_message_loc = NULL) {
  plot_data <- dplyr::select(data, x, y)
  x_min <- min(plot_data$x)
  x_max <- max(plot_data$x)
  y_min <- min(plot_data$y)
  y_max <- max(plot_data$y)
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
  
  r2d3::r2d3(plot_data, "main.js",
             dependencies = c("d3-jetpack"),
             options = list(draw_start = draw_start, linear = as.character(linear),
                            pin_start = T, x_range = x_range, y_range = y_range,
                            line_style = NULL,
                            data_line_color = data_line_color, drawn_line_color = drawn_line_color,
                            free_draw = F, shiny_message_loc = shiny_message_loc))
}


# Define UI for application that draws a histogram
ui <- navbarPage(
  "Testing Graphics",
  # ---- Informed Consent ----------------------------------------------------
  tabPanel(
    title = "About This Study",  
    fluidRow(
      column(
        width = 6, offset = 3,
        radioButtons(
          "consent_type", label = "Recruitment Method", 
          choices = c("Reddit - pilot" = 1, "Prolific" = 2, "Reddit - in parallel with Prolific" = 3), 
          width = "100%", inline = T)
      )
    ),
    fluidRow(
      column(
        width = 10, offset = 1,
        h3("Perception and Decision Making Using Statistical Graphs"),
        p("This study is intended to assess how people perceive statistical graphs and charts, and how charts are used to make decisions. If you are 19 years of age or older, and have normal or corrected-to-normal vision, you may participate in this research."),
        h4("What is the reason for doing this research study?"),
        p("Statistical charts and graphs are everywhere – in news articles, advertisements, and on TV. We are interested in whether people read information from charts accurately, and whether certain types of charts are more useful when making data-informed decisions. Unfortunately, we know relatively little about how people read and perceive charts. This study is designed to address this gap in research by systematically investigating the use of charts in different tasks and contexts. In order to participate you must be 19 years of age or older and have normal or corrected-to-normal vision."),
        h4("What will be done during this research study?"),
        p("Participation in this study should require less than 30 minutes of your time. You will be asked to look at statistical charts and then answer questions or complete tasks based on the visualization and contextual information."),
        p("You may be asked to estimate, predict, or make decisions based on one or more graphs. You will be able to provide an explanation of your response, if you choose to do so. "),
        p("We expect that each of these tasks will take less than 3 minutes to complete. We will start out with practice questions so that you can become accustomed to the interface. After the practice task(s), there will be a series of questions. At the end of the study, you will be asked for some demographic information, such as your age, education level, and occupation."),
        p("Participation will take place in a location of your choosing, on your computer."),
        h4("What are the possible risks of being in this research study?"),
        p("There are no known risks to you from being in this research study."),
        h4("What are the possible benefits to you?"),
        p("You are not expected to get any benefit from being in this study."),
        h4("Will you be compensated for being in this research study?"),
        conditionalPanel('input.consent_type == 2', p("We will pay you $XX for participating in this study through Prolific. At the conclusion of this study, you will be provided a URL that will direct you back to Prolific, signaling study completion. While you are free to quit at any time or to decline to answer any question, you will only be compensated if you complete the study and click on the redirect URL provided at the end of the study."),
                         p("In order to document your receipt of the payment, you must provide your name and address through Prolific. Payment records will be stored for up to 7 years and may be stored with Financial Personnel at the University. ")),
        conditionalPanel('input.consent_type == 1', "You will not be paid to take part in this study. "),
        conditionalPanel('input.consent_type == 3', "You will not be paid to take part in this study if you participate through Reddit. In recognition of the fact that not all individuals who want to contribute to science are comfortable providing identifying information to an outside service, we have designed this study with two different participation options. If you wish to be compensated for your participation, you may register with Prolific and locate the study on that platform."),
        h4("How will information about you be protected?"),
        p("Reasonable steps will be taken to protect the privacy and the confidentiality of your study data; however, in some circumstances we cannot guarantee absolute privacy and/or confidentiality. This study will never link personally identifying information with your responses. "),
        conditionalPanel('input.consent_type == 2', "If you are participating through an outside service, such as Prolific, your participation will be recorded and linked to your Prolific account so that you can receive payment. At no point will any identifiable information be linked with your data: any responses you provide are completely anonymous. "),
        p("The research records will be securely stored electronically through University approved methods and will only be seen by the research team and/or those authorized to view, access, or use the records during the study."),
        p("Those who will have access to your research records are the study personnel, the Institutional Review Board (IRB), and any other person, agency, or sponsor as required by law or contract or institutional responsibility. The information from this study may be published in scientific journals or presented at scientific meetings. The individual data records, plus summaries and group-level analyses, will be published, but your identity will be kept strictly confidential."),
        h4("What are your rights as a research participant?"),
        p("You may ask any questions concerning this research and have those questions answered before agreeing to participate in or during the study."),
        p("For study related questions, please contact Dr. Susan Vanderplas (susan.vanderplas@unl.edu)"),
        p("For questions concerning your rights or complaints about the research contact the Institutional Review Board (IRB):"),
        tags$ul(tags$li("Phone: 1 (402) 472-6965"), tags$li("Email: irb@unl.edu")),
        h4("What will happen if you decide not to be in this research study, or decide to stop participating once you start?"),
        p("You can decide not to be in this research study, or you can stop being in this research study (“withdraw’) at any time before, during, or after the research begins for any reason. Deciding not to be in this research study or deciding to withdraw will not affect your relationship with the investigator or with the University of Nebraska-Lincoln."),
        conditionalPanel('input.consent_type == 2', "You will not lose any benefits to which you are entitled. However, if you withdraw before you receive a redirect link, we cannot compensate you for participation in the study."),
        h4("Documentation of Informed Consent"),
        p("You are voluntarily making a decision whether or not to participate in this research study. By clicking on the I Agree button below, your consent to participate is implied. You should print a copy of this page for your records. ")
      ),
      fluidRow(
        column(width = 4, offset = 2, actionButton("consent", "I agree", class = "btn-success")),
        column(width = 4, tags$button("I do not agree", class = "btn btn-danger", href = "www.google.com"))
      )
    )
  ),
  # ---- Tab 1 ---------------------------------------------------------------
  tabPanel(
    title = "You Draw It",
    # icon = "pencil-alt",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "d3.css")
    ),
    fluidRow(
      column(
        width = 9,
        d3Output("shinydrawr"),
        helpText("What will this line look like for the rest of the x values?
                         Using your mouse, draw on the plot to fill in the values,
                         maintaining the previous trend.")
      ),
      column(
        width = 3,
        p("(Determined by experimental design)"),
        checkboxInput("drawr_linear_axis", "Linear Y Axis?", value = T),
        hr(),
        p("Recorded Data:"),
        tableOutput("drawrmessage")
      )
    )
  ),
  # ---- Tab 2 ---------------------------------------------------------------
  tabPanel(
    title = "Lineup",
    fluidRow(
      column(
        width = 8,
        plotOutput("lineup", height = "600px"),
        helpText("Which panel or panels of this plot are the most different?
                         Enter the panel number(s) on the right side,
                         and explain why you chose that panel."),
        useShinyjs(),  # Set up shinyjs
      ),
      column(
        width = 4,
        h3("Selected Plot"),
        selectInput("lineup_panel", "Most Different Panel(s)", choices = 1:20, multiple = T),
        textAreaInput(
          inputId = "lineup_reason", "Why did you choose the plot?",
          placeholder = "Provide some description of the features you used to make your decision here."),
        actionButton("lineup_submit", "Submit my answer"),
        hr(),
        actionButton("lineup_answer_btn", "What's the answer?", class = "btn-light"),
        hidden(textOutput("lineup_answer")),
        hr(),
        p("(Determined by experimental design)"),
        checkboxInput("lineup_linear_axis", "Linear Y Axis?", value = T)
      )
    )
  ),
  # ---- Tab 3 ---------------------------------------------------------------
  tabPanel(
    title = "Estimation",
    fluidRow(
      column(
        width = 8,
        plotOutput("est_plot", height = "600px"),
        uiOutput("est_helptext")
      ),
      column(
        width = 4,
        uiOutput("est_q1"),
        hr(),
        uiOutput("est_q2"),
        helpText("(e.g. if there are 3x as many, answer 3)"),
        hr(),
        p("(Determined by experimental design)"),
        checkboxInput("est_linear_axis", "Linear Y Axis?", value = T),
        selectInput("est_scenario", "Scenario", choices = 1:5, selected = 1),
        hr(),
        actionButton("est_ans_btn", "What are the answers?", class = "btn-light"),
        hidden(uiOutput("est_answer"))
      )
    )
    # icon = "ruler-combined"
  ),
  # ---- Tab 4 ---------------------------------------------------------------
  tabPanel(
    title = "Other Information",
    fluidRow(
      column(
        width = 4, offset = 2,
        h3("Demographic Information"),
        selectInput("age", label = "Age?",
                    choices = c("12-18", "19-24", "25-34", "35-44", "45-64", "65+"),
                    selected = NULL, multiple = F),
        selectInput("gender", label = "Gender Identity?",
                    choices = c("Male", "Female", "Nonbinary", "Prefer not to say"),
                    multiple = F),
        selectInput("education", label = "Education",
                    choices = c("some K-12", "completed K-12 or equivalent",
                                "some college/university",
                                "completed 2-year degree", "completed 4-year degree",
                                "some graduate/professional school",
                                "completed graduate/professional degree"),
                    multiple = F),
        textInput("occupation", label = "What is your occupation?"),
        
        inputIp("ipid"),
        inputUserid("fingerprint"),
        textOutput("testtext")
      ),
      column(
        width = 4,
        h3("Math Background"),
        selectInput("mathed", label = "What is the highest level math class you've taken?",
                    choices = c("high school math",
                                "college-level math (not calculus based)",
                                "college-level calculus",
                                "college-level math (above calculus)",
                                "graduate-level math class"),
                    multiple = F, width = "100%"),
        selectInput("mathed2", label = "What other math-related classes have you taken?",
                    choices = c("high school statistics",
                                "high school physics",
                                "college-level statistics",
                                "college-level physics",
                                "college-level engineering",
                                "graduate-level statistics",
                                "graduate-level physics",
                                "graduate-level engineering"),
                    multiple = T, width = "100%"),
        selectInput("graph_excel",
                    label = "How often do you create graphs or charts in Excel or another software program?",
                    choices = c("Daily", "Once a week", "Once a month", "A few times a year", "Less than once a year", "Never"),
                    multiple = F, width = "100%"),
        selectInput("log", label = "How often do you use logarithms?",
                    choices = c("I don't know what that means",
                                "I don't use logarithms",
                                "I use logarithms occasionally",
                                "I use logarithms at least once a week"),
                    multiple = F, width = "100%"),
        checkboxInput("stem", "I am in a STEM-related field"),
        checkboxInput("enjoy_math", "I like to use math")
      )
    ),
    fluidRow(
      column(width = 8, offset = 2,
             tags$hr(),
             h3("Other Comments?"),
             textAreaInput(inputId = "other_comments",
                           label = "Please leave any other comments you have about this study here",
                           width = "100%")
      )
    )
  )
  # ---- End UI --------------------------------------------------------------
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # ---- Tab 1 ---------------------------------------------------------------
  data_tab1 <- tibble(x = 1:30, y = exp((x-15)/5))
  
  y_range <- range(data_tab1$y) * c(.7, 1.1)
  
  draw_start <- 20
  
  message_loc <- session$ns("drawr_message")
  drawn_data <- shiny::reactiveVal()
  
  output$shinydrawr <- r2d3::renderD3({
    islinear <- ifelse(input$drawr_linear_axis, "true", "false")
    # Use redef'd drawr function
    drawr(data = data_tab1,
          linear = islinear,
          draw_start = draw_start,
          shiny_message_loc = message_loc,
          x_range = range(data_tab1$x), y_range = y_range,
          drawn_line_color = "skyblue", data_line_color = "blue")
  })
  
  shiny::observeEvent(input$drawr_linear_axis, {
    drawn_data(NULL)
  })
  
  shiny::observeEvent(input$drawr_message, {
    data_tab1 %>%
      dplyr::filter(x >= draw_start) %>%
      dplyr::mutate(drawn = input$drawr_message) %>%
      drawn_data()
  })
  
  output$drawrmessage <- renderTable({
    drawn_data()
  })
  
  # ---- Tab 2 ---------------------------------------------------------------
  
  
  data_tab2 <- reactive({
    input$lineup_submit
    
    gen_exp_data <- function(coef, n = 30) {
      tibble(
        x = seq(-15, 15, length.out = n),
        y = coef * x,
        err = rnorm(n, 1, .25)
      )
    }
    
    tibble(c1 = rnorm(20, 1, .05),
           c2 = c(1.3, rep(1, 19)),
           .id = 1:20,
           pos = sample(.id, 20)) %>%
      pivot_longer(cols = c("c1", "c2"), names_to = "group", values_to = "coef") %>%
      mutate(data = map(coef, gen_exp_data)) %>%
      unnest(data) %>%
      group_by(.id) %>%
      # Scaling is done by plot, not by group within plot
      mutate(yfix = exp((y - mean(y))/sd(y)) + err) %>%
      ungroup()
  })
  
  observe({
    onclick("lineup_answer_btn", {
      toggle("lineup_answer")
    })
  })
  
  answertext <- reactive({
    sprintf("The target is in panel %d", unique(filter(data_tab2(), .id == 1)$pos))
  })
  
  # Hide the answer and clear the inputs if a new lineup is generated
  observeEvent({
    input$lineup_submit
  }, {
    hide("lineup_answer")
    updateTextAreaInput(session, "lineup_reason", value = character(0))
    updateSelectInput(session, "lineup_panel", selected = character(0))
  })
  
  
  
  output$lineup_answer <- renderText({
    # message(sprintf("lineup_answer_btn value is %d", isolate(input$lineup_answer_btn)))
    answertext()
  })
  
  output$lineup <- renderPlot({
    df <- data_tab2()
    lineup_plot <- ggplot(df, aes(x = x, y = yfix, color = group)) +
      geom_point() +
      facet_wrap(~pos, ncol = 5) +
      scale_color_discrete(guide = F) +
      theme(axis.text = element_blank(), axis.title = element_blank())
    
    if (!input$lineup_linear_axis)  lineup_plot <- lineup_plot + scale_y_log10()
    
    lineup_plot
  })
  
  # ---- Tab 3 ---------------------------------------------------------------
  
  est_data <- reactive({
    filter(df, scenario == as.numeric(input$est_scenario))
  })
  
  format_yaxis <- function(x) {
    x1 <- format(x, scientific = F, digits = 1, drop0trailing = T, trim = T)
    x2 <- format(x, digits = 1, scientific = T, drop0trailing = T, trim = T) %>%
      gsub("e\\+00", "", x = .) %>%
      gsub("\\+0", "", x = .) %>%
      gsub("\\-0", "-", x = .) %>%
      gsub("e(-?\\d{1,2})", " %*% 10^{\\1}", x = .) %>%
      gsub("^1 ... ", "", x = .)
    parse(text = ifelse(x >= 0.01 & x < 1000, x1, x2))
  }
  
  output$est_q1 <- renderUI({
    tmp <- filter(qs, scenario == input$est_scenario, qnum == 1)
    numericInput("est_q1", label = tmp$q, min = tmp$lb, max = tmp$ub, step = tmp$delta, value = tmp$lb)
  })
  
  output$est_q2 <- renderUI({
    tmp <- filter(qs, scenario == input$est_scenario, qnum == 2)
    numericInput("est_q2", label = tmp$q, min = tmp$lb, max = tmp$ub, step = tmp$delta, value = tmp$lb)
  })
  
  output$est_plot <- renderPlot({
    tmp <- est_data()$data[[1]] %>%
      filter(dec_date >= 1980) %>%
      mutate(rank = as.character(rank))
    
    est_axis <- est_extra(as.numeric(input$est_scenario), input$est_linear_axis)
    
    p <- ggplot(data = tmp, aes(x = dec_date, y = value)) +
      theme(axis.title.x = element_blank()) +
      theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
    
    if (input$est_scenario < 3){
      p <- p + geom_point()
    } else {
      p <- p + geom_point(aes(color = rank, shape = rank))
    }
    p <- p + isolate(ylab(sprintf("%s (%s)", est_data()$type, est_data()$unit)))
    
    est_axis$major_breaks
    if (input$est_linear_axis) {
      p <- p + scale_y_continuous(breaks = est_axis$major_breaks, minor_breaks = est_axis$minor_breaks, labels = format_yaxis)
    } else {
      p <- p + scale_y_log10(breaks = est_axis$major_breaks, minor_breaks = est_axis$minor_breaks, labels = format_yaxis)
    }
    
    print(p)
  })
  
  observe({
    onclick("est_ans_btn", {
      toggle("est_answer")
    })
  })
  
  observe({
    input$est_scenario
    isolate({
      hide("est_answer")
    })
  })
  
  output$est_helptext <- renderUI({
    est_axis <- est_extra(as.numeric(input$est_scenario), input$est_linear_axis)
    list(p(est_axis$explain_base), p(est_axis$explain_ext))
  })
  
  output$est_answer <- renderUI({
    tmp <- filter(qs, scenario == input$est_scenario)
    list(
      p(sprintf("Q%d Computed (Exact) Answer: %.2f", 1, tmp$answer[1])),
      p(sprintf("Q%d Computed (Exact) Answer: %.2f", 2, tmp$answer[2])))
  })
  
  # ---- Tab 4 ---------------------------------------------------------------
  output$testtext <- renderText(paste("Participant Browser fingerprint: ", input$fingerprint))
  
}

# Run the application
shinyApp(ui = ui, server = server)
