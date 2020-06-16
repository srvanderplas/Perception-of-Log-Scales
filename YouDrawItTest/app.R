library(shiny)
library(shinyjs)
# library(shinysense)
library(r2d3)
library(tidyverse)
library(gridSVG)
# library(plotly)

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
    useShinyjs(),  # Set up shinyjs
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
                         and explain why you chose that panel.")
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
                textOutput("est_helptext")
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
                hidden(textOutput("est_answer"))
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
                h3("Math"),
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

    est_axis <- reactiveValues()
    observe({
        # Calculate breaks and labels based on axis and scenario
        if (input$est_scenario == 1) {
            est_axis$explain_base <- "This plot shows the decrease in the price of computer memory between 1980 and 2020,
                     by showing the memory (in MB) which could be purchased for $1."
            if (input$est_linear_axis) {
                est_axis$major_breaks <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500)
                est_axis$minor_breaks <- seq(0, 500, by = 25)
                est_axis$labels <- as.character(est_axis$major_breaks)
                est_axis$explain_ext <- "The y-axis is shown in 50-MB increments, with lighter gridlines at 25-MB increments."
            } else {
                est_axis$major_breaks <-  c(1, 5) %*% t(10^(-4:2)) %>% sort()
                labels <- as.character(est_axis$major_breaks)
                labels[1:8] <- c(expression(frac(1, 10000)),
                                 expression(frac(5, 10000)),
                                 expression(frac(1, 1000)),
                                 expression(frac(5, 1000)),
                                 expression(frac(1, 100)),
                                 expression(frac(5, 100)),
                                 expression(frac(1, 10)),
                                 expression(frac(5, 10)))
                est_axis$minor_breaks <- c(2.5, 5, 7.5) %*% t(10^(-4:2)) %>% sort()
                est_axis$labels <- labels
                est_axis$explain_ext <- paste(c(
                "The y-axis is shown on log (base 10) scale,",
                "with primary gridlines at 1 and 5 for each order of magnitude,",
                "and lighter gridlines at 2.5 and 7.5.",
                "So between 1 and 10, there are labeled gridlines at 1, 5, and 10,",
                "and lighter gridlines at 2.5 and 7.5."), collapse = " ")
            }
        }
        if (input$est_scenario == 2) {
            est_axis$explain_base <- "This plot shows the decrease in the price of hard drive storage space between 1980 and 2020,
                     by showing the space (in MB) which could be purchased for $1."
            if (input$est_linear_axis) {
                est_axis$major_breaks <- seq(0, 70000, by = 10000)
                est_axis$minor_breaks <- seq(0, 70000, by = 2000)
                est_axis$labels <- as.character(est_axis$major_breaks)
                est_axis$explain_ext <- "The y-axis is shown in 10,000-MB increments, with lighter gridlines at 2,000-MB increments."
            } else {
                est_axis$major_breaks <-  c(1, 5) %*% t(10^(-4:5)) %>% sort()
                labels <- format(isolate(est_axis$major_breaks), nsmall = 3, scipen = F) %>%
                    gsub("e\\+00", "", x = .) %>%
                    gsub("\\+0", "", x = .) %>%
                    gsub("\\-0", "-", x = .) %>%
                    gsub("e(-?\\d{1,2})", " %*% 10^{\\1}", x = .) %>%
                    gsub("^1 ... ", "", x = .)
                idx <- isolate(est_axis$major_breaks) %in% c(.1, .5, 1, 5, 10, 50)
                labels[idx] <- as.character(isolate(est_axis$major_breaks)[idx])
                est_axis$minor_breaks <- c(2.5, 5, 7.5) %*% t(10^(-4:5)) %>% sort()
                est_axis$labels <- map(labels, ~parse(text = .))
                est_axis$explain_ext <- paste(c(
                    "The y-axis is shown on log (base 10) scale,",
                    "with primary gridlines at 1 and 5 for each order of magnitude,",
                    "and lighter gridlines at 2.5 and 7.5.",
                    "So between 1 and 10, there are labeled gridlines at 1, 5, and 10,",
                    "and lighter gridlines at 2.5 and 7.5."), collapse = " ")
            }
        }
        if (input$est_scenario == 3) {
            est_axis$explain_base <- "This plot shows the number of computational cores in each of the top 3 supercomputers in the world, between 1995 and 2020."
            if (input$est_linear_axis) {
                est_axis$major_breaks <- seq(0, 1e7, by = 1e6)
                est_axis$minor_breaks <- seq(0, 1e7, by = 5e5)
                est_axis$labels <- as.character(est_axis$major_breaks)
                est_axis$explain_ext <- "The y-axis is shown in 100,000 core increments, with lighter gridlines at 50,000 core increments."
            } else {
                est_axis$major_breaks <-  c(1, 5) %*% t(10^(1:7)) %>% sort()
                labels <- format(isolate(est_axis$major_breaks), nsmall = 3, scipen = F) %>%
                    gsub("e\\+00", "", x = .) %>%
                    gsub("\\+0", "", x = .) %>%
                    gsub("\\-0", "-", x = .) %>%
                    gsub("e(-?\\d{1,2})", " %*% 10^{\\1}", x = .) %>%
                    gsub("^1 ... ", "", x = .)
                idx <- isolate(est_axis$major_breaks) %in% c(.1, .5, 1, 5, 10, 50)
                labels[idx] <- as.character(isolate(est_axis$major_breaks)[idx])
                est_axis$minor_breaks <- c(2.5, 7.5) %*% t(10^(1:7)) %>% sort()
                est_axis$labels <- map(labels, ~parse(text = .))
                est_axis$explain_ext <- paste(c(
                    "The y-axis is shown on log (base 10) scale,",
                    "with primary gridlines at 1 and 5 for each order of magnitude,",
                    "and lighter gridlines at 2.5 and 7.5.",
                    "So between 100 and 1000, there are labeled gridlines at 100, 500, and 1000,",
                    "and lighter gridlines at 250 and 750."), collapse = " ")
            }
        }
        if (input$est_scenario == 4) {
            est_axis$explain_base <- "This plot shows the maximum sustained computing power (measured in TFlops/s) in each of the top 3 supercomputers in the world between 1995 and 2020."
            if (input$est_linear_axis) {
                est_axis$major_breaks <- seq(0, 5e5, by = 5e4)
                est_axis$minor_breaks <- seq(0, 5e5, by = 1e4)
                est_axis$labels <- as.character(est_axis$major_breaks)
                est_axis$explain_ext <- "The y-axis is shown in increments of 50,000 TFlops/s, with lighter gridlines at increments of 10,000 TFlops/s."
            } else {
                est_axis$major_breaks <-  c(1, 5) %*% t(10^(1:5)) %>% sort()
                labels <- format(isolate(est_axis$major_breaks), nsmall = 3, scipen = F) %>%
                    gsub("e\\+00", "", x = .) %>%
                    gsub("\\+0", "", x = .) %>%
                    gsub("\\-0", "-", x = .) %>%
                    gsub("e(-?\\d{1,2})", " %*% 10^{\\1}", x = .) %>%
                    gsub("^1 ... ", "", x = .)
                idx <- isolate(est_axis$major_breaks) %in% c(.1, .5, 1, 5, 10, 50)
                labels[idx] <- as.character(isolate(est_axis$major_breaks)[idx])
                est_axis$minor_breaks <- c(2.5, 7.5) %*% t(10^(1:7)) %>% sort()
                est_axis$labels <- map(labels, ~parse(text = .))
                est_axis$explain_ext <- paste(c(
                    "The y-axis is shown on log (base 10) scale,",
                    "with primary gridlines at 1 and 5 for each order of magnitude,",
                    "and lighter gridlines at 2.5 and 7.5.",
                    "So between 100 and 1000, there are labeled gridlines at 100, 500, and 1000,",
                    "and lighter gridlines at 250 and 750."), collapse = " ")
            }
        }
        if (input$est_scenario == 5) {
            est_axis$explain_base <- "This plot shows the total peak computing power (measured in TFlops/s) in each of the top 3 supercomputers in the world between 1995 and 2020."
            if (input$est_linear_axis) {
                est_axis$major_breaks <- seq(0, 5e5, by = 5e4)
                est_axis$minor_breaks <- seq(0, 5e5, by = 1e4)
                est_axis$labels <- as.character(est_axis$major_breaks)
                est_axis$explain_ext <- "The y-axis is shown in increments of 50,000 TFlops/s, with lighter gridlines at increments of 10,000 TFlops/s."
            } else {
                est_axis$major_breaks <-  c(1, 5) %*% t(10^(1:5)) %>% sort()
                labels <- format(isolate(est_axis$major_breaks), nsmall = 3, scipen = F) %>%
                    gsub("e\\+00", "", x = .) %>%
                    gsub("\\+0", "", x = .) %>%
                    gsub("\\-0", "-", x = .) %>%
                    gsub("e(-?\\d{1,2})", " %*% 10^{\\1}", x = .) %>%
                    gsub("^1 ... ", "", x = .)
                idx <- isolate(est_axis$major_breaks) %in% c(.1, .5, 1, 5, 10, 50)
                labels[idx] <- as.character(isolate(est_axis$major_breaks)[idx])
                est_axis$minor_breaks <- c(2.5, 7.5) %*% t(10^(1:7)) %>% sort()
                est_axis$labels <- map(labels, ~parse(text = .))
                est_axis$explain_ext <- paste(c(
                    "The y-axis is shown on log (base 10) scale,",
                    "with primary gridlines at 1 and 5 for each order of magnitude,",
                    "and lighter gridlines at 2.5 and 7.5.",
                    "So between 100 and 1000, there are labeled gridlines at 100, 500, and 1000,",
                    "and lighter gridlines at 250 and 750."), collapse = " ")
            }
        }
    })

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

        p <- ggplot(data = tmp, aes(x = dec_date, y = value)) +
            theme(axis.title.x = element_blank()) +
            theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

        if (input$est_scenario < 3){
            p <- p + geom_point()
        } else {
            p <- p + geom_point(aes(color = rank, shape = rank))
        }
        p <- p + isolate(ylab(sprintf("%s (%s)", est_data()$type, est_data()$unit)))

        if (input$est_linear_axis) {
            p <- p + scale_y_continuous(breaks = est_axis$major_breaks, minor_breaks = est_axis$minor_breaks, labels = est_axis$labels)
        } else {
            p <- p + scale_y_log10(breaks = est_axis$major_breaks, minor_breaks = est_axis$minor_breaks, labels = est_axis$labels)
        }

        print(p)
    })

    observe({
        onclick("est_answer_btn", {
            toggle("est_answer")
        })
    })

    output$est_helptext <- renderText({
        paste(est_axis$explain_base, est_axis$explain_ext, sep = "\n\n")
    })

    output$est_answer <- renderText({
        tmp <- filter(qs, scenario == input$est_scenario)
        sprintf("Q%d Computed (Exact) Answer: %.2f\nQ%d Computed (Exact) Answer: %.2f", 1, tmp$answer[1], 2, tmp$answer[2])
    })

    # ---- Tab 4 ---------------------------------------------------------------
    output$testtext <- renderText(paste("Participant Browser fingerprint: ", input$fingerprint))

}

# Run the application
shinyApp(ui = ui, server = server)
